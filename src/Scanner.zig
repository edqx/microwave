const std = @import("std");

const Scanner = @This();

pub const Error = error{ UnexpectedEndOfStream, UnexpectedByte };

pub const Range = struct { usize, usize };

pub const Token = struct {
    pub const Kind = enum {
        newline,
        comment,

        identifier,
        access,
        equals,

        value_delimiter,

        string,
        literal_string,

        integer,
        base_integer,
        float,
        positive_inf,
        negative_inf,
        positive_nan,
        negative_nan,

        offset_date_time,
        local_date_time,
        local_date,
        local_time,

        table_or_array_start,
        table_or_array_end,
        inline_table_start,
        inline_table_end,
    };

    kind: Kind,
    contents: []u8,

    file_range: Range = .{ 0, 0 },
};

const NumberParseState = enum {
    sign,
    base,
    integer,
    base_integer,
    fraction,
    exponent_sign,
    exponent,

    identifier,
};

pub const VTable = struct {
    getSeekPos: *const fn (scanner: *Scanner) usize = defaultSeekPos,
};

fn defaultSeekPos(scanner: *Scanner) usize {
    _ = scanner;
    return 0;
}

fn isWhitespace(char: u8) bool {
    return switch (char) {
        ' ', '\t' => true,
        else => false,
    };
}

fn isLinefeed(char: u8) bool {
    return char == '\n';
}

fn isValueDelimiter(char: u8) bool {
    return char == ',';
}

fn isIdentifier(char: u8) bool {
    return switch (char) {
        'A'...'Z', 'a'...'z', '0'...'9', '_', '-' => true,
        else => false,
    };
}

fn isDigit(char: u8) bool {
    return switch (char) {
        '0'...'9' => true,
        else => false,
    };
}

fn isBaseDigit(char: u8) bool {
    return char == '0';
}

fn isBaseChar(char: u8) bool {
    return switch (char) {
        'x', 'o', 'b' => true,
        else => false,
    };
}

fn isSign(char: u8) bool {
    return switch (char) {
        '+', '-' => true,
        else => false,
    };
}

fn isExponentialDelimiter(char: u8) bool {
    return switch (char) {
        'e', 'E' => true,
        else => false,
    };
}

fn isFractionDelimiter(char: u8) bool {
    return switch (char) {
        '.' => true,
        else => false,
    };
}

const date_time_delimiter_len = 1;
fn isDateTimeDelimiter(char: u8) bool {
    return switch (char) {
        ' ', 'T', 't' => true,
        else => false,
    };
}

fn isZulu(char: u8) bool {
    return switch (char) {
        'Z', 'z' => true,
        else => false,
    };
}

fn peekOffset(reader: *std.Io.Reader, offset: usize, len: usize) ![]u8 {
    return (try reader.peek(offset + len))[offset..][0..len];
}

// helper function to check whether a pattern, where digits are represented
// by '0', is matched by a given slice
//
// caller guarantees that pattern.len == slice.len
fn matchesDigitPattern(pattern: []const u8, slice: []const u8) bool {
    return for (pattern, slice) |t, a| {
        if (t == '0') {
            if (!isDigit(a)) break false;
        } else {
            if (a != t) break false;
        }
    } else true;
}

const local_date_template = "0000-00-00";
const local_date_len = local_date_template.len;
fn isLocalDate(date: [local_date_len]u8) bool {
    return matchesDigitPattern(local_date_template, &date);
}

const local_time_template = "00:00:00";
const local_time_len = local_time_template.len;
fn isLocalTime(time: [local_time_len]u8) bool {
    return matchesDigitPattern(local_time_template, &time);
}

const time_offset_template = "00:00";
const time_offset_len = time_offset_template.len;
fn isTimeOffset(time: [time_offset_len]u8) bool {
    return matchesDigitPattern(time_offset_template, &time);
}

fn ignoreEof(err: std.Io.Reader.Error) !void {
    switch (err) {
        error.EndOfStream => {},
        error.ReadFailed => return err,
    }
}

vtable: *const VTable = &.{},
reader: *std.Io.Reader,

pub fn takeToken(scanner: *Scanner) !?Token {
    const start = scanner.getPos();
    var token = scanner.takeTokenImpl() catch |e| switch (e) {
        error.EndOfStream => return null,
        else => return e,
    };
    const end = scanner.getPos();

    token.file_range = .{ start, end };
    return token;
}

fn takeTokenImpl(scanner: *Scanner) !Token {
    const byte = try scanner.reader.peekByte();

    switch (byte) {
        '#' => {
            return .{
                .kind = .comment,
                .contents = try scanner.takeUntilNewline(),
            };
        },
        'A'...'Z', 'a'...'z', '_' => {
            return .{
                .kind = .identifier,
                .contents = try scanner.takeUntilNotIdentifier(),
            };
        },
        '+', '-', '0'...'9' => {
            if (isDigit(byte)) {
                // let's fill the buffer early so that any tossed bytes don't get invalidated
                // by subsequent calls to 'peek'
                scanner.reader.fillMore() catch |e| try ignoreEof(e);
                if (try scanner.maybeTakeAnyDateTime()) |date_time_token| {
                    return date_time_token;
                }
            }

            var state: NumberParseState = .sign;
            var kind: Token.Kind = .integer;

            var take: []u8 = &.{};

            while (true) {
                if (try scanner.isPeekEndValue(take.len)) {
                    break;
                }
                const peek = scanner.reader.peek(take.len + 1) catch |e| switch (e) {
                    error.EndOfStream => break,
                    error.ReadFailed => return e,
                };
                const last = peek[peek.len - 1];
                switch (state) {
                    .sign => {
                        if (isSign(last)) {
                            state = .integer;
                        } else if (isBaseDigit(last)) {
                            state = .base;
                            kind = .base_integer;
                        } else if (isDigit(last)) {
                            state = .integer;
                            continue;
                        } else unreachable;
                    },
                    .base => {
                        if (isBaseChar(last)) {
                            state = .base_integer;
                        } else return error.UnexpectedByte;
                    },
                    .integer => {
                        if (isFractionDelimiter(last)) {
                            state = .fraction;
                            kind = .float;
                        } else if (isExponentialDelimiter(last)) {
                            state = .exponent_sign;
                            kind = .float;
                        } else if (!isDigit(last)) {
                            state = .identifier;
                            kind = .identifier;
                        }
                    },
                    .base_integer => {
                        if (!isDigit(last)) {
                            state = .identifier;
                            kind = .identifier;
                        }
                    },
                    .fraction => {
                        if (isExponentialDelimiter(last)) {
                            state = .exponent_sign;
                        } else if (!isDigit(last)) break;
                    },
                    .exponent_sign => {
                        if (isSign(last)) {
                            state = .exponent;
                        } else if (isDigit(last)) {
                            state = .exponent;
                            continue;
                        } else return error.UnexpectedByte;
                    },
                    .exponent => {
                        if (!isDigit(last)) break;
                    },
                    .identifier => {
                        if (!isIdentifier(last)) break;
                    },
                }
                take = peek;
            }

            if (state == .sign or state == .base or state == .exponent_sign) {
                return error.UnexpectedEndOfStream;
            }

            scanner.reader.toss(take.len);

            return .{
                .kind = kind,
                .contents = take,
            };
        },
        '.' => {
            return .{
                .kind = .access,
                .contents = try scanner.reader.take(1),
            };
        },
        '"' => {
            scanner.reader.toss(1);
            return .{
                .kind = .string,
                .contents = try scanner.takeSingleString(),
            };
        },
        '\'' => {
            scanner.reader.toss(1);
            return .{
                .kind = .literal_string,
                .contents = try scanner.takeSingleLiteralString(),
            };
        },
        '=' => {
            return .{
                .kind = .equals,
                .contents = try scanner.reader.take(1),
            };
        },
        '[' => {
            return .{
                .kind = .table_or_array_start,
                .contents = try scanner.reader.take(1),
            };
        },
        ']' => {
            return .{
                .kind = .table_or_array_end,
                .contents = try scanner.reader.take(1),
            };
        },
        '{' => {
            return .{
                .kind = .inline_table_start,
                .contents = try scanner.reader.take(1),
            };
        },
        '}' => {
            return .{
                .kind = .inline_table_end,
                .contents = try scanner.reader.take(1),
            };
        },
        '\n' => {
            return .{
                .kind = .newline,
                .contents = try scanner.reader.take(1),
            };
        },
        else => {
            scanner.reader.toss(1);
            return try scanner.takeTokenImpl();
        },
    }
}

fn takeSingleString(scanner: *Scanner) ![]u8 {
    var take: []u8 = &.{};
    var escape: bool = false;
    while (true) {
        const peek = try scanner.reader.peek(take.len + 1);
        switch (peek[peek.len - 1]) {
            '"' => {
                if (!escape) break;
            },
            '\\' => {
                escape = !escape;
            },
            else => {
                escape = false;
            },
        }
        take = peek;
    }
    scanner.reader.toss(take.len + 1);
    return take;
}

fn takeSingleLiteralString(scanner: *Scanner) ![]u8 {
    const slice = try scanner.reader.takeDelimiterExclusive('\'');
    scanner.reader.toss(1);
    return slice;
}

fn takeUntilNotIdentifier(scanner: *Scanner) ![]u8 {
    var take: []u8 = &.{};
    while (true) {
        const peek = try scanner.reader.peek(take.len + 1);
        if (!isIdentifier(peek[peek.len - 1])) break;
        take = peek;
    }
    scanner.reader.toss(take.len);
    return take;
}

fn takeUntilNotDigit(scanner: *Scanner) ![]u8 {
    var take: []u8 = &.{};
    while (true) {
        const peek = try scanner.reader.peek(take.len + 1);
        const last = peek[peek.len - 1];
        if (!isDigit(last)) break;
        take = peek;
    }
    scanner.reader.toss(take.len);
    return take;
}

fn takeUntilNewline(scanner: *Scanner) ![]u8 {
    const slice = try scanner.reader.takeDelimiterExclusive('\n');
    if (slice[slice.len - 1] == '\r') {
        return slice[0 .. slice.len - 2];
    }
    return slice[0 .. slice.len - 1];
}

fn maybeTakeAnyDateTime(scanner: *Scanner) !?Token {
    const start_seek = scanner.reader.seek;
    if (try scanner.maybeTakeDateToken()) |date_token| {
        if (scanner.reader.peek(date_time_delimiter_len + local_time_len)) |time_slice| {
            if (isDateTimeDelimiter(time_slice[0]) and isLocalTime(time_slice[1..][0..local_time_len].*)) {
                scanner.reader.toss(1);

                const token_kind: Token.Kind = blk: {
                    _ = try scanner.maybeTakeTimeToken() orelse unreachable;

                    if (scanner.reader.peekByte()) |zulu_byte| {
                        if (isZulu(zulu_byte)) {
                            scanner.reader.toss(1);
                            break :blk .offset_date_time;
                        }
                        if (isSign(zulu_byte)) {
                            if (scanner.reader.peek(1 + time_offset_len)) |offset_slice| {
                                if (isTimeOffset(offset_slice[1..][0..time_offset_len].*)) {
                                    scanner.reader.toss(1 + time_offset_len);
                                    break :blk .offset_date_time;
                                }
                            } else |e| try ignoreEof(e);
                        }
                    } else |e| try ignoreEof(e);

                    break :blk .local_date_time;
                };

                return .{
                    .kind = token_kind,
                    .contents = scanner.reader.buffer[start_seek..][0..scanner.reader.seek],
                };
            }
        } else |e| try ignoreEof(e);
        return date_token;
    } else if (try scanner.maybeTakeTimeToken()) |time_token| {
        return time_token;
    }
    return null;
}

fn maybeTakeDateToken(scanner: *Scanner) !?Token {
    if (scanner.reader.peek(local_date_len)) |local_date_slice| {
        // note: peek returns _exactly_ local_date_len amount of bytes
        if (isLocalDate(local_date_slice[0..local_date_len].*)) {
            scanner.reader.toss(local_date_len);

            return .{
                .kind = .local_date,
                .contents = local_date_slice,
            };
        }
    } else |e| try ignoreEof(e);
    return null;
}

fn maybeTakeTimeToken(scanner: *Scanner) !?Token {
    const start_seek = scanner.reader.seek;
    if (scanner.reader.peek(local_time_len)) |local_time_slice| {
        if (isLocalTime(local_time_slice[0..local_time_len].*)) {
            scanner.reader.toss(local_time_len);

            if (scanner.reader.peek(2)) |milliseconds_slice| {
                if (isFractionDelimiter(milliseconds_slice[0]) and isDigit(milliseconds_slice[1])) {
                    scanner.reader.toss(1); // toss '.'
                    _ = try scanner.takeUntilNotDigit();
                    return .{
                        .kind = .local_time,
                        .contents = scanner.reader.buffer[start_seek..][0..scanner.reader.seek],
                    };
                }
            } else |e| try ignoreEof(e);

            return .{
                .kind = .local_time,
                .contents = local_time_slice,
            };
        }
    } else |e| try ignoreEof(e);
    return null;
}

fn isPeekEndValue(scanner: *Scanner, offset: usize) !bool {
    if (peekOffset(scanner.reader, offset, 2)) |crlf| {
        if (std.mem.eql(u8, crlf, "\r\n")) return true;
    } else |e| try ignoreEof(e);
    if (peekOffset(scanner.reader, offset, 1)) |lf| {
        if (isWhitespace(lf[0]) or isLinefeed(lf[0]) or isValueDelimiter(lf[0])) return true;
    } else |e| try ignoreEof(e);
    return false;
}

pub fn getPos(scanner: *Scanner) usize {
    return scanner.vtable.getSeekPos(scanner) + scanner.reader.seek;
}

pub fn rangeContents(self: *Scanner, range: Token.Range) []const u8 {
    return self.buffer[range.start..range.end];
}

pub fn tokenContents(self: *Scanner, token: Token) []const u8 {
    return self.rangeContents(token.range);
}

const test_buf: []const u8 =
    \\# this is an example task
    \\name="Write a Shopping List"
    \\created_at = 2025-11-30 18:37:50.999999+05:30
    \\priority = 0b0001100
    \\tags=[
    \\  "personal",
    \\  "weekly",
    \\  "barney"
    \\]
    \\
    \\assigned_to="everyone"
    \\priority="medium" # we've done this
    \\status="resolved"
    \\
    \\[[notes]]
    \\attributed_to="jen"
    \\note="""
    \\We're going shopping for the week tomorrow, remember to write down what you need to buy.
    \\"""
    \\[[notes]]
    \\attributed_to="rhea"
    \\note="""
    \\We need to eat healthier!
    \\"""
    \\attachments=["images/food_pyramid.png"]
    \\
    \\[[notes]]
    \\attributed_to="phoebe"
    \\note="""
    \\I'm allergic to seafood. Let's not buy any of that.
    \\"""
;

fn expectToken(token: ?Token, kind: Token.Kind) !void {
    try std.testing.expect(token != null);
    try std.testing.expectEqual(kind, token.?.kind);
}

fn testAnyScanner(scanner: anytype) !void {
    try expectToken(try scanner.takeToken(), .comment);
    try expectToken(try scanner.takeToken(), .newline);

    try expectToken(try scanner.takeToken(), .identifier);
    try expectToken(try scanner.takeToken(), .equals);
    try expectToken(try scanner.takeToken(), .string);
    try expectToken(try scanner.takeToken(), .newline);

    try expectToken(try scanner.takeToken(), .identifier);
    try expectToken(try scanner.takeToken(), .equals);
    try expectToken(try scanner.takeToken(), .offset_date_time);
    try expectToken(try scanner.takeToken(), .newline);

    try expectToken(try scanner.takeToken(), .identifier);
    try expectToken(try scanner.takeToken(), .equals);
    try expectToken(try scanner.takeToken(), .base_integer);
    try expectToken(try scanner.takeToken(), .newline);

    try expectToken(try scanner.takeToken(), .identifier);
    try expectToken(try scanner.takeToken(), .equals);

    try expectToken(try scanner.takeToken(), .table_or_array_start);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .literal_string);
    //     try expectToken(try scanner.next(), .delimeter);
    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .literal_string);
    //     try expectToken(try scanner.next(), .delimeter);
    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .literal_string);
    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .array_end);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .equals);

    //     try expectToken(try scanner.next(), .literal_string);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .equals);

    //     try expectToken(try scanner.next(), .literal_string);

    //     try expectToken(try scanner.next(), .whitespace);
    //     try expectToken(try scanner.next(), .comment);
    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .equals);

    //     try expectToken(try scanner.next(), .literal_string);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .many_table_start);

    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .many_table_end);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .equals);

    //     try expectToken(try scanner.next(), .literal_string);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .equals);

    //     try expectToken(try scanner.next(), .literal_string);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .many_table_start);

    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .many_table_end);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .equals);

    //     try expectToken(try scanner.next(), .literal_string);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .equals);

    //     try expectToken(try scanner.next(), .literal_string);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .equals);

    //     try expectToken(try scanner.next(), .array_start);

    //     try expectToken(try scanner.next(), .literal_string);
    //     try expectToken(try scanner.next(), .array_end);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .many_table_start);

    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .many_table_end);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .equals);

    //     try expectToken(try scanner.next(), .literal_string);

    //     try expectToken(try scanner.next(), .newline);
    //     try expectToken(try scanner.next(), .key);
    //     try expectToken(try scanner.next(), .equals);

    //     try expectToken(try scanner.next(), .literal_string);
}

test Scanner {
    var reader: std.Io.Reader = .fixed(test_buf);

    var scanner: Scanner = .{
        .vtable = &.{},
        .reader = &reader,
    };

    try testAnyScanner(&scanner);
}
