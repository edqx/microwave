const std = @import("std");

const Scanner = @This();

pub const Error = error{ UnexpectedEndOfBuffer, UnexpectedToken };

pub const Token = struct {
    pub const Kind = enum {
        ignored,
        key,
        value,
        access,
        comment,
        array_start,
        array_end,
        table_start,
        table_end,
        many_table_start,
        many_table_end,
        inline_table_start,
        inline_table_end,
    };

    pub const Range = struct {
        start: usize,
        end: usize,

        pub fn offset(self: Range, add: usize) Range {
            return .{
                .start = self.start + add,
                .end = self.end + add,
            };
        }

        pub fn expand(self: Range, other: Range) Range {
            return .{
                .start = @min(self.start, other.start),
                .end = @max(self.end, other.end),
            };
        }

        pub fn token(self: Range, kind: Kind) Token {
            return .{ .kind = kind, .range = self };
        }
    };

    kind: Kind,
    range: Range,
};

pub const State = enum {
    expect_key_or_table,
    expect_key_with_access,
    expect_value,
    expect_table_key,
    expect_table_key_with_access,
    expect_many_table_key,
    expect_many_table_key_with_access,
};

const StringKind = enum {
    single_line,
    multiple_lines,
};

buffer: []const u8,
cursor: usize = 0,

state: State = .expect_key_or_table,

fn predKey(b: u8) bool {
    return switch (b) {
        'A'...'Z', 'a'...'z', '0'...'9', '_', '-' => true,
        else => false,
    };
}

fn predString(b: u8) bool {
    return b == '"' or b == '\'';
}

fn predEq(b: u8) bool {
    return b == '=';
}

fn predEscape(b: u8) bool {
    return b == '\\';
}

fn predAccess(b: u8) bool {
    return b == '.';
}

fn predArrayOpen(b: u8) bool {
    return b == '[';
}

fn predArrayClose(b: u8) bool {
    return b == ']';
}

fn predTableOpen(b: u8) bool {
    return b == '[';
}

fn predTableClose(b: u8) bool {
    return b == ']';
}

fn predInlineTableOpen(b: u8) bool {
    return b == '{';
}

fn predInlineTableClose(b: u8) bool {
    return b == '}';
}

fn predDelimeter(b: u8) bool {
    return b == ',';
}

fn predSpace(b: u8) bool {
    return switch (b) {
        // see std.ascii.whitespace
        ' ', '\t', std.ascii.control_code.vt, std.ascii.control_code.ff => true,
        else => false,
    };
}

fn predNewline(b: u8) bool {
    return switch (b) {
        '\r', '\n' => true,
        else => false,
    };
}

fn predWhitespace(b: u8) bool {
    return predSpace(b) or predNewline(b);
}

fn predNotNewline(b: u8) bool {
    return !predNewline(b);
}

fn predNotWhitespace(b: u8) bool {
    return !predWhitespace(b);
}

fn peekMany(self: *Scanner, num_bytes: usize) ![]const u8 {
    std.debug.assert(num_bytes != 0);
    if (self.buffer.len < num_bytes or self.cursor > self.buffer.len - num_bytes) return Error.UnexpectedEndOfBuffer;
    return self.buffer[self.cursor .. self.cursor + num_bytes];
}

fn peekSingle(self: *Scanner) !u8 {
    return (try self.peekMany(1))[0];
}

fn consumeNone(self: *Scanner) !Token.Range {
    return .{
        .start = self.cursor,
        .end = self.cursor,
    };
}

fn consumeAnySingle(self: *Scanner) !Token.Range {
    defer self.cursor += 1;
    return .{
        .start = self.cursor,
        .end = self.cursor + 1,
    };
}

fn consumeSingle(self: *Scanner, predicate: fn (char: u8) bool) !?Token.Range {
    const peek = try self.peekSingle();
    if (!predicate(peek)) return null;
    return try self.consumeAnySingle();
}

fn consumeMany(self: *Scanner, predicate: fn (char: u8) bool) !?Token.Range {
    var range: Token.Range = try self.consumeSingle(predicate) orelse return null;
    while (try self.consumeSingle(predicate)) |further_range| {
        range = range.expand(further_range);
    }
    return range;
}

fn consumeString(self: *Scanner, kind: StringKind) !Token.Range {
    var escape = false;
    var range = try self.consumeNone();
    while (true) {
        if (try self.consumeSingle(predEscape)) |escape_range| {
            range = range.expand(escape_range);
        }
        switch (kind) {
            .single_line => {
                if (predNewline(try self.peekSingle())) return range;
                if (!escape and try self.consumeSingle(predString) != null) return range;
                escape = !escape and predEscape(try self.peekSingle());
                range = range.expand(try self.consumeAnySingle());
            },
            .multiple_lines => {
                if (std.mem.eql(u8, try self.peekMany(3), "\"\"\"")) {
                    self.cursor += 3;
                    return range;
                }
                escape = !escape and predEscape(try self.peekSingle());
                range = range.expand(try self.consumeAnySingle());
            },
        }
        escape = false;
    }
    unreachable;
}

fn consumeKeyPart(self: *Scanner) !?Token.Range {
    return if (try self.consumeMany(predKey)) |range| range else string_key: {
        if (try self.consumeSingle(predString) == null) return null;
        break :string_key try self.consumeString(.single_line);
    };
}

fn statefulConsumeTableClose(self: *Scanner) !?Token.Range {
    const table_close_range = try self.consumeSingle(predTableClose) orelse return null;
    if (self.state == .expect_many_table_key_with_access) {
        const many_table_close_range = try self.consumeSingle(predTableClose) orelse return Error.UnexpectedToken;
        self.state = .expect_key_or_table;
        return table_close_range.expand(many_table_close_range);
    }
    self.state = .expect_key_or_table;
    return table_close_range;
}

fn statefulConsumeRoot(self: *Scanner) !Token {
    if (try self.consumeMany(predWhitespace)) |range| return range.token(.ignored);
    const key_range = try self.consumeKeyPart() orelse {
        const table_open_range = try self.consumeSingle(predTableOpen) orelse return Error.UnexpectedToken;

        if (try self.consumeSingle(predTableOpen)) |many_table_open_range| {
            self.state = .expect_many_table_key;
            errdefer self.state = .expect_key_or_table;
            return table_open_range.expand(many_table_open_range).token(.many_table_start);
        }
        self.state = .expect_table_key;
        errdefer self.state = .expect_key_or_table;
        return table_open_range.token(.table_start);
    };

    self.state = .expect_key_with_access;
    errdefer self.state = .expect_key_or_table;
    return key_range.token(.key);
}

pub fn next(self: *Scanner) !?Token {
    const original_pos = self.cursor;
    errdefer self.cursor = original_pos;
    switch (self.state) {
        .expect_key_or_table => {
            return self.statefulConsumeRoot() catch |e| switch (e) {
                Error.UnexpectedEndOfBuffer => return null,
                else => return e,
            };
        },
        .expect_key_with_access => {
            if (try self.consumeMany(predSpace)) |range| return range.token(.ignored);
            if (try self.consumeSingle(predAccess)) |range| return range.token(.access);
            if (try self.consumeSingle(predEq) != null) {
                self.state = .expect_value;
                errdefer self.state = .expect_key_with_access;
                return try self.next();
            }
            const key_range = try self.consumeKeyPart() orelse return Error.UnexpectedToken;
            return key_range.token(.key);
        },
        .expect_value => {
            if (try self.consumeMany(predSpace)) |range| return range.token(.ignored);
            if (try self.consumeSingle(predArrayOpen)) |range| return range.token(.array_start);
            if (try self.consumeSingle(predArrayClose)) |range| return range.token(.array_end);
            if (try self.consumeSingle(predInlineTableOpen)) |range| return range.token(.inline_table_start);
            if (try self.consumeSingle(predInlineTableClose)) |range| return range.token(.inline_table_end);

            if (try self.consumeSingle(predNewline)) |range| {
                self.state = .expect_key_or_table;
                errdefer self.state = .expect_value;
                const more_whitespace_range = try self.consumeMany(predWhitespace) orelse return range.token(.ignored);
                return range.expand(more_whitespace_range).token(.ignored);
            } else if (try self.consumeSingle(predDelimeter)) |range| {
                _ = range;
                return try self.next();
            }

            if (std.mem.eql(u8, try self.peekMany(3), "\"\"\"")) {
                self.cursor += 3;
                const string_range = try self.consumeString(.multiple_lines);
                return string_range.token(.value);
            } else if (try self.consumeSingle(predString) != null) {
                const string_range = try self.consumeString(.single_line);
                return string_range.token(.value);
            }

            const full_value_range = try self.consumeMany(predNotWhitespace) orelse return Error.UnexpectedToken;
            return full_value_range.token(.value);
        },
        .expect_table_key, .expect_many_table_key => |original_state| {
            if (try self.consumeMany(predSpace)) |range| return range.token(.ignored);
            const key_range = try self.consumeKeyPart() orelse return Error.UnexpectedToken;
            self.state = switch (self.state) {
                .expect_table_key => .expect_table_key_with_access,
                .expect_many_table_key => .expect_many_table_key_with_access,
                else => unreachable,
            };
            errdefer self.state = original_state;
            return key_range.token(.key);
        },
        .expect_table_key_with_access, .expect_many_table_key_with_access => |original_state| {
            if (try self.consumeMany(predSpace)) |range| return range.token(.ignored);
            if (try self.consumeSingle(predAccess)) |range| return range.token(.access);
            if (try self.statefulConsumeTableClose()) |range| {
                self.state = .expect_key_or_table;
                errdefer self.state = original_state;
                return range.token(switch (original_state) {
                    .expect_table_key_with_access => .table_end,
                    .expect_many_table_key_with_access => .many_table_end,
                    else => unreachable,
                });
            }
            const key_range = try self.consumeKeyPart() orelse return Error.UnexpectedToken;
            return key_range.token(.key);
        },
    }
}

pub fn tokenContents(self: *Scanner, token: Token) []const u8 {
    return self.buffer[token.range.start..token.range.end];
}

const test_buf: []const u8 =
    \\name="Write a Shopping List"
    \\tags=["personal","weekly", "barney"]
    \\
    \\assigned_to="everyone"
    \\priority="medium"
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
    try std.testing.expect(token != null and token.?.kind == kind);
}

fn testAnyScanner(scanner: anytype) !void {
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .value);
    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .array_start);
    try expectToken(try scanner.next(), .value);
    try expectToken(try scanner.next(), .value);
    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .value);
    try expectToken(try scanner.next(), .array_end);

    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .value);
    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .value);
    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .value);

    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .many_table_start);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .many_table_end);
    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .value);
    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .value);

    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .many_table_start);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .many_table_end);
    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .value);
    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .value);
    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .array_start);
    try expectToken(try scanner.next(), .value);
    try expectToken(try scanner.next(), .array_end);

    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .many_table_start);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .many_table_end);
    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .value);
    try expectToken(try scanner.next(), .ignored);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .value);
}

test Scanner {
    var scanner: Scanner = .{ .buffer = test_buf };
    try testAnyScanner(&scanner);
}

pub fn BufferedReaderScanner(comptime buf_size: usize, comptime ReaderType: type) type {
    return struct {
        const BufferedReaderScannerT = @This();

        pub const BufferedReaderError = Error || ReaderType.Error || error{BufferTooSmall};

        buffer: [buf_size]u8 = undefined,
        buffer_global_offset: usize = 0,
        eof: bool = false,
        reader: ReaderType,

        scanner: Scanner = .{ .buffer = &.{} },

        fn fillBuffer(self: *BufferedReaderScannerT) !void {
            const request_bytes = buf_size - self.scanner.buffer.len;
            const read_bytes = try self.reader.readAll(self.buffer[buf_size - request_bytes ..]);
            if (read_bytes != request_bytes) self.eof = true;
            self.scanner.buffer = &self.buffer;
        }

        fn moveBufferForward(self: *BufferedReaderScannerT) !void {
            const request_bytes = self.scanner.cursor;
            if (request_bytes == 0) return BufferedReaderError.BufferTooSmall;
            self.buffer_global_offset += request_bytes;
            std.mem.copyForwards(u8, self.buffer[0 .. buf_size - request_bytes], self.buffer[request_bytes..]);
            const read_bytes = try self.reader.readAll(self.buffer[buf_size - request_bytes ..]);
            if (read_bytes != request_bytes) self.eof = true;
            self.scanner.cursor = 0;
            self.scanner.buffer = &self.buffer;
        }

        fn adjustBuffer(self: *BufferedReaderScannerT) !void {
            if (self.eof) return Error.UnexpectedEndOfBuffer;
            if (self.scanner.buffer.len < buf_size) {
                try self.fillBuffer();
            } else {
                try self.moveBufferForward();
            }
        }

        fn adjustBufferNext(self: *BufferedReaderScannerT) BufferedReaderError!?Token {
            try self.adjustBuffer();
            return try self.nextImpl();
        }

        fn nextImpl(self: *BufferedReaderScannerT) BufferedReaderError!?Token {
            return self.scanner.next() catch |e| switch (e) {
                Error.UnexpectedEndOfBuffer => return try self.adjustBufferNext(),
                else => return e,
            } orelse try self.adjustBufferNext();
        }

        pub fn next(self: *BufferedReaderScannerT) !?Token {
            var next_token_in_buffer = try self.nextImpl() orelse return null;
            next_token_in_buffer.range = next_token_in_buffer.range.offset(self.buffer_global_offset);
            return next_token_in_buffer;
        }

        pub fn tokenContents(self: *BufferedReaderScannerT, token: Token) []const u8 {
            return self.buffer[token.range.start - self.buffer_global_offset .. token.range.end - self.buffer_global_offset];
        }
    };
}

pub fn bufferedReaderScanner(reader: anytype) BufferedReaderScanner(4096, @TypeOf(reader)) {
    return .{ .reader = reader };
}

test BufferedReaderScanner {
    var fba = std.io.fixedBufferStream(test_buf);
    const reader = fba.reader();

    var scanner = bufferedReaderScanner(reader);
    try testAnyScanner(&scanner);
}
