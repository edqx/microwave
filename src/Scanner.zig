const std = @import("std");

const Scanner = @This();

pub const Error = error{UnexpectedByte};

pub const Range = struct { usize, usize };

pub const Token = struct {
    pub const Kind = enum {
        newline,

        comment,
        identifier,
        access,
        string,
        literal_string,
        equals,
        table_or_array_start,
        table_or_array_end,
        inline_table_start,
        inline_table_end,

        integer,
        base_integer,
        float,
        inf,
        nan,
        bool,

        offset_date_time,
        local_date_time,
        local_date,
        local_time,
    };

    kind: Kind,
    contents: []u8,

    file_range: Range = .{ 0, 0 },
};

pub const State = enum {
    root,
    table_key,
    inline_key,
    value,
    array_container,
};

pub const VTable = struct {
    getSeekPos: *const fn (scanner: *Scanner) usize = defaultSeekPos,
};

fn defaultSeekPos(scanner: *Scanner) usize {
    _ = scanner;
    return 0;
}

vtable: *const VTable,

reader: *std.Io.Reader,

pub fn next(scanner: *Scanner) !?Token {
    const start = scanner.getPos();
    var maybe_token = try scanner.nextImpl();
    const end = scanner.getPos();

    if (maybe_token) |*token| {
        token.file_range = .{ start, end };
    }
    return maybe_token;
}

fn nextImpl(scanner: *Scanner) !?Token {
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
            // TODO: check for dates
            // switch (byte) {
            //     // dates can't start with + or -, so dont bother checking
            //     '0'...'9' => {
            //         if (scanner.reader.peek(35)) |slice| {

            //         }
            //     }
            // }
            const slice = try scanner.takeUntilNotNumberOrIdentifier();
            // TODO: check for float
            // TODO: check for integer base
            // TODO: check if this contains non-number bytes. this makes it an .identifier instead
            return .{
                .kind = .integer,
                .contents = slice,
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
            return try scanner.nextImpl();
        },
    }
}

fn isWhitespace(char: u8) bool {
    return switch (char) {
        ' ', '\t' => true,
        else => false,
    };
}

fn isIdentifier(char: u8) bool {
    return switch (char) {
        'A'...'Z', 'a'...'z', '0'...'9', '_', '-' => true,
        else => false,
    };
}

fn isNumber(char: u8) bool {
    return switch (char) {
        '0'...'9', '.', 'e', '+', '-' => true,
        else => false,
    };
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

fn takeUntilNotNumberOrIdentifier(scanner: *Scanner) ![]u8 {
    var take: []u8 = &.{};
    while (true) {
        const peek = try scanner.reader.peek(take.len + 1);
        const last = peek[peek.len - 1];
        if (isIdentifier(last) or isNumber(last)) break;
        take = peek;
    }
    scanner.reader.toss(take.len);
    return take;
}

fn takeUntilNewline(scanner: *Scanner) ![]u8 {
    const slice = try scanner.reader.takeDelimiterExclusive('\n');
    // scanner.reader.toss(1); // remove ending newline
    if (slice[slice.len - 1] == '\r') {
        return slice[0 .. slice.len - 2];
    }
    return slice[0 .. slice.len - 1];
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
    try expectToken(try scanner.next(), .comment);
    try expectToken(try scanner.next(), .newline);

    try expectToken(try scanner.next(), .identifier);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .string);
    try expectToken(try scanner.next(), .newline);

    try expectToken(try scanner.next(), .identifier);
    try expectToken(try scanner.next(), .equals);

    try expectToken(try scanner.next(), .table_or_array_start);

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
