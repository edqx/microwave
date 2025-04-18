const std = @import("std");

const Scanner = @This();

pub const Error = error{ UnexpectedEndOfBuffer, UnexpectedByte };

pub const Token = struct {
    pub const Kind = enum {
        whitespace,
        comment,
        newline,
        delimeter,
        key,
        access,
        equals,
        literal_string,
        literal_base_integer,
        literal_integer,
        literal_float,
        literal_inf,
        literal_nan,
        literal_bool,
        literal_offset_date_time,
        literal_local_date_time,
        literal_local_date,
        literal_local_time,
        array_start_or_table_start,
        array_end_or_table_end,
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

const StringKind = enum {
    single_line,
    multiple_lines,
};

buffer: []const u8,
can_request_more: bool = false,
offset: usize = 0,

fn isCommentChar(b: u8) bool {
    return b == '#';
}

fn isKeyChar(b: u8) bool {
    return switch (b) {
        'A'...'Z', 'a'...'z', '0'...'9', '_', '-' => true,
        else => false,
    };
}

fn isUtcChar(b: u8) bool {
    return switch (b) {
        'Z', 'z' => true,
        else => false,
    };
}

fn isDateSeparator(b: u8) bool {
    return b == '-';
}

fn isTimeSeparator(b: u8) bool {
    return b == ':';
}

fn isDateTimeSeparator(b: u8) bool {
    return switch (b) {
        ' ', 'T', 't' => true,
        else => false,
    };
}

fn isNumberSign(b: u8) bool {
    return switch (b) {
        '+', '-' => true,
        else => false,
    };
}

fn isBaseSignifierPrefix(b: u8) bool {
    return b == '0';
}

fn isBase10Digit(b: u8) bool {
    return switch (b) {
        '0'...'9' => true,
        else => false,
    };
}

fn isBase16Digit(b: u8) bool {
    return switch (b) {
        '0'...'9', 'a'...'f', 'A'...'F' => true,
        else => false,
    };
}

fn isBase8Digit(b: u8) bool {
    return switch (b) {
        '0'...'7' => true,
        else => false,
    };
}

fn isBase2Digit(b: u8) bool {
    return switch (b) {
        '0', '1' => true,
        else => false,
    };
}

fn isUnsignedIntegerBase(b: u8) bool {
    return switch (b) {
        'x', 'o', 'b' => true,
        else => false,
    };
}

fn isDigitGrouper(b: u8) bool {
    return b == '_';
}

fn isFractionalSeparator(b: u8) bool {
    return b == '.';
}

fn isExponentialSeparator(b: u8) bool {
    return switch (b) {
        'e', 'E' => true,
        else => false,
    };
}

fn isStringChar(b: u8) bool {
    return switch (b) {
        '"', '\'' => true,
        else => false,
    };
}

fn isEqualsChar(b: u8) bool {
    return b == '=';
}

fn isStringEscapeChar(b: u8) bool {
    return b == '\\';
}

fn isAccessChar(b: u8) bool {
    return b == '.';
}

fn isArrayOpenChar(b: u8) bool {
    return b == '[';
}

fn isArrayCloseChar(b: u8) bool {
    return b == ']';
}

fn isTableOpenChar(b: u8) bool {
    return b == '[';
}

fn isTableCloseChar(b: u8) bool {
    return b == ']';
}

fn isInlineTableOpenChar(b: u8) bool {
    return b == '{';
}

fn isInlineTableCloseChar(b: u8) bool {
    return b == '}';
}

fn isDelimeterChar(b: u8) bool {
    return b == ',';
}

fn isSpaceChar(b: u8) bool {
    return switch (b) {
        // see std.ascii.whitespace
        ' ', '\t', std.ascii.control_code.vt, std.ascii.control_code.ff => true,
        else => false,
    };
}

fn isNewlineChar(b: u8) bool {
    return switch (b) {
        '\r', '\n' => true,
        else => false,
    };
}

fn isWhitespaceChar(b: u8) bool {
    return isSpaceChar(b) or isNewlineChar(b);
}

fn isNotNewlineChar(b: u8) bool {
    return !isNewlineChar(b);
}

fn isNotWhitespaceChar(b: u8) bool {
    return !isWhitespaceChar(b);
}

pub fn reachedEnd(self: *Scanner, offset_required: usize) bool {
    return !self.can_request_more and
        (self.buffer.len < offset_required or self.offset == self.buffer.len - offset_required);
}

fn peekMany(self: *Scanner, num_bytes: usize) ![]const u8 {
    std.debug.assert(num_bytes != 0);
    if (self.buffer.len < num_bytes or self.offset > self.buffer.len - num_bytes) return Error.UnexpectedEndOfBuffer;
    return self.buffer[self.offset .. self.offset + num_bytes];
}

fn peekSingle(self: *Scanner) !u8 {
    return (try self.peekMany(1))[0];
}

fn peekAdvance(self: *Scanner, offset: usize) !u8 {
    return (try self.peekMany(offset))[offset - 1];
}

fn consumeNone(self: *Scanner) !Token.Range {
    return .{
        .start = self.offset,
        .end = self.offset,
    };
}

fn consumeAnySingle(self: *Scanner) !Token.Range {
    if (self.offset > self.buffer.len - 1) return Error.UnexpectedEndOfBuffer;
    defer self.offset += 1;
    return .{
        .start = self.offset,
        .end = self.offset + 1,
    };
}

fn consumeSingle(self: *Scanner, predicate: fn (char: u8) bool) !?Token.Range {
    const peek = self.peekSingle() catch |e| switch (e) {
        Error.UnexpectedEndOfBuffer => return if (self.reachedEnd(0)) null else e,
        else => return e,
    };
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

fn consumeSlice(self: *Scanner, slice: []const u8) !?Token.Range {
    for (0..slice.len) |i| {
        const peek = self.peekAdvance(i + 1) catch |e| switch (e) {
            Error.UnexpectedEndOfBuffer => if (self.reachedEnd(i)) return null else return e,
            else => return e,
        };
        if (peek != slice[i]) return null;
    }

    defer self.offset += slice.len;
    return .{
        .start = self.offset,
        .end = self.offset + slice.len,
    };
}

fn consumeCommentLine(self: *Scanner) !?Token.Range {
    if (try self.consumeSingle(isCommentChar) == null) return null;
    return try self.consumeMany(isNotNewlineChar);
}

fn consumeString(self: *Scanner, kind: StringKind) !?Token.Range {
    const string_char_kind: []const u8 = switch (kind) {
        .single_line => if (try self.consumeSingle(isStringChar)) |range| self.rangeContents(range) else return null,
        .multiple_lines => if (try self.consumeSlice("\"\"\"")) |range|
            self.rangeContents(range)
        else if (try self.consumeSlice("'''")) |range|
            self.rangeContents(range)
        else
            return null,
    };

    var escape = false;
    var range = try self.consumeNone();
    while (true) {
        switch (kind) {
            .single_line => {
                if (isNewlineChar(try self.peekSingle())) return Error.UnexpectedByte;
            },
            .multiple_lines => {},
        }
        if (!escape and try self.consumeSlice(string_char_kind) != null) return range;
        escape = !escape and isStringEscapeChar(try self.peekSingle());
        range = range.expand(try self.consumeAnySingle());
    }
    unreachable;
}

fn consumeDigitGroups(self: *Scanner, digit_predicate: fn (b: u8) bool) !?Token.Range {
    var digits_range = try self.consumeSingle(digit_predicate) orelse return null;
    while (true) {
        const expect_digit = try self.consumeSingle(isDigitGrouper) != null;
        const another_digit_range = try self.consumeSingle(digit_predicate) orelse
            if (expect_digit) return Error.UnexpectedByte else break;
        digits_range = digits_range.expand(another_digit_range);
    }
    return digits_range;
}

fn consumeUnsignedIntegerBaseNumber(self: *Scanner) !?Token.Range {
    const original_pos = self.offset;

    if (try self.consumeSingle(isBaseSignifierPrefix) == null) return null;
    const base_range = try self.consumeSingle(isUnsignedIntegerBase) orelse {
        self.offset = original_pos;
        return null;
    };
    const base = self.buffer[base_range.start];
    const digits_range = switch (base) {
        'x' => try self.consumeDigitGroups(isBase16Digit),
        'o' => try self.consumeDigitGroups(isBase8Digit),
        'b' => try self.consumeDigitGroups(isBase2Digit),
        else => unreachable,
    } orelse return Error.UnexpectedByte;
    return base_range.expand(digits_range);
}

fn consumeDate(self: *Scanner) !?Token.Range {
    const original_pos = self.offset;

    const year_range = try self.consumeMany(isBase10Digit) orelse return null;
    if (try self.consumeSingle(isDateSeparator) == null) {
        self.offset = original_pos;
        return null;
    }
    const month_range = try self.consumeMany(isBase10Digit) orelse return Error.UnexpectedByte;
    if (try self.consumeSingle(isDateSeparator) == null) return Error.UnexpectedByte;
    const day_range = try self.consumeMany(isBase10Digit) orelse return Error.UnexpectedByte;
    _ = month_range;
    return year_range.expand(day_range);
}

fn consumeTime(self: *Scanner) !?Token.Range {
    const original_pos = self.offset;

    const hour_range = try self.consumeMany(isBase10Digit) orelse return null;
    if (try self.consumeSingle(isTimeSeparator) == null) {
        self.offset = original_pos;
        return null;
    }
    const minute_range = try self.consumeMany(isBase10Digit) orelse return Error.UnexpectedByte;
    if (try self.consumeSingle(isTimeSeparator) == null) return hour_range.expand(minute_range);
    const second_range = try self.consumeMany(isBase10Digit) orelse return Error.UnexpectedByte;
    if (try self.consumeSingle(isFractionalSeparator)) |_| {
        const millisecond_range = try self.consumeMany(isBase10Digit) orelse return Error.UnexpectedByte;
        return hour_range.expand(millisecond_range);
    }
    return hour_range.expand(second_range);
}

fn consumeOffset(self: *Scanner) !?Token.Range {
    if (try self.consumeSingle(isUtcChar)) |range| return range;
    const sign_range = try self.consumeSingle(isNumberSign) orelse return null;
    const hour_range = try self.consumeMany(isBase10Digit) orelse return Error.UnexpectedByte;
    if (try self.consumeSingle(isTimeSeparator) == null) return Error.UnexpectedByte;
    const minute_range = try self.consumeMany(isBase10Digit) orelse return Error.UnexpectedByte;
    _ = hour_range;
    return sign_range.expand(minute_range);
}

fn consumeDateTimeLiteralToken(self: *Scanner) !?Token {
    if (try self.consumeDate()) |date_range| {
        if (try self.consumeSingle(isDateTimeSeparator)) |_| {
            const time_range = try self.consumeTime() orelse return Error.UnexpectedByte;
            if (try self.consumeOffset()) |offset_range| {
                return date_range.expand(offset_range).token(.literal_offset_date_time);
            }
            return date_range.expand(time_range).token(.literal_local_date_time);
        }
        return date_range.token(.literal_local_date);
    }
    const time_range = try self.consumeTime() orelse return null;
    return time_range.token(.literal_local_time);
}

fn consumeNumberLiteralToken(self: *Scanner) !?Token {
    const sign_range = try self.consumeSingle(isNumberSign);
    if (sign_range == null) {
        if (try self.consumeUnsignedIntegerBaseNumber()) |range| return range.token(.literal_base_integer);
    }

    if (try self.consumeSlice("nan")) |nan_range| {
        const special_range = if (sign_range) |range| range.expand(nan_range) else nan_range;
        return special_range.token(.literal_nan);
    }

    if (try self.consumeSlice("inf")) |inf_range| {
        const special_range = if (sign_range) |range| range.expand(inf_range) else inf_range;
        return special_range.token(.literal_inf);
    }

    const digits_range = try self.consumeDigitGroups(isBase10Digit) orelse
        return if (sign_range != null) Error.UnexpectedByte else null;

    const integer_part_range = if (sign_range) |range| range.expand(digits_range) else digits_range;

    if (try self.consumeSingle(isFractionalSeparator) != null) {
        const fraction_digits = try self.consumeDigitGroups(isBase10Digit) orelse return Error.UnexpectedByte;
        if (try self.consumeSingle(isExponentialSeparator) != null) {
            _ = try self.consumeSingle(isNumberSign);
            const exponential_range = try self.consumeDigitGroups(isBase10Digit) orelse return Error.UnexpectedByte;
            return integer_part_range.expand(exponential_range).token(.literal_float);
        }
        return integer_part_range.expand(fraction_digits).token(.literal_float);
    } else if (try self.consumeSingle(isExponentialSeparator) != null) {
        _ = try self.consumeSingle(isNumberSign);
        const exponential_range = try self.consumeDigitGroups(isBase10Digit) orelse return Error.UnexpectedByte;
        return integer_part_range.expand(exponential_range).token(.literal_float);
    }
    return integer_part_range.token(.literal_integer);
}

pub fn next(self: *Scanner) !?Token {
    const original_pos = self.offset;
    errdefer self.offset = original_pos;

    if (try self.consumeMany(isSpaceChar)) |range| return range.token(.whitespace);
    if (try self.consumeMany(isWhitespaceChar)) |range| return range.token(.newline);
    if (try self.consumeCommentLine()) |range| return range.token(.comment);
    if (try self.consumeSingle(isAccessChar)) |range| return range.token(.access);
    if (try self.consumeSingle(isEqualsChar)) |range| return range.token(.equals);
    if (try self.consumeSingle(isArrayOpenChar)) |range| return range.token(.array_start_or_table_start);
    if (try self.consumeSingle(isArrayCloseChar)) |range| return range.token(.array_end_or_table_end);
    if (try self.consumeSingle(isInlineTableOpenChar)) |range| return range.token(.inline_table_start);
    if (try self.consumeSingle(isInlineTableCloseChar)) |range| return range.token(.inline_table_end);
    if (try self.consumeSingle(isDelimeterChar)) |range| return range.token(.delimeter);
    const restore_pos = self.offset;
    if (try self.consumeDateTimeLiteralToken()) |date_time_token| {
        return date_time_token;
    } else {
        self.offset = restore_pos;
    }
    if (try self.consumeNumberLiteralToken()) |number_token| return number_token;
    if (try self.consumeString(.multiple_lines)) |string_range| return string_range.token(.literal_string);
    if (try self.consumeString(.single_line)) |string_range| return string_range.token(.literal_string);
    if (try self.consumeSlice("true") orelse try self.consumeSlice("false")) |range| return range.token(.literal_bool);
    if (try self.consumeMany(isKeyChar)) |range| return range.token(.key);
    return if (self.reachedEnd(0)) null else Error.UnexpectedByte;
}

pub fn cursor(self: *Scanner) usize {
    return self.offset;
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
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .array_start_or_table_start);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .delimeter);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .delimeter);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .array_end_or_table_end);

    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .whitespace);
    try expectToken(try scanner.next(), .comment);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .newline);

    try expectToken(try scanner.next(), .array_start_or_table_start);
    try expectToken(try scanner.next(), .array_start_or_table_start);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .array_end_or_table_end);
    try expectToken(try scanner.next(), .array_end_or_table_end);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .newline);

    try expectToken(try scanner.next(), .array_start_or_table_start);
    try expectToken(try scanner.next(), .array_start_or_table_start);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .array_end_or_table_end);
    try expectToken(try scanner.next(), .array_end_or_table_end);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .array_start_or_table_start);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .array_end_or_table_end);
    try expectToken(try scanner.next(), .newline);

    try expectToken(try scanner.next(), .array_start_or_table_start);
    try expectToken(try scanner.next(), .array_start_or_table_start);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .array_end_or_table_end);
    try expectToken(try scanner.next(), .array_end_or_table_end);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .literal_string);
    try expectToken(try scanner.next(), .newline);
    try expectToken(try scanner.next(), .key);
    try expectToken(try scanner.next(), .equals);
    try expectToken(try scanner.next(), .literal_string);
}

test Scanner {
    var scanner: Scanner = .{ .buffer = test_buf };
    try testAnyScanner(&scanner);
}

pub fn BufferedReaderScanner(comptime buf_size: usize, comptime ReaderType: type) type {
    return struct {
        // todo: optimise buffer space by removing comments in pre-process?

        const BufferedReaderScannerT = @This();

        pub const Error = Scanner.Error || ReaderType.Error || error{BufferTooSmall};

        buffer: [buf_size]u8 = undefined,
        buffer_global_offset: usize = 0,
        reader: ReaderType,

        scanner: Scanner = .{
            .buffer = &.{},
            .can_request_more = true,
        },

        fn fillBuffer(self: *BufferedReaderScannerT) !void {
            const request_bytes = buf_size - self.scanner.buffer.len;
            const read_bytes = try self.reader.readAll(self.buffer[buf_size - request_bytes ..]);
            if (read_bytes != request_bytes) self.scanner.can_request_more = false;
            self.scanner.buffer = &self.buffer;
        }

        fn moveBufferForward(self: *BufferedReaderScannerT) !void {
            const request_bytes = self.scanner.offset;
            if (request_bytes == 0) return BufferedReaderScannerT.Error.BufferTooSmall;
            self.buffer_global_offset += request_bytes;
            std.mem.copyForwards(u8, self.buffer[0 .. buf_size - request_bytes], self.buffer[request_bytes..]);
            const read_bytes = try self.reader.readAll(self.buffer[buf_size - request_bytes ..]);
            if (read_bytes != request_bytes) self.scanner.can_request_more = false;
            self.scanner.offset = 0;
            self.scanner.buffer = &self.buffer;
        }

        fn adjustBuffer(self: *BufferedReaderScannerT) !void {
            if (!self.scanner.can_request_more) return error.UnexpectedEndOfBuffer;
            if (self.scanner.buffer.len < buf_size) {
                try self.fillBuffer();
            } else {
                try self.moveBufferForward();
            }
        }

        fn adjustBufferNext(self: *BufferedReaderScannerT) BufferedReaderScannerT.Error!?Token {
            try self.adjustBuffer();
            return try self.nextImpl();
        }

        fn nextImpl(self: *BufferedReaderScannerT) BufferedReaderScannerT.Error!?Token {
            return self.scanner.next() catch |e| switch (e) {
                error.UnexpectedEndOfBuffer => return try self.adjustBufferNext(),
                else => return e,
            } orelse try self.adjustBufferNext();
        }

        pub fn next(self: *BufferedReaderScannerT) !?Token {
            var next_token_in_buffer = try self.nextImpl() orelse return null;
            next_token_in_buffer.range = next_token_in_buffer.range.offset(self.buffer_global_offset);
            return next_token_in_buffer;
        }

        pub fn cursor(self: *BufferedReaderScannerT) usize {
            return self.buffer_global_offset + self.scanner.offset;
        }

        pub fn rangeContents(self: *BufferedReaderScannerT, range: Token.Range) []const u8 {
            return self.buffer[range.start - self.buffer_global_offset .. range.end - self.buffer_global_offset];
        }

        pub fn tokenContents(self: *BufferedReaderScannerT, token: Token) []const u8 {
            return self.rangeContents(token.range);
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
