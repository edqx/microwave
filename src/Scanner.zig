//! A lexical token iterator for a TOML file.

const std = @import("std");

const Scanner = @This();

pub const Error = std.Io.Reader.DelimiterError || error{ UnexpectedEndOfStream, UnexpectedByte };

/// Represents a file position range, from start to end (exclusive).
pub const Range = struct { usize, usize };

/// A single lexical token in a TOML file.
///
/// https://toml.io/en/v1.0.0#spec
pub const Token = struct {
    pub const Kind = enum {
        /// Either \r\n or just \n
        ///
        /// https://toml.io/en/v1.0.0#spec
        newline,
        /// A '#' followed by the rest of the line, not including the newline
        ///
        /// https://toml.io/en/v1.0.0#comment
        comment,

        /// A combination of A-Z, a-z, _, -, 0-9
        ///
        /// Note that not _all_ keys will be represented by an .identifier token:
        /// - String keys are represented by .string or .literal_string
        /// - Integer keys and other ambiguous tokens used for keys may fall under .integer, .base_integer, .float, etc.
        ///
        /// https://toml.io/en/v1.0.0#keys
        identifier,
        /// A single '.' used for adding key depth and accessing sub-keys
        ///
        /// https://toml.io/en/v1.0.0#keys
        access,
        /// A single '=' for separating keys from their values
        ///
        /// https://toml.io/en/v1.0.0#keyvalue-pair
        equals,

        /// A single ',' for separating array values, as well as inline table key pairs.
        value_delimiter,

        /// A single-line or multi-line string wrapped in "s with full escaping. Does not
        /// include the quotes.
        ///
        /// https://toml.io/en/v1.0.0#string
        string,
        /// A single-line or multi-line literal string wrapped in 's with no escaping. Does
        /// not include the quotes.
        ///
        /// https://toml.io/en/v1.0.0#string
        literal_string,

        /// A sequence of digits, preceeded by a sign (+ or -) and optionally including
        /// underscores to separate groups of digits.
        ///
        /// https://toml.io/en/v1.0.0#integer
        integer,
        /// A sequence of digits with a base, NOT preceeded by a sign (+ or -) but beginning
        /// with a base indicator of either "0x" for hex, "0b" for binary or "0o" for octal.
        ///
        /// The actual digits are not checked against the base here, meaning that a binary
        /// base integer may contain digits that are not 0 or 1.
        ///
        /// https://toml.io/en/v1.0.0#integer
        base_integer,
        /// A floating point number, consisting of a sign (+ or -), a sequence of digits,
        /// optionally a fractional part and optionally an exponential part.
        ///
        /// https://toml.io/en/v1.0.0#float
        float,
        /// Positive or negative infinity, represented by the keyword "inf".
        ///
        /// https://toml.io/en/v1.0.0#float
        inf,
        /// Positive or negative NaN, represented by the keyword "nan".
        ///
        /// https://toml.io/en/v1.0.0#float
        nan,

        /// An [RFC 3339](https://www.rfc-editor.org/rfc/rfc3339) formatted date-time
        /// with a timezone offset, optionally with indefinite precision of fractional
        /// seconds.
        ///
        /// https://toml.io/en/v1.0.0#offset-date-time
        offset_date_time,
        /// An [RFC 3339](https://www.rfc-editor.org/rfc/rfc3339) formatted date-time
        /// without a timezone offset, optionally with indefinite precision of fractional
        /// seconds.
        ///
        /// https://toml.io/en/v1.0.0#offset-date-time
        local_date_time,
        /// An [RFC 3339](https://www.rfc-editor.org/rfc/rfc3339) formatted date.
        ///
        /// https://toml.io/en/v1.0.0#offset-date-time
        local_date,
        /// An [RFC 3339](https://www.rfc-editor.org/rfc/rfc3339) formatted time, optionally
        /// with indefinite precision of fractional seconds.
        ///
        /// https://toml.io/en/v1.0.0#offset-date-time
        local_time,

        /// Either the beginning of a table at the root level, or the start of an array value.
        /// Since the scanner does not hold any state, it is up to the user to determine
        /// which the token represents.
        ///
        /// https://toml.io/en/v1.0.0#array
        ///
        /// https://toml.io/en/v1.0.0#table
        table_or_array_start,
        /// Either the end of a table at the root level, or the end of an array value.
        /// Since the scanner does not hold any state, it is up to the user to determine
        /// which the token represents.
        ///
        /// https://toml.io/en/v1.0.0#array
        ///
        /// https://toml.io/en/v1.0.0#table
        table_or_array_end,
        /// The beginning of an inline table.
        ///
        /// https://toml.io/en/v1.0.0#inline-table
        inline_table_start,
        /// The end of an inline table.
        ///
        /// https://toml.io/en/v1.0.0#inline-table
        inline_table_end,
    };

    /// The kind of token that the underlying bytes represent.
    kind: Kind,
    /// The underlying bytes that make up this token. Note that this references buffered memory
    /// inside the reader given to the scanner, and therefore may be invalidated by future
    /// iterations when the buffer is flushed.
    contents: []u8,

    /// The byte offsets of the file for the entire token, if a `VTable` with `VTable.getSeekPos` is
    /// provided to the scanner. Note that it is not required to take into account the current
    /// reader buffer seek position, as this is accounted for already by the scanner.
    ///
    /// Note that this does not just include the _data_ of the token, but the entire region
    /// of the file responsible for the token. e.g., the file range for string tokens will
    /// include the wrapping quotes.
    file_range: Range = .{ 0, 0 },
};

/// A table of functions for providing additional information to the scanner.
pub const VTable = struct {
    /// The position in the overall file/source that the scanner is currently at. Note that
    ///
    ///
    /// Defaults to 0, useful for reading from a fixed slice.
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
    return switch (char) {
        ',', ']', '}' => true,
        else => false,
    };
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

fn unexpectedEof(err: std.Io.Reader.Error) Error {
    return switch (err) {
        error.EndOfStream => error.UnexpectedEndOfStream,
        error.ReadFailed => |e| e,
    };
}

vtable: *const VTable = &.{},
/// The underlying reader that the scanner will attempt to read from.
///
/// Note that an adequate buffer size is required for reading a lot of potential data from
/// the TOML source. A minimum size of 35 bytes is recommended to accomodate the larger data
/// types, such as dates.
reader: *std.Io.Reader,

/// Take a single lexical token from the underlying reader, advancing the seek
/// position.
pub fn takeToken(scanner: *Scanner) Error!?Token {
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
            const contents = try scanner.takeUntilNotIdentifier();

            return .{
                .kind = if (std.mem.eql(u8, contents, "inf"))
                    .inf
                else if (std.mem.eql(u8, contents, "nan"))
                    .nan
                else
                    .identifier,
                .contents = contents,
            };
        },
        '+', '-', '0'...'9' => {
            if (byte == '+' or byte == '-') {
                if (peekOffset(scanner.reader, 1, 3)) |bytes| {
                    if (std.mem.eql(u8, bytes, "inf")) {
                        return .{
                            .kind = .inf,
                            .contents = try scanner.reader.take(4),
                        };
                    }
                    if (std.mem.eql(u8, bytes, "nan")) {
                        return .{
                            .kind = .nan,
                            .contents = try scanner.reader.take(4),
                        };
                    }
                } else |e| try ignoreEof(e);
            }

            // we can use this to fill the buffer as much as possible. this works because the final '\n'
            // is still ultimately part of the buffer
            _ = try scanner.reader.peekDelimiterExclusive('\n');
            if (isDigit(byte)) {
                // let's fill the buffer early so that any tossed bytes don't get invalidated
                // by subsequent calls to 'peek'
                if (try scanner.maybeTakeAnyDateTime()) |date_time_token| {
                    return date_time_token;
                }
            }

            var kind: Token.Kind = .integer;
            var integer_base: ?u8 = null;

            const start_seek = scanner.reader.seek;

            if (byte == '+' or byte == '-') {
                scanner.reader.toss(1);
            }

            const determinant = scanner.reader.peekByte() catch |e| return unexpectedEof(e);

            if (determinant == '0') {
                scanner.reader.toss(1);
                if (scanner.reader.peekByte()) |base| {
                    kind, integer_base = switch (base) {
                        'b' => .{ .base_integer, 2 },
                        'o' => .{ .base_integer, 8 },
                        'x' => .{ .base_integer, 16 },
                        '.', 'e', 'E' => .{ .float, null }, // we allow floats to start with 0. or 0e
                        else => if (isIdentifier(base)) .{ .identifier, null } else .{ .integer, null },
                    };
                    // we expect that if the next byte is not part of a number, it will fall through
                    // this parsing mechanism, so it's best not to consume it
                    if (kind != .integer) {
                        scanner.reader.toss(1);
                    }
                } else |e| try ignoreEof(e);
            }

            // we can't allow signs on bin/otc/hex literals
            if (integer_base != null and (byte == '+' or byte == '-')) {
                return error.UnexpectedByte;
            }

            switch (kind) {
                .integer => {
                    _ = try scanner.discardIntegerPart(false);
                    if (try scanner.discardFraction()) kind = .float;
                    if (try scanner.discardExponent()) kind = .float;
                },
                .base_integer => {
                    expect_base_digit: {
                        const expect_digit = scanner.reader.peekByte() catch |e| switch (e) {
                            error.ReadFailed => return e,
                            error.EndOfStream => {
                                kind = .identifier;
                                break :expect_base_digit;
                            },
                        };
                        switch (expect_digit) {
                            '0'...'9', 'a'...'f', 'A'...'F' => {},
                            else => {
                                kind = .identifier;
                            },
                        }
                    }
                    _ = try scanner.discardIntegerPart(true);
                },
                .float => { // this branch only occurs on 0. or 0e
                    _ = try scanner.discardIntegerPart(false);
                    _ = try scanner.discardExponent();
                },
                .identifier => {},
                else => unreachable,
            }

            if (scanner.reader.peekByte()) |e| {
                if (isIdentifier(e)) {
                    kind = .identifier;
                }
            } else |e| try ignoreEof(e);

            if (kind == .identifier) {
                _ = try scanner.takeUntilNotIdentifier();
            }

            return .{
                .kind = kind,
                .contents = scanner.reader.buffer[start_seek..scanner.reader.seek],
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
            if (scanner.reader.peek(2)) |bytes| {
                if (std.mem.eql(u8, bytes, "\"\"")) {
                    scanner.reader.toss(2);
                    return .{
                        .kind = .string,
                        .contents = try scanner.takeString(.multiline_normal),
                    };
                }
            } else |e| try ignoreEof(e);
            return .{
                .kind = .string,
                .contents = try scanner.takeString(.normal),
            };
        },
        '\'' => {
            scanner.reader.toss(1);
            if (scanner.reader.peek(2)) |bytes| {
                if (std.mem.eql(u8, bytes, "''")) {
                    scanner.reader.toss(2);
                    return .{
                        .kind = .literal_string,
                        .contents = try scanner.takeString(.multiline_literal),
                    };
                }
            } else |e| try ignoreEof(e);
            return .{
                .kind = .literal_string,
                .contents = try scanner.takeString(.literal),
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
        ',' => {
            return .{
                .kind = .value_delimiter,
                .contents = try scanner.reader.take(1),
            };
        },
        else => {
            scanner.reader.toss(1);
            return try scanner.takeTokenImpl();
        },
    }
}

const StringKind = enum {
    normal,
    multiline_normal,
    literal,
    multiline_literal,
};

fn takeString(scanner: *Scanner, kind: StringKind) ![]u8 {
    var take: []u8 = &.{};
    while (scanner.reader.peek(take.len + 1)) |bytes| {
        switch (bytes[bytes.len - 1]) {
            0x00...0x08, 0x0b...0x1f, 0x7f => {
                return error.UnexpectedByte;
            },
            0x0a => switch (kind) {
                .normal, .literal => return error.UnexpectedByte,
                .multiline_literal, .multiline_normal => {},
            },
            else => {},
        }
        take = bytes;
        // for multi-line strings, the spec allows closing them with 4 or 5 quotes,
        // in which case the first 2 are part of the string
        switch (kind) {
            .normal => if (take[take.len - 1] == '"') {
                if (take.len > 1 and take[take.len - 2] == '\\') {
                    continue;
                }
                scanner.reader.toss(take.len);
                take.len -= 1;
                return take;
            },
            .multiline_normal => if (std.mem.endsWith(u8, take, "\"\"\"")) {
                if (take.len > 3 and take[take.len - 4] == '\\') {
                    continue;
                }
                if (scanner.reader.peek(take.len + 1)) |more_bytes| {
                    if (more_bytes[more_bytes.len - 1] == '\"') {
                        continue;
                    }
                } else |e| try ignoreEof(e);

                scanner.reader.toss(take.len);
                take.len -= 3;
                return take;
            },
            .literal => if (take[take.len - 1] == '\'') {
                scanner.reader.toss(take.len);
                take.len -= 1;
                return take;
            },
            .multiline_literal => if (std.mem.endsWith(u8, take, "'''")) {
                if (scanner.reader.peek(take.len + 1)) |more_bytes| {
                    if (more_bytes[more_bytes.len - 1] == '\'') {
                        continue;
                    }
                } else |e| try ignoreEof(e);

                scanner.reader.toss(take.len);
                take.len -= 3;
                return take;
            },
        }
    } else |e| switch (e) {
        error.ReadFailed => return e,
        error.EndOfStream => return error.UnexpectedEndOfStream,
    }
    return take;
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

// we don't ever really 'discard' it, we just read it directly from the buffer
// it only has this name because this function does not track the buffer state, so it
// may refresh the buffer
fn discardIntegerPart(scanner: *Scanner, base: bool) !bool {
    var underscore_allowed = false;
    var num_digits: usize = 0;
    while (scanner.reader.peekByte()) |digit| : (num_digits += 1) {
        switch (digit) {
            '_' => {
                if (!underscore_allowed) return error.UnexpectedByte;
                underscore_allowed = false;
            },
            '0'...'9' => {
                underscore_allowed = true;
            },
            'a'...'f', 'A'...'F' => {
                if (!base) break;
                underscore_allowed = true;
            },
            else => break,
        }
        scanner.reader.toss(1);
    } else |e| try ignoreEof(e);
    if (!underscore_allowed and num_digits > 0) return error.UnexpectedByte;
    return num_digits > 0;
}

fn discardFraction(scanner: *Scanner) !bool {
    if (scanner.reader.peekByte()) |exp| {
        if (isFractionDelimiter(exp)) {
            scanner.reader.toss(1);
            _ = try scanner.discardIntegerPart(false);
            return true;
        }
    } else |e| try ignoreEof(e);
    return false;
}

fn discardExponent(scanner: *Scanner) !bool {
    if (scanner.reader.peekByte()) |exp| {
        if (isExponentialDelimiter(exp)) {
            scanner.reader.toss(1);
            if (scanner.reader.peekByte()) |sign| {
                if (isSign(sign)) {
                    scanner.reader.toss(1);
                }
            } else |e| try ignoreEof(e);

            _ = try scanner.discardIntegerPart(false);
            return true;
        }
    } else |e| try ignoreEof(e);
    return false;
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
                    .contents = scanner.reader.buffer[start_seek..scanner.reader.seek],
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
                        .contents = scanner.reader.buffer[start_seek..scanner.reader.seek],
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

/// Get the current position in the overall TOML file that the scanner is at.
pub fn getPos(scanner: *Scanner) usize {
    return scanner.vtable.getSeekPos(scanner) + scanner.reader.seek;
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

test "Scanner2" {
    const buf =
        \\str = ''''That,' she said, 'is still pointless.''''
    ;
    var reader: std.Io.Reader = .fixed(buf);

    var scanner: Scanner = .{
        .vtable = &.{},
        .reader = &reader,
    };

    while (try scanner.takeToken()) |_| {}
}
