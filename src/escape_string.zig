const std = @import("std");

fn trimInitialNewlineFromString(token_contents: []const u8) []const u8 {
    if (token_contents.len > 0) {
        if (token_contents[0] == '\n') return token_contents[1..];
        if (token_contents[0] == '\r' and token_contents[1] == '\n') return token_contents[2..];
    }

    return token_contents;
}

pub fn parseEscapedStringAlloc(allocator: std.mem.Allocator, token_contents: []const u8) (std.mem.Allocator.Error || error{ WriteFailed, InvalidEscape, InvalidUtf8 })![]const u8 {
    var allocating_writer: std.Io.Writer.Allocating = try .initCapacity(allocator, token_contents.len);
    defer allocating_writer.deinit();

    const writer = &allocating_writer.writer;

    const trimmed_contents = trimInitialNewlineFromString(token_contents);

    const codepoints = std.unicode.Utf8View.init(trimmed_contents) catch return error.InvalidUtf8;
    var iter = codepoints.iterator();

    while (iter.nextCodepointSlice()) |slice| {
        const decoded_codepoint = std.unicode.utf8Decode(slice) catch return error.InvalidUtf8;

        if (decoded_codepoint == '\\') {
            if (iter.i >= trimmed_contents.len) return error.InvalidEscape;
            const next_byte = trimmed_contents[iter.i];
            iter.i += 1;
            var unicode_buf: [4]u8 = undefined;
            _ = try writer.writeAll(switch (next_byte) {
                '\r', '\n', '\t', ' ' => |tag| {
                    var encounted_newline = tag == '\r' or tag == '\n';
                    while (true) : (iter.i += 1) {
                        if (iter.i >= trimmed_contents.len) break;
                        switch (trimmed_contents[iter.i]) {
                            '\n' => {
                                encounted_newline = true;
                                continue;
                            },
                            '\r', '\t', ' ' => continue,
                            else => break,
                        }
                    }
                    if (!encounted_newline) {
                        return error.InvalidEscape;
                    }
                    continue;
                },
                '"' => "\"",
                '\'' => "'",
                '\\' => "\\",
                'b' => &.{std.ascii.control_code.bs},
                't' => "\t",
                'n' => "\n",
                'f' => &.{std.ascii.control_code.ff},
                'r' => "\r",
                'e' => &.{std.ascii.control_code.esc},
                inline 'x', 'u', 'U' => |tag| blk: {
                    const num_nibbles = switch (tag) {
                        'x' => 2,
                        'u' => 4,
                        'U' => 8,
                        else => unreachable,
                    };
                    if (trimmed_contents.len < num_nibbles or iter.i >= trimmed_contents.len - (num_nibbles - 1)) return error.InvalidEscape;
                    const int = trimmed_contents[iter.i..][0..num_nibbles];
                    iter.i += num_nibbles;
                    const parsed_codepoint = std.fmt.parseInt(u21, int, 16) catch return error.InvalidEscape;
                    if (std.unicode.isSurrogateCodepoint(parsed_codepoint)) return error.InvalidEscape;
                    // if (!((parsed_codepoint >= 0 and parsed_codepoint <= 0xd7ff) or
                    //     (parsed_codepoint >= 0xe000 and parsed_codepoint <= 0x10fff))) return Error.InvalidEscape;

                    const num_bytes_to_write = std.unicode.utf8Encode(parsed_codepoint, &unicode_buf) catch return error.InvalidUtf8;
                    break :blk unicode_buf[0..num_bytes_to_write];
                },
                else => return error.InvalidEscape,
            });
            continue;
        }

        _ = try writer.writeAll(slice);
    }

    const string_contents = try allocating_writer.toOwnedSlice();
    errdefer allocator.free(string_contents);
    return string_contents;
}
