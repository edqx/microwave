const std = @import("std");
const parse = @import("parse.zig");

pub fn isBareKey(key_name: []const u8) bool {
    for (key_name) |char| {
        switch (char) {
            'A'...'Z', 'a'...'z', '0'...'9', '_', '-' => {},
            else => return false,
        }
    }
    return true;
}

pub const Options = struct {
    // pub const Check = enum {
    //     arbitrary,
    //     none,
    // };

    pub const DateTimeSeparator = enum {
        t,
        space,
    };

    pub const Newlines = enum {
        crlf,
        lf,
    };

    newlines: Newlines = .lf,
    unicode_full_escape_strings: bool = false,
    format_float_options: std.fmt.format_float.FormatOptions = .{},
    // check_depth: Check = .arbitrary,
    date_time_separator: DateTimeSeparator = .t,
};

pub const NestingKind = union(enum) {
    pub const Container = struct {
        expect_delimeter: bool = false,
    };

    key_pair,
    array: Container,
    inline_table: Container,
};

pub fn WriteStream(WriterT: type, comptime options: Options) type {
    return struct {
        const WriteStreamT = @This();

        underlying_writer: WriterT,

        allocator: std.mem.Allocator,

        stack: std.ArrayListUnmanaged(NestingKind) = .empty,
        start: bool = true,

        fn assertCanWriteKey(self: *WriteStreamT) void {
            std.debug.assert(self.stack.items.len == 0 or self.stack.getLast() == .inline_table);
        }

        fn assertCanWriteValue(self: *WriteStreamT) void {
            std.debug.assert(self.stack.items.len != 0 and switch (self.stack.getLast()) {
                .array, .key_pair => true,
                .inline_table => false,
            });
        }

        pub fn deinit(self: *WriteStreamT) void {
            self.stack.deinit(self.allocator);
        }

        pub fn writeStringRaw(self: *WriteStreamT, string: []const u8) !void {
            if (options.unicode_full_escape_strings) {
                const utf8_view = try std.unicode.Utf8View.init(string);
                var codepoints = utf8_view.iterator();
                while (codepoints.nextCodepoint()) |codepoint| {
                    switch (codepoint) {
                        std.ascii.control_code.bs => try self.underlying_writer.writeAll("\\b"),
                        '\t' => try self.underlying_writer.writeAll("\\t"),
                        '\n' => try self.underlying_writer.writeAll("\\n"),
                        std.ascii.control_code.ff => try self.underlying_writer.writeAll("\\f"),
                        '\r' => try self.underlying_writer.writeAll("\\r"),
                        '"' => try self.underlying_writer.writeAll("\\\""),
                        '\\' => try self.underlying_writer.writeAll("\\"),
                        ' ', '#'...'[', ']'...'~' => try self.underlying_writer.writeByte(@as(u8, @intCast(codepoint))),
                        else => {
                            try self.underlying_writer.print("\\u{d:0>8}", .{codepoint});
                        },
                    }
                }
                return;
            }
            for (string) |char| {
                try self.underlying_writer.writeAll(switch (char) {
                    std.ascii.control_code.bs => "\\b",
                    '\t' => "\\t",
                    '\n' => "\\n",
                    std.ascii.control_code.ff => "\\f",
                    '\r' => "\\r",
                    '"' => "\\\"",
                    '\\' => "\\",
                    else => &.{char},
                });
            }
        }

        pub fn writeKeyRaw(self: *WriteStreamT, key_name: []const u8) !void {
            const bare_key = isBareKey(key_name);
            if (bare_key) {
                try self.underlying_writer.writeAll(key_name);
                return;
            }

            try self.underlying_writer.writeAll("\"");
            try self.writeStringRaw(key_name);
            try self.underlying_writer.writeAll("\"");
        }

        fn writeDelimeter(self: *WriteStreamT) !void {
            if (self.stack.items.len == 0) {
                defer self.start = false;
                if (!self.start) try self.underlying_writer.writeAll(switch (options.newlines) {
                    .crlf => "\r\n",
                    .lf => "\n",
                });
                return;
            }
            try self.underlying_writer.writeAll(switch (self.stack.getLast()) {
                inline .array, .inline_table => |container| if (container.expect_delimeter) ", " else return,
                .key_pair => " = ",
            });
        }

        fn finishValue(self: *WriteStreamT) void {
            if (self.stack.items.len == 0) return;
            switch (self.stack.items[self.stack.items.len - 1]) {
                inline .array, .inline_table => |*container| {
                    container.expect_delimeter = true;
                },
                .key_pair => {
                    _ = self.stack.pop();
                    self.finishValue();
                },
            }
        }

        pub fn writeDeepKeyPairRaw(self: *WriteStreamT, key_parts: []const []const u8) !void {
            for (0.., key_parts) |i, key_part| {
                if (i > 0) {
                    try self.underlying_writer.writeAll(".");
                }
                try self.writeKeyRaw(key_part);
            }
        }

        pub fn beginDeepKeyPair(self: *WriteStreamT, key_parts: []const []const u8) !void {
            self.assertCanWriteKey();
            try self.writeDelimeter();
            try self.writeDeepKeyPairRaw(key_parts);
            try self.stack.append(self.allocator, .key_pair);
        }

        pub fn beginKeyPair(self: *WriteStreamT, key_name: []const u8) !void {
            try self.beginDeepKeyPair(&.{key_name});
        }

        pub fn writeString(self: *WriteStreamT, string: []const u8) !void {
            self.assertCanWriteValue();
            try self.writeDelimeter();
            try self.underlying_writer.writeAll("\"");
            try self.writeStringRaw(string);
            try self.underlying_writer.writeAll("\"");
            self.finishValue();
        }

        pub fn writeInteger(self: *WriteStreamT, int: i64) !void {
            self.assertCanWriteValue();
            try self.writeDelimeter();
            try self.underlying_writer.print("{d}", .{int});
            self.finishValue();
        }

        pub fn writeFloat(self: *WriteStreamT, float: f64) !void {
            self.assertCanWriteValue();
            try self.writeDelimeter();
            var buf: [std.fmt.format_float.bufferSize(options.format_float_options.mode, f64)]u8 = undefined;
            const float_string = try std.fmt.formatFloat(&buf, float, options.format_float_options);
            try self.underlying_writer.writeAll(float_string);
            self.finishValue();
        }

        pub fn writeBoolean(self: *WriteStreamT, boolean: bool) !void {
            self.assertCanWriteValue();
            try self.writeDelimeter();
            try self.underlying_writer.writeAll(if (boolean) "true" else "false");
            self.finishValue();
        }

        pub fn writeDateTime(self: *WriteStreamT, date_time: parse.Value.DateTime) !void {
            self.assertCanWriteValue();
            try self.writeDelimeter();
            if (date_time.date) |date| {
                try self.underlying_writer.writeAll(date);
            }
            if (date_time.time) |time| {
                if (date_time.date != null) try self.underlying_writer.writeAll(switch (options.date_time_separator) {
                    .space => " ",
                    .t => "T",
                });
                try self.underlying_writer.writeAll(time);
            }
            if (date_time.date != null and date_time.time != null) {
                if (date_time.offset) |offset| {
                    try self.underlying_writer.writeAll(offset);
                }
            }
            self.finishValue();
        }

        pub fn beginArray(self: *WriteStreamT) !void {
            self.assertCanWriteValue();
            try self.writeDelimeter();
            try self.underlying_writer.writeAll("[ ");
            try self.stack.append(self.allocator, .{ .array = .{} });
        }

        pub fn endArray(self: *WriteStreamT) !void {
            const last_nest = self.stack.pop();
            std.debug.assert(last_nest != null and last_nest.? == .array);
            if (last_nest.?.array.expect_delimeter) try self.underlying_writer.writeAll(" ");
            try self.underlying_writer.writeAll("]");
            self.finishValue();
        }

        pub fn arrayLine(self: *WriteStreamT) !void {
            std.debug.assert(self.stack.items.len != 0 and self.stack.getLast() == .array);
            self.assertCanWriteValue();
            try self.writeDelimeter();
        }

        pub fn beginInlineTable(self: *WriteStreamT) !void {
            self.assertCanWriteValue();
            try self.writeDelimeter();
            try self.underlying_writer.writeAll("{ ");
            try self.stack.append(self.allocator, .{ .inline_table = .{} });
        }

        pub fn endInlineTable(self: *WriteStreamT) !void {
            const last_nest = self.stack.pop();
            std.debug.assert(last_nest != null and last_nest.? == .inline_table);
            if (last_nest.?.inline_table.expect_delimeter) try self.underlying_writer.writeAll(" ");
            try self.underlying_writer.writeAll("}");
            self.finishValue();
        }

        pub fn writeDeepTable(self: *WriteStreamT, key_parts: []const []const u8) !void {
            std.debug.assert(self.stack.items.len == 0);
            try self.writeDelimeter();
            try self.writeDelimeter();
            try self.underlying_writer.writeAll("[");
            try self.writeDeepKeyPairRaw(key_parts);
            try self.underlying_writer.writeAll("]");
        }

        pub fn writeTable(self: *WriteStreamT, key_name: []const u8) !void {
            try self.writeDeepTable(&.{key_name});
        }

        pub fn writeDeepManyTable(self: *WriteStreamT, key_parts: []const []const u8) !void {
            std.debug.assert(self.stack.items.len == 0);
            try self.writeDelimeter();
            try self.writeDelimeter();
            try self.underlying_writer.writeAll("[[");
            try self.writeDeepKeyPairRaw(key_parts);
            try self.underlying_writer.writeAll("]]");
        }

        pub fn writeManyTable(self: *WriteStreamT, key_name: []const u8) !void {
            try self.writeDeepManyTable(&.{key_name});
        }
    };
}

test WriteStream {
    var dynamic_buffer: std.ArrayListUnmanaged(u8) = .empty;
    defer dynamic_buffer.deinit(std.testing.allocator);

    const writer = dynamic_buffer.writer(std.testing.allocator);

    var write_stream: WriteStream(std.ArrayListUnmanaged(u8).Writer, .{
        .unicode_full_escape_strings = true,
    }) = .{
        .underlying_writer = writer,
        .allocator = std.testing.allocator,
    };
    defer write_stream.deinit();

    try write_stream.beginKeyPair("barney");
    try write_stream.beginInlineTable();
    {
        try write_stream.beginKeyPair("first_name");
        try write_stream.writeString("Barney");
        try write_stream.beginKeyPair("colours");
        try write_stream.beginArray();
        {
            try write_stream.writeString("brown");
            try write_stream.writeString("red");
            try write_stream.writeString("white");
        }
        try write_stream.endArray();
    }
    try write_stream.endInlineTable();

    try write_stream.writeTable("lala");
    try write_stream.beginKeyPair("first_name");
    try write_stream.writeString("Lala");

    try write_stream.writeDeepManyTable(&.{ "lala", "colours" });
    try write_stream.beginKeyPair("face");
    try write_stream.writeString("black");
    try write_stream.beginKeyPair("body");
    try write_stream.writeString("black");
    try write_stream.beginKeyPair("tail");
    try write_stream.writeString("black");
    try write_stream.beginKeyPair("legs");
    try write_stream.writeString("black");

    try std.testing.expectEqualSlices(u8,
        \\barney = { first_name = "Barney", colours = [ "brown", "red", "white" ] }
        \\
        \\[lala]
        \\first_name = "Lala"
        \\
        \\[[lala.colours]]
        \\face = "black"
        \\body = "black"
        \\tail = "black"
        \\legs = "black"
    , dynamic_buffer.items);
}
