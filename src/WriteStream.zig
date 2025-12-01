const std = @import("std");
const DateTime = @import("rfc3339.zig").DateTime;

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
    format_float_options: std.fmt.float.Options = .{},
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

const WriteStream = @This();

allocator: std.mem.Allocator,

writer: *std.Io.Writer,
options: Options = .{},

stack: std.ArrayListUnmanaged(NestingKind) = .empty,
start: bool = true,

fn assertCanWriteKey(self: *WriteStream) void {
    std.debug.assert(self.stack.items.len == 0 or self.stack.getLast() == .inline_table);
}

fn assertCanWriteValue(self: *WriteStream) void {
    std.debug.assert(self.stack.items.len != 0 and switch (self.stack.getLast()) {
        .array, .key_pair => true,
        .inline_table => false,
    });
}

pub fn deinit(self: *WriteStream) void {
    self.stack.deinit(self.allocator);
}

pub fn writeStringRaw(self: *WriteStream, string: []const u8, multiline: bool) !void {
    if (self.options.unicode_full_escape_strings) {
        const utf8_view = try std.unicode.Utf8View.init(string);
        var codepoints = utf8_view.iterator();
        while (codepoints.nextCodepoint()) |codepoint| {
            switch (codepoint) {
                std.ascii.control_code.bs => try self.writer.writeAll("\\b"),
                '\t' => try self.writer.writeAll(if (multiline) "\t" else "\\t"),
                '\n' => try self.writer.writeAll(if (multiline) "\n" else "\\n"),
                std.ascii.control_code.ff => try self.writer.writeAll("\\f"),
                '\r' => try self.writer.writeAll(if (multiline) "\r" else "\\r"),
                '"' => try self.writer.writeAll("\\\""),
                '\\' => try self.writer.writeAll("\\"),
                ' ', '#'...'[', ']'...'~' => try self.writer.writeByte(@as(u8, @intCast(codepoint))),
                else => {
                    try self.writer.print("\\u{d:0>8}", .{codepoint});
                },
            }
        }
        return;
    }
    for (string) |char| {
        try self.writer.writeAll(switch (char) {
            std.ascii.control_code.bs => "\\b",
            '\t' => if (multiline) "\t" else "\\t",
            '\n' => if (multiline) "\n" else "\\n",
            std.ascii.control_code.ff => "\\f",
            '\r' => if (multiline) "\r" else "\\r",
            '"' => "\\\"",
            '\\' => "\\",
            else => &.{char},
        });
    }
}

pub fn writeKeyRaw(self: *WriteStream, key_name: []const u8) !void {
    const bare_key = isBareKey(key_name);
    if (bare_key) {
        try self.writer.writeAll(key_name);
        return;
    }

    try self.writer.writeAll("\"");
    try self.writeStringRaw(key_name, false);
    try self.writer.writeAll("\"");
}

fn writeDelimeter(self: *WriteStream) !void {
    if (self.stack.items.len == 0) {
        defer self.start = false;
        if (!self.start) try self.writer.writeAll(switch (self.options.newlines) {
            .crlf => "\r\n",
            .lf => "\n",
        });
        return;
    }
    try self.writer.writeAll(switch (self.stack.getLast()) {
        inline .array, .inline_table => |container| if (container.expect_delimeter) ", " else return,
        .key_pair => " = ",
    });
}

fn finishValue(self: *WriteStream) void {
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

pub fn writeDeepKeyPairRaw(self: *WriteStream, key_parts: []const []const u8) !void {
    for (0.., key_parts) |i, key_part| {
        if (i > 0) {
            try self.writer.writeAll(".");
        }
        try self.writeKeyRaw(key_part);
    }
}

pub fn beginDeepKeyPair(self: *WriteStream, key_parts: []const []const u8) !void {
    self.assertCanWriteKey();
    try self.writeDelimeter();
    try self.writeDeepKeyPairRaw(key_parts);
    try self.stack.append(self.allocator, .key_pair);
}

pub fn beginKeyPair(self: *WriteStream, key_name: []const u8) !void {
    try self.beginDeepKeyPair(&.{key_name});
}

pub fn writeString(self: *WriteStream, string: []const u8) !void {
    self.assertCanWriteValue();
    try self.writeDelimeter();
    try self.writer.writeAll("\"");
    try self.writeStringRaw(string, false);
    try self.writer.writeAll("\"");
    self.finishValue();
}

pub fn writeMultilineString(self: *WriteStream, string: []const u8) !void {
    self.assertCanWriteValue();
    try self.writeDelimeter();
    try self.writer.writeAll("\"\"\"\n");
    try self.writeStringRaw(string, true);
    try self.writer.writeAll("\"\"\"");
    self.finishValue();
}

pub fn writeInteger(self: *WriteStream, int: i64) !void {
    self.assertCanWriteValue();
    try self.writeDelimeter();
    try self.writer.print("{d}", .{int});
    self.finishValue();
}

pub fn writeFloat(self: *WriteStream, float: f64) !void {
    self.assertCanWriteValue();
    try self.writeDelimeter();
    switch (self.options.format_float_options.mode) {
        inline else => |mode| {
            var buf: [std.fmt.float.bufferSize(mode, f64)]u8 = undefined;
            const float_string = try std.fmt.float.render(&buf, float, self.options.format_float_options);
            try self.writer.writeAll(float_string);
            self.finishValue();
        },
    }
}

pub fn writeBoolean(self: *WriteStream, boolean: bool) !void {
    self.assertCanWriteValue();
    try self.writeDelimeter();
    try self.writer.writeAll(if (boolean) "true" else "false");
    self.finishValue();
}

fn writeDate(self: *WriteStream, date: DateTime.Date) !void {
    try self.writer.print("{d:0>4}-{d:0>2}-{d:0>2}", .{ date.year, date.month, date.day });
}

fn writeTime(self: *WriteStream, time: DateTime.Time) !void {
    try self.writer.print("{d:0>2}:{d:0>2}", .{ time.hour, time.minute });
    try self.writer.print(":{d:0>2}", .{time.second});
    if (time.millisecond) |millisecond| {
        try self.writer.print(".{d:0<3}", .{millisecond});
    }
}

fn writeDateTimeSeparator(self: *WriteStream) !void {
    try self.writer.writeAll(switch (self.options.date_time_separator) {
        .space => " ",
        .t => "T",
    });
}

pub fn writeDateTime(self: *WriteStream, date_time: DateTime) !void {
    self.assertCanWriteValue();
    try self.writeDelimeter();
    switch (date_time) {
        .just_date => |date| {
            try self.writeDate(date);
        },
        .just_time => |time| {
            try self.writeTime(time);
        },
        .local_date_time => |both| {
            try self.writeDate(both.date);
            try self.writeDateTimeSeparator();
            try self.writeTime(both.time);
        },
        .offset_date_time => |all| {
            try self.writeDate(all.date);
            try self.writeDateTimeSeparator();
            try self.writeTime(all.time);
            if (all.offset.isUtc()) {
                try self.writer.writeAll("Z");
            } else {
                try self.writer.print("{s}{d:0>2}:{d:0>2}", .{
                    if (all.offset.negative) "-" else "+",
                    all.offset.hour,
                    all.offset.minute,
                });
            }
        },
    }
    self.finishValue();
}

pub fn beginArray(self: *WriteStream) !void {
    self.assertCanWriteValue();
    try self.writeDelimeter();
    try self.writer.writeAll("[ ");
    try self.stack.append(self.allocator, .{ .array = .{} });
}

pub fn endArray(self: *WriteStream) !void {
    const last_nest = self.stack.pop();
    std.debug.assert(last_nest != null and last_nest.? == .array);
    if (last_nest.?.array.expect_delimeter) try self.writer.writeAll(" ");
    try self.writer.writeAll("]");
    self.finishValue();
}

pub fn arrayLine(self: *WriteStream) !void {
    std.debug.assert(self.stack.items.len != 0 and self.stack.getLast() == .array);
    self.assertCanWriteValue();
    try self.writeDelimeter();
}

pub fn beginInlineTable(self: *WriteStream) !void {
    self.assertCanWriteValue();
    try self.writeDelimeter();
    try self.writer.writeAll("{ ");
    try self.stack.append(self.allocator, .{ .inline_table = .{} });
}

pub fn endInlineTable(self: *WriteStream) !void {
    const last_nest = self.stack.pop();
    std.debug.assert(last_nest != null and last_nest.? == .inline_table);
    if (last_nest.?.inline_table.expect_delimeter) try self.writer.writeAll(" ");
    try self.writer.writeAll("}");
    self.finishValue();
}

pub fn writeDeepTable(self: *WriteStream, key_parts: []const []const u8) !void {
    std.debug.assert(self.stack.items.len == 0);
    try self.writeDelimeter();
    try self.writeDelimeter();
    try self.writer.writeAll("[");
    try self.writeDeepKeyPairRaw(key_parts);
    try self.writer.writeAll("]");
}

pub fn writeTable(self: *WriteStream, key_name: []const u8) !void {
    try self.writeDeepTable(&.{key_name});
}

pub fn writeDeepManyTable(self: *WriteStream, key_parts: []const []const u8) !void {
    std.debug.assert(self.stack.items.len == 0);
    try self.writeDelimeter();
    try self.writeDelimeter();
    try self.writer.writeAll("[[");
    try self.writeDeepKeyPairRaw(key_parts);
    try self.writer.writeAll("]]");
}

pub fn writeManyTable(self: *WriteStream, key_name: []const u8) !void {
    try self.writeDeepManyTable(&.{key_name});
}

test WriteStream {
    var allocating_writer: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer allocating_writer.deinit();

    var write_stream: WriteStream = .{
        .writer = &allocating_writer.writer,
        .allocator = std.testing.allocator,
        .options = .{
            .unicode_full_escape_strings = true,
        },
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
    , allocating_writer.written());
}
