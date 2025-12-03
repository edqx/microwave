const std = @import("std");

const Parser = @import("Parser.zig");
const DateTime = @import("rfc3339.zig").DateTime;
const WriteStream = @import("WriteStream.zig");

const microwave = @import("microwave.zig");

const Stringify = @This();

stream: *WriteStream,

key_allocator: std.mem.Allocator,
key_stack: std.ArrayListUnmanaged([]const u8) = .empty,

pub fn deinit(self: *Stringify) void {
    self.key_stack.deinit(self.key_allocator);
}

fn writeInlineString(self: *Stringify, string: []const u8) !void {
    const contains_newline = std.mem.indexOfAny(u8, string, "\r\n") != null;
    if (contains_newline) {
        try self.stream.writeMultilineString(string);
    } else {
        try self.stream.writeString(string);
    }
}

fn writeInlineValue(self: *Stringify, value: Parser.Value) !void {
    switch (value) {
        .table => unreachable,
        inline .inline_table, .implicit_table => |table_value| {
            try self.stream.beginInlineTable();
            var entries = table_value.iterator();
            while (entries.next()) |entry| {
                try self.stream.beginKeyPair(entry.key_ptr.*);
                try self.writeInlineValue(entry.value_ptr.*);
            }
            try self.stream.endInlineTable();
        },
        inline .array, .array_of_tables => |array_value, tag| {
            try self.stream.beginArray();
            for (array_value.items) |elem| {
                try self.writeInlineValue(switch (tag) {
                    .array_of_tables => elem,
                    .array => elem,
                    else => unreachable,
                });
            }
            try self.stream.endArray();
        },
        .string, .integer_string => |string_value| try self.writeInlineString(string_value),
        .integer => |int_value| try self.stream.writeInteger(int_value),
        .float => |float_value| try self.stream.writeFloat(float_value),
        .bool => |bool_value| try self.stream.writeBoolean(bool_value),
        .datetime => |datetime_value| try self.stream.writeDateTime(datetime_value),
    }
}

fn writeTableKeys(self: *Stringify, table: Parser.Value.Table) !void {
    var entries1 = table.iterator();
    while (entries1.next()) |entry| {
        switch (entry.value_ptr.*) {
            .inline_table, .implicit_table, .table, .array_of_tables => {},
            .array, .string, .integer, .integer_string, .float, .bool, .datetime => {
                try self.stream.beginKeyPair(entry.key_ptr.*);
                try self.writeInlineValue(entry.value_ptr.*);
            },
        }
    }

    var entries2 = table.iterator();
    while (entries2.next()) |entry| {
        try self.key_stack.append(self.key_allocator, entry.key_ptr.*);
        defer _ = self.key_stack.pop();

        switch (entry.value_ptr.*) {
            .array, .string, .integer, .integer_string, .float, .bool, .datetime => {},
            .inline_table, .implicit_table, .table => |table_value| {
                if (table_value.count() == 0 or tableContainsNormalKeys(table_value)) {
                    try self.stream.writeDeepTable(self.key_stack.items);
                }
                try self.writeTableKeys(table_value);
            },
            .array_of_tables => |many_tables| {
                for (many_tables.items) |many_table_value| {
                    try self.stream.writeDeepManyTable(self.key_stack.items);
                    try self.writeTableKeys(many_table_value.table);
                }
            },
        }
    }
}

fn writeInline(self: *Stringify, val: anytype) !void {
    const val_type = @TypeOf(val);
    const type_info = @typeInfo(val_type);
    if (val_type == Parser.Value.Table) {
        try self.writeInlineValue(.{ .table = val });
    } else if (val_type == Parser.Value) {
        try self.writeInlineValue(val);
    } else if (val_type == []const u8) {
        try self.writeInlineString(val);
    } else if (val_type == i64) {
        try self.stream.writeInteger(val);
    } else if (val_type == f64) {
        try self.stream.writeFloat(val);
    } else if (val_type == bool) {
        try self.stream.writeBoolean(val);
    } else if (val_type == DateTime) {
        try self.stream.writeDateTime(val);
    } else if (type_info == .@"struct") {
        try self.stream.beginInlineTable();
        inline for (type_info.@"struct".fields) |struct_field| {
            try self.stream.beginKeyPair(struct_field.name);
            try self.writeInline(@field(val, struct_field.name));
        }
        try self.stream.endInlineTable();
    } else if (type_info == .pointer and type_info.pointer.size == .slice) {
        try self.stream.beginArray();
        for (val) |elem| {
            try self.writeInline(elem);
        }
        try self.stream.endArray();
    }
}

fn writeFields(self: *Stringify, val: anytype) !void {
    inline for (@typeInfo(@TypeOf(val)).@"struct".fields) |struct_field| {
        const field_type_info = @typeInfo(struct_field.type);
        if (struct_field.type == []const u8 or
            struct_field.type == i64 or
            struct_field.type == f64 or
            struct_field.type == bool or
            struct_field.type == DateTime or
            struct_field.type == Parser.Value.Table or
            struct_field.type == Parser.Value or
            (field_type_info == .pointer and
                field_type_info.pointer.size == .slice and
                @typeInfo(field_type_info.pointer.child) != .@"struct"))
        {
            try self.stream.beginKeyPair(struct_field.name);
            try self.writeInline(@field(val, struct_field.name));
        } else if (field_type_info == .@"struct") {
            //
        } else if (field_type_info == .pointer and
            field_type_info.pointer.size == .slice and
            @typeInfo(field_type_info.pointer.child) == .@"struct")
        {
            //
        } else @compileError("Cannot stringify type " ++ @typeName(struct_field.type) ++ " in " ++ @typeName(@TypeOf(val)));
    }

    inline for (@typeInfo(@TypeOf(val)).@"struct".fields) |struct_field| {
        try self.key_stack.append(self.key_allocator, struct_field.name);
        defer _ = self.key_stack.pop();

        const field_type_info = @typeInfo(struct_field.type);
        if (field_type_info == .@"struct") {
            if (field_type_info.@"struct".fields.len == 0 or structContainsNormalKeys(@field(val, struct_field.name))) {
                try self.stream.writeDeepTable(self.key_stack.items);
            }
            try self.writeFields(@field(val, struct_field.name));
        } else if (field_type_info == .pointer and
            field_type_info.pointer.size == .slice and
            @typeInfo(field_type_info.pointer.child) == .@"struct")
        {
            for (@field(val, struct_field.name)) |many_table_value| {
                try self.stream.writeDeepManyTable(self.key_stack.items);
                try self.writeFields(many_table_value);
            }
        }
    }
}

fn tableContainsNormalKeys(table: Parser.Value.Table) bool {
    var entries = table.iterator();
    return while (entries.next()) |entry| {
        switch (entry.value_ptr.*) {
            .inline_table, .implicit_table, .table, .array_of_tables => {},
            .array, .string, .integer, .integer_string, .float, .bool, .datetime => break true,
        }
    } else false;
}

fn structContainsNormalKeys(val: anytype) bool {
    return inline for (@typeInfo(@TypeOf(val)).@"struct".fields) |field| {
        if (field.type == Parser.Value) {
            switch (@field(val, field.name)) {
                .table, .array_of_tables => {},
                .array, .string, .integer, .float, .bool, .datetime => break true,
            }
            continue;
        }
        if (field.type == []const u8 or
            field.type == i64 or
            field.type == f64 or
            field.type == bool or
            field.type == Parser.Value.DateTime)
        {
            break true;
        }
    } else false;
}

pub fn writeTable(stringify: *Stringify, root_table: Parser.Value.Table) !void {
    try stringify.writeTableKeys(root_table);
}

pub fn write(stringify: *Stringify, val: anytype) !void {
    try stringify.writeFields(val);
}

test writeTable {
    var allocating_writer: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer allocating_writer.deinit();

    var document = try microwave.parseFromSlice(std.testing.allocator,
        \\valid_first_names = ["Barney", "Lala", "Bo", { name = "Jenny", dead = true }]
        \\
        \\[dog]
        \\barney.first_name = "Barney"
        \\barney.last_name = "Smale"
        \\barney.colours = ["brown", "red", "white"]
        \\
        \\[[other_dogs]]
        \\name = "Jenny"
        \\dead = true
        \\
        \\[[other_dogs.colours]]
        \\head = "white"
        \\body = "black"
        \\feet = "white"
    );
    defer document.deinit();

    var write_stream: WriteStream = .{
        .allocator = std.testing.allocator,
        .writer = &allocating_writer.writer,
    };
    defer write_stream.deinit();

    var stringify: Stringify = .{
        .key_allocator = std.testing.allocator,
        .stream = &write_stream,
    };
    defer stringify.deinit();

    try stringify.writeTable(document.table);

    try std.testing.expectEqualSlices(u8,
        \\valid_first_names = [ "Barney", "Lala", "Bo", { name = "Jenny", dead = true } ]
        \\
        \\[dog.barney]
        \\first_name = "Barney"
        \\last_name = "Smale"
        \\colours = [ "brown", "red", "white" ]
        \\
        \\[[other_dogs]]
        \\name = "Jenny"
        \\dead = true
        \\
        \\[[other_dogs.colours]]
        \\head = "white"
        \\body = "black"
        \\feet = "white"
    , allocating_writer.written());
}

const Dog = struct {
    const Friend = struct {
        const Relationship = struct {
            friendly: bool,
            difficult: bool,
        };

        name: []const u8,
        breed: []const u8,
        age: i64,

        relationship: Relationship,
    };

    name: []const u8,
    breed: []const u8,
    age: i64,

    colours: []const []const u8,
    friends: []const Friend,

    other_info: Parser.Value,
};

test write {
    var allocating_writer: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer allocating_writer.deinit();

    const val: Dog = .{
        .name = "Barney",
        .breed = "unknown",
        .age = 16,
        .colours = &.{ "brown", "white", "red" },
        .friends = &.{
            .{
                .name = "Lala",
                .breed = "unknown",
                .age = 1,
                .relationship = .{
                    .friendly = true,
                    .difficult = false,
                },
            },
        },
        .other_info = .{ .string = "really old" },
    };

    var write_stream: WriteStream = .{
        .allocator = std.testing.allocator,
        .writer = &allocating_writer.writer,
    };
    defer write_stream.deinit();

    var stringify: Stringify = .{
        .key_allocator = std.testing.allocator,
        .stream = &write_stream,
    };
    defer stringify.deinit();

    try stringify.write(val);

    try std.testing.expectEqualSlices(u8,
        \\name = "Barney"
        \\breed = "unknown"
        \\age = 16
        \\colours = [ "brown", "white", "red" ]
        \\other_info = "really old"
        \\
        \\[[friends]]
        \\name = "Lala"
        \\breed = "unknown"
        \\age = 1
        \\
        \\[friends.relationship]
        \\friendly = true
        \\difficult = false
    , allocating_writer.written());
}
