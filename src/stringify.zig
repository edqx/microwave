const std = @import("std");

const parse = @import("parse.zig");
const write_stream = @import("write_stream.zig");

pub fn tableContainsNormalKeys(table: parse.Value.Table) bool {
    var entries = table.iterator();
    return while (entries.next()) |entry| {
        switch (entry.value_ptr.*) {
            .none => unreachable,
            .table, .array_of_tables => {},
            .array, .string, .integer, .float, .boolean, .date_time => break true,
        }
    } else false;
}

pub fn structContainsNormalKeys(val: anytype) bool {
    return inline for (@typeInfo(@TypeOf(val)).@"struct".fields) |field| {
        if (field.type == parse.Value) {
            switch (@field(val, field.name)) {
                .none => unreachable,
                .table, .array_of_tables => {},
                .array, .string, .integer, .float, .boolean, .date_time => break true,
            }
            continue;
        }
        if (field.type == []const u8 or
            field.type == i64 or
            field.type == f64 or
            field.type == bool or
            field.type == parse.Value.DateTime)
        {
            break true;
        }
    } else false;
}

pub fn Stringify(WriteStreamType: type) type {
    return struct {
        const StringifyT = @This();

        stream: WriteStreamType,

        key_allocator: std.mem.Allocator,
        key_stack: std.ArrayListUnmanaged([]const u8) = .empty,

        pub fn deinit(self: *StringifyT) void {
            self.key_stack.deinit(self.key_allocator);
        }

        pub fn writeInlineValue(self: *StringifyT, value: parse.Value) !void {
            switch (value) {
                .none => unreachable,
                .table => |table_value| {
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
                            .array_of_tables => .{ .table = elem },
                            .array => elem,
                            else => unreachable,
                        });
                    }
                    try self.stream.endArray();
                },
                .string => |string_value| try self.stream.writeString(string_value),
                .integer => |int_value| try self.stream.writeInteger(int_value),
                .float => |float_value| try self.stream.writeFloat(float_value),
                .boolean => |bool_value| try self.stream.writeBoolean(bool_value),
                .date_time => |date_time_value| try self.stream.writeDateTime(date_time_value),
            }
        }

        pub fn writeTableKeys(self: *StringifyT, table: parse.Value.Table) !void {
            var entries1 = table.iterator();
            while (entries1.next()) |entry| {
                switch (entry.value_ptr.*) {
                    .none => unreachable,
                    .table, .array_of_tables => {},
                    .array, .string, .integer, .float, .boolean, .date_time => {
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
                    .none => unreachable,
                    .array, .string, .integer, .float, .boolean, .date_time => {},
                    .table => |table_value| {
                        if (table_value.count() == 0 or tableContainsNormalKeys(table_value)) {
                            try self.stream.writeDeepTable(self.key_stack.items);
                        }
                        try self.writeTableKeys(table_value);
                    },
                    .array_of_tables => |many_tables| {
                        for (many_tables.items) |many_table_value| {
                            try self.stream.writeDeepManyTable(self.key_stack.items);
                            try self.writeTableKeys(many_table_value);
                        }
                    },
                }
            }
        }

        pub fn writeInline(self: *StringifyT, val: anytype) !void {
            const val_type = @TypeOf(val);
            const type_info = @typeInfo(val_type);
            if (val_type == parse.Value.Table) {
                try self.writeInlineValue(.{ .table = val });
            } else if (val_type == parse.Value) {
                try self.writeInlineValue(val);
            } else if (val_type == []const u8) {
                try self.stream.writeString(val);
            } else if (val_type == i64) {
                try self.stream.writeInteger(val);
            } else if (val_type == f64) {
                try self.stream.writeFloat(val);
            } else if (val_type == bool) {
                try self.stream.writeBoolean(val);
            } else if (val_type == parse.Value.DateTime) {
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

        pub fn writeFields(self: *StringifyT, val: anytype) !void {
            inline for (@typeInfo(@TypeOf(val)).@"struct".fields) |struct_field| {
                const field_type_info = @typeInfo(struct_field.type);
                if (struct_field.type == []const u8 or
                    struct_field.type == i64 or
                    struct_field.type == f64 or
                    struct_field.type == bool or
                    struct_field.type == parse.Value.DateTime or
                    struct_field.type == parse.Value.Table or
                    struct_field.type == parse.Value or
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
    };
}

pub fn stringifyTableToStream(temp_allocator: std.mem.Allocator, root_table: parse.Value.Table, stream: anytype) !void {
    var stringifier: Stringify(@TypeOf(stream)) = .{ .key_allocator = temp_allocator, .stream = stream };
    defer stringifier.deinit();

    try stringifier.writeTableKeys(root_table);
}

pub fn stringifyTable(temp_allocator: std.mem.Allocator, root_table: parse.Value.Table, writer: anytype) !void {
    var stream: write_stream.Stream(@TypeOf(writer), .{}) = .{
        .allocator = temp_allocator,
        .underlying_writer = writer,
    };
    defer stream.deinit();

    try stringifyTableToStream(temp_allocator, root_table, &stream);
}

pub fn stringifyToStream(temp_allocator: std.mem.Allocator, val: anytype, stream: anytype) !void {
    var stringifier: Stringify(@TypeOf(stream)) = .{ .key_allocator = temp_allocator, .stream = stream };
    defer stringifier.deinit();

    try stringifier.writeFields(val);
}

pub fn stringify(temp_allocator: std.mem.Allocator, val: anytype, writer: anytype) !void {
    var stream: write_stream.Stream(@TypeOf(writer), .{}) = .{
        .allocator = temp_allocator,
        .underlying_writer = writer,
    };
    defer stream.deinit();

    try stringifyToStream(temp_allocator, val, &stream);
}

test stringifyTable {
    var dynamic_buffer: std.ArrayListUnmanaged(u8) = .empty;
    defer dynamic_buffer.deinit(std.testing.allocator);

    var table = try parse.fromSliceOwned(std.testing.allocator,
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
    defer parse.deinitTable(std.testing.allocator, &table);

    try stringifyTable(std.testing.allocator, table, dynamic_buffer.writer(std.testing.allocator));

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
    , dynamic_buffer.items);
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

    other_info: parse.Value,
};

test stringify {
    var dynamic_buffer: std.ArrayListUnmanaged(u8) = .empty;
    defer dynamic_buffer.deinit(std.testing.allocator);

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

    try stringify(std.testing.allocator, val, dynamic_buffer.writer(std.testing.allocator));
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
    , dynamic_buffer.items);
}
