const std = @import("std");
const parse = @import("parse.zig");

pub fn Populate(Container: type) type {
    const type_info = @typeInfo(Container);
    return struct {
        pub const Error = error{ IncorrectType, MissingKey };

        pub fn deinitRecursive(allocator: std.mem.Allocator, val: *Container) void {
            if (Container == parse.Value) {
                val.deinitRecursive(allocator);
            } else if (Container == parse.Value.Table) {
                parse.deinitTable(allocator, val);
            } else if (Container == parse.Value.DateTime) {
                val.deinit(allocator);
            } else if (type_info == .@"struct") {
                comptime var i = type_info.@"struct".fields.len;
                inline while (i > 0) {
                    i -= 1;
                    const field = type_info.@"struct".fields[i];
                    Populate(field.type).deinitRecursive(allocator, &@field(val, field.name));
                }
            } else if (Container == []const u8) {
                allocator.free(val.*);
            } else if (type_info == .pointer and type_info.pointer.size == .slice) {
                var i: usize = val.len;
                while (i > 0) {
                    i -= 1;
                    Populate(type_info.pointer.child).deinitRecursive(allocator, &val.*[i]);
                }
                allocator.free(val.*);
            } else if (Container == i64 or Container == f64 or Container == bool) {
                //
            } else @compileError("Cannot de-initialise container of type " ++ @typeName(Container));
        }

        pub fn intoFromValueOwned(allocator: std.mem.Allocator, destination: *Container, value: parse.Value) !void {
            if (Container == parse.Value) {
                destination.* = try value.dupeRecursive(allocator);
                return;
            }
            if (type_info == .@"union") {
                inline for (type_info.@"union".fields) |field| {
                    var field_dest: @FieldType(Container, field.name) = undefined;
                    var success = true;
                    Populate(@FieldType(Container, field.name)).intoFromValueOwned(allocator, &field_dest, value) catch |e| switch (e) {
                        Error.IncorrectType, Error.MissingKey => {
                            success = false;
                        },
                        else => return e,
                    };
                    if (success) {
                        destination.* = @unionInit(Container, field.name, field_dest);
                        return;
                    }
                }
                return Error.IncorrectType;
            }
            switch (value) {
                .none => unreachable,
                .table => |table_value| {
                    if (Container == parse.Value.Table) {
                        const duped = try value.dupeRecursive(allocator);
                        errdefer duped.deinitRecursive(allocator);
                        destination.* = duped.table;
                        return;
                    }
                    if (type_info != .@"struct") return Error.IncorrectType;
                    var field_idx: usize = 0;
                    errdefer for (0..field_idx) |i| {
                        inline for (0.., type_info.@"struct".fields) |j, field| {
                            if (i == j) {
                                Populate(field.type).deinitRecursive(allocator, &@field(destination, field.name));
                            }
                        }
                    };
                    inline for (type_info.@"struct".fields) |field| {
                        const child_value = table_value.get(field.name) orelse {
                            if (@typeInfo(field.type) == .optional) {
                                @field(destination, field.name) = null;
                                continue;
                            }
                            return Error.MissingKey;
                        };
                        try Populate(field.type).intoFromValueOwned(allocator, &@field(destination, field.name), child_value);
                        field_idx += 1;
                    }
                },
                inline .array, .array_of_tables => |array_value, tag| {
                    if (type_info != .pointer or type_info.pointer.size != .slice) return Error.IncorrectType;
                    if (Container == []const u8) return Error.IncorrectType;
                    var result: std.ArrayListUnmanaged(type_info.pointer.child) = try .initCapacity(allocator, array_value.items.len);
                    errdefer result.deinit(allocator);
                    errdefer for (result.items) |*elem| {
                        Populate(type_info.pointer.child).deinitRecursive(allocator, elem);
                    };
                    for (array_value.items) |inner_value| {
                        var rest: type_info.pointer.child = undefined;
                        try Populate(type_info.pointer.child).intoFromValueOwned(allocator, &rest, switch (tag) {
                            .array => inner_value,
                            .array_of_tables => .{ .table = inner_value },
                            else => unreachable,
                        });
                        errdefer Populate(type_info.pointer.child).deinitRecursive(allocator, rest);
                        result.appendAssumeCapacity(rest);
                    }
                    destination.* = try result.toOwnedSlice(allocator);
                },
                .string => |string_value| {
                    if (Container != []const u8) return Error.IncorrectType;
                    destination.* = try allocator.dupe(u8, string_value);
                },
                .integer => |integer_value| {
                    if (Container == i64) {
                        destination.* = integer_value;
                    } else if (Container == f64) {
                        destination.* = @floatFromInt(integer_value);
                    } else return Error.IncorrectType;
                },
                .float => |float_value| {
                    if (Container != f64) return Error.IncorrectType;
                    destination.* = float_value;
                },
                .boolean => |bool_value| {
                    if (Container != bool) return Error.IncorrectType;
                    destination.* = bool_value;
                },
                .date_time => |date_time_value| {
                    if (Container != parse.Value.DateTime) return Error.IncorrectType;
                    destination.* = try date_time_value.dupe(allocator);
                },
            }
        }

        pub fn intoFromTableOwned(allocator: std.mem.Allocator, destination: *Container, table: parse.Value.Table) !void {
            try intoFromValueOwned(allocator, destination, .{ .table = table });
        }

        pub fn intoFromSliceOwned(allocator: std.mem.Allocator, destination: *Container, slice: []const u8) !void {
            var table = try parse.fromSliceOwned(allocator, slice);
            defer table.deinit(allocator);
            try intoFromTableOwned(allocator, destination, table);
        }

        pub fn intoFromReaderOwned(allocator: std.mem.Allocator, destination: *Container, reader: anytype) !void {
            var table = try parse.fromReaderOwned(allocator, reader);
            defer table.deinit(allocator);
            try intoFromTableOwned(allocator, destination, table);
        }

        //
        pub fn createFromValueOwned(allocator: std.mem.Allocator, value: parse.Value) !Container {
            var out: Container = undefined;
            try intoFromValueOwned(allocator, &out, value);
            return out;
        }

        pub fn createFromTableOwned(allocator: std.mem.Allocator, table: parse.Value.Table) !Container {
            var out: Container = undefined;
            try intoFromTableOwned(allocator, &out, table);
            return out;
        }

        pub fn createFromSliceOwned(allocator: std.mem.Allocator, slice: []const u8) !Container {
            var out: Container = undefined;
            try intoFromSliceOwned(allocator, &out, slice);
            return out;
        }

        pub fn createFromReaderOwned(allocator: std.mem.Allocator, reader: anytype) !Container {
            var out: Container = undefined;
            try intoFromReaderOwned(allocator, &out, reader);
            return out;
        }

        //
        pub const Document = struct {
            value: Container,
            arena: std.heap.ArenaAllocator,

            pub fn deinit(self: Document) void {
                self.arena.deinit();
            }
        };

        pub fn createFromValue(allocator: std.mem.Allocator, value: parse.Value) !Document {
            var arena = std.heap.ArenaAllocator.init(allocator);
            errdefer arena.deinit();
            return .{
                .value = try createFromValueOwned(arena.allocator(), value),
                .arena = arena,
            };
        }

        pub fn createFromTable(allocator: std.mem.Allocator, table: parse.Value.Table) !Document {
            var arena = std.heap.ArenaAllocator.init(allocator);
            errdefer arena.deinit();
            return .{
                .value = try createFromTableOwned(arena.allocator(), table),
                .arena = arena,
            };
        }

        pub fn createFromSlice(allocator: std.mem.Allocator, slice: []const u8) !Document {
            var arena = std.heap.ArenaAllocator.init(allocator);
            errdefer arena.deinit();
            return .{
                .value = try createFromSliceOwned(arena.allocator(), slice),
                .arena = arena,
            };
        }

        pub fn createFromReader(allocator: std.mem.Allocator, reader: anytype) !Document {
            var arena = std.heap.ArenaAllocator.init(allocator);
            errdefer arena.deinit();
            return .{
                .value = try createFromReaderOwned(arena.allocator(), reader),
                .arena = arena,
            };
        }
    };
}

const TestDog = struct {
    const Friend = struct {
        name: []const u8,
    };

    name: []const u8,
    breed: []const u8,
    age: i64,

    friends: []Friend,

    any: parse.Value.Table,
};

test Populate {
    const buf =
        \\name = "Barney"
        \\breed = "unknown"
        \\age = 16
        \\
        \\any = {a=[{},{},{},{a=[[[[{b=[],c={d=[]}}]]]]}]}
        \\
        \\[[friends]]
        \\name = "Bo"
        \\
        \\[[friends]]
        \\name = "Lala"
    ;

    const doc = try parse.fromSlice(std.testing.allocator, buf);
    defer doc.deinit();

    var test_struct: TestDog = undefined;
    try Populate(TestDog).intoFromValueOwned(std.testing.allocator, &test_struct, .{ .table = doc.root_table });
    defer Populate(TestDog).deinitRecursive(std.testing.allocator, &test_struct);

    try std.testing.expectEqualSlices(u8, "Barney", test_struct.name);
    try std.testing.expectEqualSlices(u8, "unknown", test_struct.breed);
    try std.testing.expectEqual(16, test_struct.age);
    try std.testing.expectEqualSlices(u8, "Bo", test_struct.friends[0].name);
    try std.testing.expectEqualSlices(u8, "Lala", test_struct.friends[1].name);
}

const Animal = union(enum) {
    dog: struct {
        name: []const u8,
        breed: []const u8,
    },
    cat: struct {
        name: []const u8,
        number_of_colours: i64,
    },
};

test "Populate with union for disjunction types" {
    const animal1 = try Populate(Animal).createFromSlice(std.testing.allocator,
        \\name = "Barney"
        \\breed = "unknown"
    );
    defer animal1.deinit();

    try std.testing.expect(animal1.value == .dog);
    try std.testing.expectEqualSlices(u8, "Barney", animal1.value.dog.name);
    try std.testing.expectEqualSlices(u8, "unknown", animal1.value.dog.breed);

    const animal2 = try Populate(Animal).createFromSlice(std.testing.allocator,
        \\name = "Whitepaws"
        \\number_of_colours = 2
    );
    defer animal2.deinit();

    try std.testing.expect(animal2.value == .cat);
    try std.testing.expectEqualSlices(u8, "Whitepaws", animal2.value.cat.name);
    try std.testing.expectEqual(2, animal2.value.cat.number_of_colours);
}
