const std = @import("std");
const Parser = @import("Parser.zig");
const DateTime = @import("rfc3339.zig").DateTime;

const microwave = @import("microwave.zig");

fn getOptionalChild(T: type) type {
    const type_info = @typeInfo(T);
    return switch (type_info) {
        .optional => |optional| getOptionalChild(optional.child),
        else => T,
    };
}

/// Populate a Zig value with TOML data.
///
/// - `.array`, `.array_of_tables` is mapped to `[]T`
/// - `.table`, `.implicit_table`, `.inline_table` is mapped to `struct { ... }`
/// - `.bool` is mapped to `bool`
/// - `.string` is mapped to `[]const u8`
/// - `.integer_string` is mapped to `[]const u8`
/// - `.integer` is mapped to `i64` or `f64`
/// - `.float` is mapped to `f64`
/// - `.datetime` is mapped to `DateTime`
///
/// See `Parser.Value` for the definitions of these values.
pub fn Populate(
    /// The Zig container type to populate.
    Container: type,
) type {
    const type_info = @typeInfo(Container);
    return struct {
        pub const Error = error{ IncorrectType, MissingKey };

        /// De-initialize the data in this struct recursively.
        ///
        /// This is not a great function to use in your code, as it hides the actual
        /// allocated data in your struct. It is better to manually de-init and
        /// de-allocate the data yourself.
        pub fn deinitDeep(allocator: std.mem.Allocator, val: *Container) void {
            if (Container == DateTime) {
                //
            } else if (Container == Parser.Value) {
                val.deinitDeep(allocator);
            } else if (Container == Parser.Value.Table) {
                var value: Parser.Value = .{ .table = val.* };
                value.deinitDeep(allocator);
            } else if (type_info == .@"struct") {
                comptime var i = type_info.@"struct".fields.len;
                inline while (i > 0) {
                    i -= 1;
                    const field = type_info.@"struct".fields[i];
                    Populate(field.type).deinitDeep(allocator, &@field(val, field.name));
                }
            } else if (Container == []const u8) {
                allocator.free(val.*);
            } else if (type_info == .pointer and type_info.pointer.size == .slice) {
                var i: usize = val.len;
                while (i > 0) {
                    i -= 1;
                    Populate(type_info.pointer.child).deinitDeep(allocator, &val.*[i]);
                }
                allocator.free(val.*);
            } else if (type_info == .optional) {
                if (val.*) |*inner| Populate(type_info.optional.child).deinitDeep(allocator, inner);
            } else if (Container == i64 or Container == f64 or Container == bool) {
                //
            } else @compileError("Cannot de-initialise container of type " ++ @typeName(Container));
        }

        /// Populate a Zig type from a parsed TOML value.
        ///
        /// This is not guaranteed to be a fully safe and leak-free operation, but in most tested
        /// cases will be.
        ///
        /// Data can be de-initialized with `deinitDeep`.
        pub fn intoFromValueLeaky(allocator: std.mem.Allocator, destination: *Container, value: Parser.Value) !void {
            if (Container == Parser.Value) {
                destination.* = try value.dupeRecursive(allocator);
                return;
            }

            switch (type_info) {
                .@"union" => |union_info| {
                    if (Container != DateTime) {
                        inline for (union_info.fields) |field| {
                            var field_dest: @FieldType(Container, field.name) = undefined;
                            var success = true;
                            Populate(@FieldType(Container, field.name)).intoFromValueLeaky(allocator, &field_dest, value) catch |e| switch (e) {
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
                },
                .optional => |optional_info| {
                    destination.* = @as(Container, undefined);
                    return Populate(optional_info.child).intoFromValueLeaky(allocator, &destination.*.?, value);
                },
                else => {},
            }
            switch (value) {
                inline .table, .implicit_table, .inline_table => |table_value| {
                    if (Container == Parser.Value.Table) {
                        destination.* = value.table;
                        return;
                    }
                    if (type_info != .@"struct") return Error.IncorrectType;
                    var field_idx: usize = 0;
                    errdefer for (0..field_idx) |i| {
                        inline for (0.., type_info.@"struct".fields) |j, field| {
                            if (i == j) {
                                Populate(field.type).deinitDeep(allocator, &@field(destination, field.name));
                            }
                        }
                    };
                    inline for (type_info.@"struct".fields) |field| {
                        if (table_value.get(field.name)) |child_value| {
                            try Populate(field.type).intoFromValueLeaky(allocator, &@field(destination, field.name), child_value);
                            field_idx += 1;
                        } else {
                            if (@typeInfo(field.type) != .optional) {
                                return Error.MissingKey;
                            }
                            @field(destination, field.name) = null;
                        }
                    }
                },
                inline .array, .array_of_tables => |array_value, tag| {
                    if (type_info != .pointer or type_info.pointer.size != .slice) return Error.IncorrectType;
                    if (Container == []const u8) return Error.IncorrectType;
                    var result: std.ArrayListUnmanaged(type_info.pointer.child) = try .initCapacity(allocator, array_value.items.len);
                    errdefer result.deinit(allocator);
                    errdefer for (result.items) |*elem| {
                        Populate(type_info.pointer.child).deinitDeep(allocator, elem);
                    };
                    for (array_value.items) |inner_value| {
                        var rest: type_info.pointer.child = undefined;
                        try Populate(type_info.pointer.child).intoFromValueLeaky(allocator, &rest, switch (tag) {
                            .array => inner_value,
                            .array_of_tables => inner_value,
                            else => unreachable,
                        });
                        errdefer Populate(type_info.pointer.child).deinitDeep(allocator, rest);
                        result.appendAssumeCapacity(rest);
                    }
                    destination.* = try result.toOwnedSlice(allocator);
                },
                inline .string, .integer_string => |string_value| {
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
                .bool => |bool_value| {
                    if (Container != bool) return Error.IncorrectType;
                    destination.* = bool_value;
                },
                .datetime => |datetime_value| {
                    if (Container != DateTime) return Error.IncorrectType;
                    destination.* = datetime_value;
                },
            }
        }

        /// Populate a Zig type from a parsed TOML table.
        ///
        /// This is not guaranteed to be a fully safe and leak-free operation, but in most tested
        /// cases will be.
        ///
        /// Data can be de-initialized with `deinitDeep`.
        pub fn intoFromTableLeaky(
            allocator: std.mem.Allocator,
            destination: *Container,
            table: Parser.Value.Table,
        ) !void {
            try intoFromValueLeaky(allocator, destination, .{ .table = table });
        }

        /// Populate a Zig type from a parsed TOML value.
        ///
        /// Returns an arena which can be used to de-initialize all allocated data
        /// in one `std.heap.ArenaAllocator.deinit` call.
        pub fn intoFromValue(
            allocator: std.mem.Allocator,
            destination: *Container,
            value: Parser.Value,
        ) !std.heap.ArenaAllocator {
            var arena: std.heap.ArenaAllocator = .init(allocator);
            try intoFromValueLeaky(arena.allocator(), destination, value);
            return arena;
        }

        /// Populate a Zig type from a parsed TOML table.
        ///
        /// Returns an arena which can be used to de-initialize all allocated data
        /// in one `std.heap.ArenaAllocator.deinit` call.
        pub fn intoFromTable(
            allocator: std.mem.Allocator,
            destination: *Container,
            table: Parser.Value.Table,
        ) !std.heap.ArenaAllocator {
            return try intoFromValue(allocator, destination, .{ .table = table });
        }
    };
}

const TestDog = struct {
    const Friend = struct {
        name: []const u8,
        met_date: ?DateTime,
    };

    name: []const u8,
    breed: []const u8,
    age: i64,

    friends: []Friend,

    // any: Parser.Value.Table,
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
        \\met_date = 2025-09-15 19:37:00
    ;

    const doc = try microwave.parseFromSlice(std.testing.allocator, buf);
    defer doc.deinit();

    var test_struct: TestDog = undefined;
    try Populate(TestDog).intoFromTableLeaky(std.testing.allocator, &test_struct, doc.table);
    defer Populate(TestDog).deinitDeep(std.testing.allocator, &test_struct);

    try std.testing.expectEqualSlices(u8, "Barney", test_struct.name);
    try std.testing.expectEqualSlices(u8, "unknown", test_struct.breed);
    try std.testing.expectEqual(16, test_struct.age);
    try std.testing.expectEqualSlices(u8, "Bo", test_struct.friends[0].name);
    try std.testing.expectEqual(null, test_struct.friends[0].met_date);
    try std.testing.expectEqualSlices(u8, "Lala", test_struct.friends[1].name);
    try std.testing.expect(test_struct.friends[1].met_date != null);
    try std.testing.expect(test_struct.friends[1].met_date.? == .local_date_time);
    try std.testing.expectEqual(2025, test_struct.friends[1].met_date.?.local_date_time.date.year);
    try std.testing.expectEqual(9, test_struct.friends[1].met_date.?.local_date_time.date.month);
    try std.testing.expectEqual(15, test_struct.friends[1].met_date.?.local_date_time.date.day);
    try std.testing.expectEqual(19, test_struct.friends[1].met_date.?.local_date_time.time.hour);
    try std.testing.expectEqual(37, test_struct.friends[1].met_date.?.local_date_time.time.minute);
    try std.testing.expectEqual(0, test_struct.friends[1].met_date.?.local_date_time.time.second);
    try std.testing.expectEqual(null, test_struct.friends[1].met_date.?.local_date_time.time.millisecond);
}
