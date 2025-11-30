const std = @import("std");

const Scanner = @import("Scanner.zig");

pub const Value = union(enum) {
    pub const Table = struct {
        explicit: bool,
        keys: std.StringArrayHashMapUnmanaged(Value),
    };

    pub const Array = std.ArrayListUnmanaged(Value);
    pub const ArrayOfTables = std.ArrayListUnmanaged(Table);

    pub const DateTime = union(enum) {
        pub const Year = std.math.IntFittingRange(0, 10000);
        pub const Month = std.math.IntFittingRange(1, 12);
        pub const Day = std.math.IntFittingRange(1, 31);

        pub const Hour = std.math.IntFittingRange(0, 23);
        pub const OffsetHour = std.math.IntFittingRange(0, 24);
        pub const Minute = std.math.IntFittingRange(0, 59);
        pub const Second = std.math.IntFittingRange(0, 59);
        pub const Millisecond = u64;

        pub const Date = struct {
            year: Year,
            month: Month,
            day: Day,
        };

        pub const Offset = struct {
            negative: bool,
            hour: OffsetHour,
            minute: Minute,

            pub fn isUtc(self: Offset) bool {
                return self.hour == 0 and self.minute == 0;
            }
        };

        pub const Time = struct {
            hour: Hour,
            minute: Minute,
            second: ?Second,
            millisecond: ?u64,
        };

        just_date: Date,
        just_time: Time,
        local_date_time: struct {
            date: Date,
            time: Time,
        },
        offset_date_time: struct {
            date: Date,
            time: Time,
            offset: Offset,
        },
    };

    none: void,
    table: Table,
    array: Array,
    array_of_tables: ArrayOfTables,
    string: []const u8,
    integer: i64,
    float: f64,
    boolean: bool,
    date_time: DateTime,

    pub fn dupeRecursive(self: Value, allocator: std.mem.Allocator) !Value {
        return blk: switch (self) {
            .none => .none,
            .table => |table_value| {
                var result: Value = .{ .table = .{ .explicit = table_value.explicit, .keys = .empty } };
                errdefer result.deinitRecursive(allocator);
                var entries = table_value.keys.iterator();
                while (entries.next()) |entry| {
                    var duped = try entry.value_ptr.dupeRecursive(allocator);
                    errdefer duped.deinitRecursive(allocator);
                    try result.table.keys.put(allocator, entry.key_ptr.*, duped);
                }
                break :blk result;
            },
            .array => |array_value| {
                var result: Value = .{ .array = try .initCapacity(allocator, array_value.items.len) };
                errdefer result.deinitRecursive(allocator);
                for (array_value.items) |item| {
                    var duped = try item.dupeRecursive(allocator);
                    errdefer duped.deinitRecursive(allocator);
                    result.array.appendAssumeCapacity(duped);
                }
                break :blk result;
            },
            .array_of_tables => |array_value| {
                var result: Value = .{ .array = try .initCapacity(allocator, array_value.items.len) };
                errdefer result.deinitRecursive(allocator);
                for (array_value.items) |item| {
                    var duped = try (@as(Value, .{ .table = item })).dupeRecursive(allocator);
                    errdefer duped.deinitRecursive(allocator);
                    result.array.appendAssumeCapacity(duped);
                }
                break :blk result;
            },
            .string => |string_value| .{ .string = try allocator.dupe(u8, string_value) },
            .integer, .float, .boolean => self,
            .date_time => self,
        };
    }

    pub fn deinitRecursive(self: *Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .none => {},
            .table => |*table_value| {
                deinitTable(allocator, table_value);
            },
            .array => |*array_value| {
                for (array_value.items) |*item_ptr| item_ptr.deinitRecursive(allocator);
                array_value.deinit(allocator);
            },
            .array_of_tables => |*array_value| {
                for (array_value.items) |*table_value| {
                    deinitTable(allocator, table_value);
                }
                array_value.deinit(allocator);
            },
            .string => |string_value| allocator.free(string_value),
            .integer, .float, .boolean => {},
            .date_time => {},
        }
    }
};

pub fn deinitTable(allocator: std.mem.Allocator, table_value: *Value.Table) void {
    for (table_value.keys.values()) |*item_ptr| item_ptr.deinitRecursive(allocator);
    table_value.keys.deinit(allocator);
}

pub const Document = struct {
    root_table: Value.Table,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: Document) void {
        self.arena.deinit();
    }
};

pub fn fromScannerOwned(arena: std.mem.Allocator, scanner: anytype) !Value.Table {
    var parser: Parser(@typeInfo(@TypeOf(scanner)).pointer.child) = .{
        .allocator = arena,
        .scanner = scanner,
    };

    const root_table = try parser.readRootTableValue();
    errdefer root_table.deinit(arena);

    return root_table;
}

fn getFixedBufferPos(scanner: *Scanner) usize {
    return scanner.reader.seek;
}

pub fn fromSliceOwned(arena: std.mem.Allocator, slice: []const u8) !Value.Table {
    var reader: std.Io.Reader = .fixed(slice);
    var scanner: Scanner = .{
        .vtable = &.{
            .getSeekPos = getFixedBufferPos,
        },
        .reader = &reader,
    };
    return try fromScannerOwned(arena, &scanner);
}

pub fn fromReaderOwned(arena: std.mem.Allocator, reader: anytype) !Value.Table {
    var scanner: Scanner = .{ .reader = &reader };
    return try fromScannerOwned(arena, &scanner);
}

pub fn fromScanner(gpa: std.mem.Allocator, scanner: anytype) !Document {
    var arena = std.heap.ArenaAllocator.init(gpa);
    errdefer arena.deinit();

    return .{
        .root_table = try fromScannerOwned(arena.allocator(), scanner),
        .arena = arena,
    };
}

pub fn fromSlice(gpa: std.mem.Allocator, slice: []const u8) !Document {
    var arena = std.heap.ArenaAllocator.init(gpa);
    errdefer arena.deinit();

    return .{
        .root_table = try fromSliceOwned(arena.allocator(), slice),
        .arena = arena,
    };
}

pub fn fromReader(gpa: std.mem.Allocator, reader: anytype) !Document {
    var arena = std.heap.ArenaAllocator.init(gpa);
    errdefer arena.deinit();

    return .{
        .root_table = try fromReaderOwned(arena.allocator(), reader),
        .arena = arena,
    };
}

fn testKey(table_value: Value.Table, path: anytype, comptime value_type: std.meta.Tag(Value), value: @FieldType(Value, @tagName(value_type))) !void {
    var parent: Value = .{ .table = table_value };
    inline for (0.., path) |i, part| {
        const is_last = i == path.len - 1;
        var child: ?Value = undefined;
        if (@TypeOf(part) == comptime_int) {
            try std.testing.expect(parent == .array or parent == .array_of_tables);
            child = switch (parent) {
                .array => parent.array.items[part],
                .array_of_tables => .{ .table = parent.array_of_tables.items[part] },
                else => unreachable,
            };
        } else {
            try std.testing.expect(parent == .table);
            child = parent.table.keys.get(part);
        }
        try std.testing.expect(child != null);
        if (is_last) {
            try std.testing.expect(std.meta.activeTag(child.?) == value_type);
            if (value_type == .string) {
                try std.testing.expectEqualSlices(u8, value, @field(child.?, @tagName(value_type)));
            } else {
                try std.testing.expectEqual(value, @field(child.?, @tagName(value_type)));
            }
        } else {
            parent = child.?;
        }
    }
}

test Parser {
    const buf: []const u8 =
        \\barney.name = "Barney"
        \\barney.age = 16
        \\barney.breed = "unknown"
        \\
        \\sprout = { name = "Sprout", age = 15, breed = "cairn-terrier x jack russell" }
        \\
        \\[barney.colours]
        \\head = "white"
        \\body = "brown"
        \\tail = "red"
        \\
        \\[[other_dog]]
        \\name = "Bo"
        \\colour = "White"
        \\origin = "Egypt"
        \\
        \\[[other_dog]]
        \\name = "Lala"
        \\colour = "Black"
        \\origin = "Serbia"
        \\
        \\[other_dog.colours]
        \\head = "black"
        \\body = "black"
        \\tail = "black"
    ;

    var scanner = Scanner{ .buffer = buf };
    var parser: Parser(Scanner) = .{ .allocator = std.testing.allocator, .scanner = &scanner };

    var root_table = try parser.readRootTableValue();
    defer deinitTable(std.testing.allocator, &root_table);

    try testKey(root_table, .{ "barney", "name" }, .string, "Barney");
    try testKey(root_table, .{ "barney", "age" }, .integer, 16);
    try testKey(root_table, .{ "barney", "breed" }, .string, "unknown");

    try testKey(root_table, .{ "barney", "colours", "head" }, .string, "white");
    try testKey(root_table, .{ "barney", "colours", "body" }, .string, "brown");
    try testKey(root_table, .{ "barney", "colours", "tail" }, .string, "red");

    try testKey(root_table, .{ "other_dog", 0, "name" }, .string, "Bo");
    try testKey(root_table, .{ "other_dog", 1, "name" }, .string, "Lala");

    try testKey(root_table, .{ "other_dog", 1, "colours", "head" }, .string, "black");
    try testKey(root_table, .{ "other_dog", 1, "colours", "body" }, .string, "black");
    try testKey(root_table, .{ "other_dog", 1, "colours", "tail" }, .string, "black");
}

test "parse test" {
    const res = try fromSlice(std.testing.allocator,
        \\        
        \\"\u0000" = "null"
        \\'\u0000' = "different key"
        \\"\u0008 \u000c \U00000041 \u007f \u0080 \u00ff \ud7ff \ue000 \uffff \U00010000 \U0010ffff" = "escaped key"
        \\
        \\"~  ÿ ퟿    𐀀 􏿿" = "basic key"
        \\'l ~  ÿ ퟿    𐀀 􏿿' = "literal key"
        \\
    );
    defer res.deinit();
}
