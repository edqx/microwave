const std = @import("std");

const Scanner = @import("Scanner.zig");

pub const Value = union(enum) {
    pub const Table = std.StringHashMapUnmanaged(Value);
    pub const Array = std.ArrayListUnmanaged(Value);
    pub const ArrayOfTables = std.ArrayListUnmanaged(Table);

    pub const DateTime = struct {
        date: ?[]const u8 = null,
        time: ?[]const u8 = null,
        offset: ?[]const u8 = null,
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

    pub fn deinitRecursive(self: *Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .none => {},
            .table => |*table_value| {
                var values = table_value.valueIterator();
                while (values.next()) |item_ptr| item_ptr.deinitRecursive(allocator);
                table_value.deinit(allocator);
            },
            .array => |*array_value| {
                for (array_value.items) |*item_ptr| item_ptr.deinitRecursive(allocator);
                array_value.deinit(allocator);
            },
            .array_of_tables => |*array_value| {
                for (array_value.items) |*table_value| {
                    var values = table_value.valueIterator();
                    while (values.next()) |item_ptr| item_ptr.deinitRecursive(allocator);
                    table_value.deinit(allocator);
                }
                array_value.deinit(allocator);
            },
            .string => |string_value| allocator.free(string_value),
            .integer, .float, .boolean => {},
            .date_time => |date_time_value| {
                if (date_time_value.date) |date| allocator.free(date);
                if (date_time_value.time) |time| allocator.free(time);
                if (date_time_value.offset) |offset| allocator.free(offset);
            },
        }
    }
};

pub fn Parser(ScannerType: type) type {
    return struct {
        const ParserT = @This();

        pub const Error = std.mem.Allocator.Error || std.fmt.ParseIntError || std.fmt.ParseFloatError || ScannerType.Error || error{ UnexpectedToken, UnexpectedEof, InvalidKeyAccess, DuplicateKey };

        allocator: std.mem.Allocator,
        scanner: *ScannerType,

        current_token: ?Scanner.Token = null,

        pub fn nextToken(self: *ParserT) !void {
            self.current_token = try self.scanner.next();
        }

        pub fn parseStringValueAlloc(self: ParserT, token_contents: []const u8) !Value {
            const string_contents = try self.allocator.dupe(u8, token_contents);
            errdefer self.allocator.free(string_contents);
            return .{ .string = string_contents };
        }

        pub fn parseBaseIntegerValue(self: ParserT, token_contents: []const u8) !Value {
            _ = self;
            const base: u8 = switch (token_contents[0]) {
                'x' => 16,
                'o' => 8,
                'b' => 2,
                else => unreachable,
            };
            const integer = std.fmt.parseInt(i64, token_contents[1..], base) catch |e| switch (e) {
                std.fmt.ParseIntError.Overflow => return e,
                std.fmt.ParseIntError.InvalidCharacter => unreachable,
            };
            return .{ .integer = integer };
        }

        pub fn parseIntegerValue(self: ParserT, token_contents: []const u8) !Value {
            _ = self;
            const integer = std.fmt.parseInt(i64, token_contents[0..], 10) catch |e| switch (e) {
                std.fmt.ParseIntError.Overflow => return e,
                std.fmt.ParseIntError.InvalidCharacter => unreachable,
            };
            return .{ .integer = integer };
        }

        pub fn parseFloatValue(self: ParserT, token_contents: []const u8) !Value {
            _ = self;
            const float = try std.fmt.parseFloat(f64, token_contents[0..]);
            return .{ .float = float };
        }

        pub fn parseInfValue(self: ParserT) !Value {
            _ = self;
            return .{ .float = std.math.inf(f64) };
        }

        pub fn parseNanValue(self: ParserT) !Value {
            _ = self;
            return .{ .float = std.math.nan(f64) };
        }

        pub fn parseBoolValue(self: ParserT, token_contents: []const u8) !Value {
            _ = self;
            const is_true = std.mem.eql(u8, token_contents, "true");
            const is_false = std.mem.eql(u8, token_contents, "false");
            std.debug.assert(is_true or is_false);
            return .{ .boolean = is_true };
        }

        pub fn parseOffsetDateTimeValueAlloc(self: ParserT, token_contents: []const u8) !Value {
            const offset_index = if (std.mem.endsWith(u8, token_contents, "Z"))
                token_contents.len - 1
            else blk: {
                const offset_separator = std.mem.lastIndexOfAny(u8, token_contents, "+-") orelse unreachable;
                break :blk offset_separator;
            };
            const offset_contents = try self.allocator.dupe(u8, token_contents[offset_index..]);
            errdefer self.allocator.free(offset_contents);
            const datetime_contents = token_contents[0..offset_index];

            const local_date_time_value = try self.parseLocalDateTimeValueAlloc(datetime_contents);
            errdefer local_date_time_value.deinitRecursive(self.allocator);

            return .{
                .date_time = .{
                    .date = local_date_time_value.date_time.date,
                    .time = local_date_time_value.date_time.time,
                    .offset = local_date_time_value.date_time.offset,
                },
            };
        }

        pub fn parseLocalDateTimeValueAlloc(self: ParserT, token_contents: []const u8) !Value {
            const time_index = (std.mem.indexOfAny(u8, token_contents, "T ") orelse unreachable) + 1;
            const date_contents = token_contents[0 .. time_index - 1];
            const time_contents = token_contents[time_index..];

            var local_date_value = try self.parseLocalDateValueAlloc(date_contents);
            errdefer local_date_value.deinitRecursive(self.allocator);
            var local_time_value = try self.parseLocalDateValueAlloc(time_contents);
            errdefer local_time_value.deinitRecursive(self.allocator);

            return .{
                .date_time = .{
                    .date = local_date_value.date_time.date,
                    .time = local_time_value.date_time.time,
                },
            };
        }

        pub fn parseLocalDateValueAlloc(self: ParserT, token_contents: []const u8) !Value {
            const date_contents = try self.allocator.dupe(u8, token_contents);
            errdefer self.allocator.free(date_contents);
            return .{
                .date_time = .{
                    .date = date_contents,
                },
            };
        }

        pub fn parseLocalTimeValueAlloc(self: ParserT, token_contents: []const u8) !Value {
            const time_contents = try self.allocator.dupe(u8, token_contents);
            errdefer self.allocator.free(time_contents);
            return .{
                .date_time = .{
                    .time = time_contents,
                },
            };
        }

        pub const AccessMode = enum {
            inline_table,
            root,
        };

        pub fn readTableAccessGetValuePtr(self: *ParserT, table: *Value.Table, mode: AccessMode) !*Value {
            std.debug.assert(self.current_token.?.kind == .key);

            var parent_table: *Value.Table = table;
            var last_key_token: Scanner.Token = self.current_token.?;

            var created_table_value_root: ?*Value = null;
            errdefer if (created_table_value_root) |table_value| table_value.deinitRecursive(self.allocator);

            var expecting_key = false;
            try self.nextToken(); // skip first key
            while (self.current_token) |token| : (try self.nextToken()) {
                switch (token.kind) {
                    .key => {
                        if (!expecting_key) return Error.UnexpectedToken;
                        const key_contents = self.scanner.tokenContents(last_key_token);
                        const nested_table_value = try parent_table.getOrPut(self.allocator, key_contents);
                        errdefer _ = parent_table.remove(key_contents);
                        if (nested_table_value.found_existing) {
                            switch (mode) {
                                .inline_table => {
                                    if (nested_table_value.value_ptr.* != .table) return Error.InvalidKeyAccess;
                                    parent_table = &nested_table_value.value_ptr.table;
                                },
                                .root => {
                                    switch (nested_table_value.value_ptr.*) {
                                        .table => |*table_value| {
                                            parent_table = table_value;
                                        },
                                        .array_of_tables => |*array_of_tables_value| {
                                            parent_table = &array_of_tables_value.items[array_of_tables_value.items.len - 1];
                                        },
                                        else => return Error.InvalidKeyAccess,
                                    }
                                },
                            }
                        } else {
                            nested_table_value.value_ptr.* = .{ .table = .empty };
                            parent_table = &nested_table_value.value_ptr.table;
                            created_table_value_root = nested_table_value.value_ptr;
                        }
                        last_key_token = token;
                        expecting_key = false;
                    },
                    .access => {
                        if (expecting_key) return Error.UnexpectedToken;
                        expecting_key = true;
                    },
                    else => {
                        if (expecting_key) return Error.UnexpectedToken;
                        break;
                    },
                }
            } else return Error.UnexpectedEof;

            const key_contents = self.scanner.tokenContents(last_key_token);
            const last_value = try parent_table.getOrPut(self.allocator, key_contents);

            if (!last_value.found_existing) last_value.value_ptr.* = .none;
            return last_value.value_ptr;
        }

        pub fn readArrayValue(self: *ParserT) !Value {
            std.debug.assert(self.current_token.?.kind == .array_start);

            var array_value: Value = .{ .array = .empty };
            errdefer array_value.deinitRecursive(self.allocator);

            try self.nextToken(); // skip array start token
            while (self.current_token) |token| : (try self.nextToken()) {
                if (token.kind == .array_end) break;
                try array_value.array.append(self.allocator, try self.readValue());
            }
            return array_value;
        }

        pub fn readInlineTableValue(self: *ParserT) !Value {
            std.debug.assert(self.current_token.?.kind == .inline_table_start);

            var table_value: Value = .{ .table = .empty };
            errdefer table_value.deinitRecursive(self.allocator);

            try self.nextToken(); // skip inline table start token
            while (self.current_token) |token| : (try self.nextToken()) {
                if (token.kind == .inline_table_end) break;
                const table_entry = try self.readTableAccessGetValuePtr(&table_value.table, .inline_table);
                if (table_entry.* != .none) return Error.DuplicateKey;
                table_entry.* = try self.readValue();
                errdefer table_entry.deinitRecursive(self.allocator);
            }
            return table_value;
        }

        pub fn readValue(self: *ParserT) Error!Value {
            const token_contents = self.scanner.tokenContents(self.current_token.?);
            const value = switch (self.current_token.?.kind) {
                .ignored,
                .comment,
                .table_start,
                .table_end,
                .many_table_start,
                .many_table_end,
                => unreachable,
                .key,
                .access,
                .inline_table_end,
                .array_end,
                => return Error.UnexpectedToken,
                .literal_string => try self.parseStringValueAlloc(token_contents),
                .literal_base_integer => try self.parseBaseIntegerValue(token_contents),
                .literal_integer => try self.parseIntegerValue(token_contents),
                .literal_float => try self.parseFloatValue(token_contents),
                .literal_inf => try self.parseInfValue(),
                .literal_nan => try self.parseNanValue(),
                .literal_bool => try self.parseBoolValue(token_contents),
                .literal_offset_date_time => try self.parseOffsetDateTimeValueAlloc(token_contents),
                .literal_local_date_time => try self.parseLocalDateTimeValueAlloc(token_contents),
                .literal_local_date => try self.parseLocalDateValueAlloc(token_contents),
                .literal_local_time => try self.parseLocalTimeValueAlloc(token_contents),
                .array_start => try self.readArrayValue(),
                .inline_table_start => try self.readInlineTableValue(),
            };
            return value;
        }

        pub fn readRootTableValue(self: *ParserT) !Value {
            var root_table: Value = .{ .table = .empty };
            var active_table = &root_table.table;

            try self.nextToken(); // skip inline table start token
            while (self.current_token) |_| : (try self.nextToken()) {
                switch (self.current_token.?.kind) {
                    .table_start => {
                        try self.nextToken();
                        const table_entry = try self.readTableAccessGetValuePtr(&root_table.table, .root);
                        if (table_entry.* != .none) return Error.DuplicateKey;
                        table_entry.* = .{ .table = .empty };
                        active_table = &table_entry.table;
                    },
                    .many_table_start => {
                        try self.nextToken();
                        const many_entry = try self.readTableAccessGetValuePtr(&root_table.table, .root);
                        if (many_entry.* == .none) {
                            many_entry.* = .{ .array_of_tables = .empty };
                        }
                        try many_entry.array_of_tables.append(self.allocator, .empty);
                        active_table = &many_entry.array_of_tables.items[many_entry.array_of_tables.items.len - 1];
                    },
                    .key => {
                        const table_entry = try self.readTableAccessGetValuePtr(active_table, .inline_table);
                        if (table_entry.* != .none) return Error.DuplicateKey;
                        table_entry.* = try self.readValue();
                        errdefer table_entry.deinitRecursive(self.allocator);
                    },
                    else => {
                        return Error.UnexpectedToken;
                    },
                }
            }
            return root_table;
        }
    };
}

allocator: std.mem.Allocator,

fn testKey(table_value: Value, path: anytype, comptime value_type: std.meta.Tag(Value), value: @FieldType(Value, @tagName(value_type))) !void {
    var parent = table_value;
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
            child = parent.table.get(part);
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
    defer root_table.deinitRecursive(std.testing.allocator);

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
