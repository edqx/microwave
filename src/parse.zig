const std = @import("std");

const Scanner = @import("Scanner.zig");

pub const Value = union(enum) {
    pub const Table = std.StringArrayHashMapUnmanaged(Value);
    pub const Array = std.ArrayListUnmanaged(Value);
    pub const ArrayOfTables = std.ArrayListUnmanaged(Table);

    pub const DateTime = struct {
        date: ?[]const u8 = null,
        time: ?[]const u8 = null,
        offset: ?[]const u8 = null,

        pub fn dupe(self: DateTime, allocator: std.mem.Allocator) !DateTime {
            const new_date = if (self.date) |date| try allocator.dupe(u8, date) else null;
            errdefer if (new_date) |date| allocator.free(date);
            const new_time = if (self.time) |time| try allocator.dupe(u8, time) else null;
            errdefer if (new_time) |time| allocator.free(time);
            const new_offset = if (self.offset) |offset| try allocator.dupe(u8, offset) else null;
            errdefer if (new_offset) |offset| allocator.free(offset);
            return .{
                .date = new_date,
                .time = new_time,
                .offset = new_offset,
            };
        }

        pub fn deinit(self: DateTime, allocator: std.mem.Allocator) void {
            if (self.offset) |offset| allocator.free(offset);
            if (self.time) |time| allocator.free(time);
            if (self.date) |date| allocator.free(date);
        }
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
                var result: Value = .{ .table = .empty };
                errdefer result.deinitRecursive(allocator);
                var entries = table_value.iterator();
                while (entries.next()) |entry| {
                    var duped = try entry.value_ptr.dupeRecursive(allocator);
                    errdefer duped.deinitRecursive(allocator);
                    try result.table.put(allocator, entry.key_ptr.*, duped);
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
            .date_time => |date_time_value| .{ .date_time = try date_time_value.dupe(allocator) },
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
                    for (table_value.values()) |*item_ptr| item_ptr.deinitRecursive(allocator);
                    table_value.deinit(allocator);
                }
                array_value.deinit(allocator);
            },
            .string => |string_value| allocator.free(string_value),
            .integer, .float, .boolean => {},
            .date_time => |date_time_value| date_time_value.deinit(allocator),
        }
    }
};

pub fn deinitTable(allocator: std.mem.Allocator, table_value: *Value.Table) void {
    for (table_value.values()) |*item_ptr| item_ptr.deinitRecursive(allocator);
    table_value.deinit(allocator);
}

pub fn Parser(ScannerType: type) type {
    return struct {
        const ParserT = @This();

        pub const Error = std.mem.Allocator.Error || std.fmt.ParseIntError || std.fmt.ParseFloatError || ScannerType.Error || error{ UnexpectedToken, UnexpectedEof, UnexpectedEol, InvalidKeyAccess, DuplicateKey };

        allocator: std.mem.Allocator,
        scanner: *ScannerType,

        current_token: ?Scanner.Token = null,

        pub fn nextToken(self: *ParserT) !void {
            self.current_token = try self.scanner.next();
        }

        fn assertCurrentToken(self: *ParserT, token_kind: Scanner.Token.Kind) void {
            std.debug.assert(self.current_token != null and self.current_token.?.kind == token_kind);
        }

        pub fn consumeWhitespace(self: *ParserT) !void {
            while (self.current_token) |token| {
                if (token.kind != .whitespace) return;
                try self.nextToken();
            }
        }

        pub fn consumeToken(self: *ParserT, token_kind: Scanner.Token.Kind) !?Scanner.Token {
            try self.consumeWhitespace();
            if (token_kind == .newline) {
                if (try self.consumeToken(.comment) != null) try self.nextToken();
            }
            const next_token = self.current_token orelse return null;
            if (next_token.kind != token_kind) return null;
            return next_token;
        }

        pub fn expectToken(self: *ParserT, token_kind: Scanner.Token.Kind) !void {
            if (try self.consumeToken(token_kind) == null) return Error.UnexpectedToken;
        }

        pub fn isEof(self: *ParserT) bool {
            return self.current_token == null;
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
            const offset_index = if (std.mem.lastIndexOfAny(u8, token_contents, "Zz") == token_contents.len - 1)
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
            const time_index = (std.mem.indexOfAny(u8, token_contents, "Tt ") orelse unreachable) + 1;
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

        pub const AccessEntry = struct {
            parent_table: *Value.Table,
            key_contents: []const u8,
            value_ptr: *Value,

            pub fn remove(self: AccessEntry) void {
                std.debug.assert(self.parent_table.orderedRemove(self.key_contents));
            }
        };

        pub fn consumeTableAccessGetValuePtr(self: *ParserT, table: *Value.Table, mode: AccessMode, expect_end_token_kind: Scanner.Token.Kind) !?AccessEntry {
            const first_key_token = try self.consumeToken(.key) orelse return null;
            try self.nextToken();
            const key_contents = self.scanner.tokenContents(first_key_token);

            if (try self.consumeToken(expect_end_token_kind) != null) {
                const last_value = try table.getOrPut(self.allocator, key_contents);
                if (!last_value.found_existing) last_value.value_ptr.* = .none;
                return .{
                    .parent_table = table,
                    .key_contents = last_value.key_ptr.*,
                    .value_ptr = last_value.value_ptr,
                };
            }

            if (try self.consumeToken(.access) != null) {
                try self.nextToken();
                const sub_table = try table.getOrPut(self.allocator, key_contents);

                if (sub_table.found_existing) {
                    switch (mode) {
                        .inline_table => {
                            if (sub_table.value_ptr.* != .table) return Error.InvalidKeyAccess;
                            return try self.consumeTableAccessGetValuePtr(&sub_table.value_ptr.table, mode, expect_end_token_kind);
                        },
                        .root => switch (sub_table.value_ptr.*) {
                            .table => |*table_value| {
                                return try self.consumeTableAccessGetValuePtr(table_value, mode, expect_end_token_kind);
                            },
                            .array_of_tables => |*array_of_tables_value| {
                                const last_table = &array_of_tables_value.items[array_of_tables_value.items.len - 1];
                                return try self.consumeTableAccessGetValuePtr(last_table, mode, expect_end_token_kind);
                            },
                            else => return Error.InvalidKeyAccess,
                        },
                    }
                } else {
                    sub_table.value_ptr.* = .{ .table = .empty };
                    errdefer _ = table.orderedRemove(key_contents);
                    return try self.consumeTableAccessGetValuePtr(&sub_table.value_ptr.table, mode, expect_end_token_kind);
                }
            }

            return Error.UnexpectedToken;
        }

        pub fn consumeArrayValue(self: *ParserT) !?Value {
            if (try self.consumeToken(.array_start) == null) return null;
            self.scanner.setState(.array_container);
            try self.nextToken();

            var array_value: Value = .{ .array = .empty };
            errdefer array_value.deinitRecursive(self.allocator);

            if (try self.consumeToken(.newline) != null) try self.nextToken();
            if (try self.consumeToken(.array_end) != null) {
                return array_value;
            }

            var i: usize = 0;
            while (true) : (i += 1) {
                var element = try self.readValue();
                self.scanner.setState(.array_container);
                errdefer element.deinitRecursive(self.allocator);
                try self.nextToken();

                try array_value.array.append(self.allocator, element);
                errdefer _ = array_value.array.pop();

                if (try self.consumeToken(.newline) != null) try self.nextToken();
                if (try self.consumeToken(.delimeter) == null) {
                    if (try self.consumeToken(.newline) != null) try self.nextToken();
                    try self.expectToken(.array_end);
                    break;
                }
                try self.nextToken();
                if (try self.consumeToken(.newline) != null) try self.nextToken();
                if (try self.consumeToken(.array_end) != null) break;
            }

            return array_value;
        }

        pub fn consumeInlineTableValue(self: *ParserT) !?Value {
            if (try self.consumeToken(.inline_table_start) == null) return null;
            self.scanner.setState(.inline_key);
            try self.nextToken();

            var table_value: Value = .{ .table = .empty };
            errdefer table_value.deinitRecursive(self.allocator);

            if (try self.consumeToken(.newline) != null) try self.nextToken();
            if (try self.consumeToken(.inline_table_end) != null) {
                return table_value;
            }

            var i: usize = 0;
            while (true) : (i += 1) {
                self.scanner.setState(.inline_key);
                const table_entry = try self.consumeTableAccessGetValuePtr(&table_value.table, .inline_table, .equals) orelse
                    return Error.UnexpectedToken;
                if (table_entry.value_ptr.* != .none) return Error.DuplicateKey;
                errdefer table_entry.remove();

                if (try self.consumeToken(.equals) == null) return Error.UnexpectedToken;
                self.scanner.setState(.value);
                try self.nextToken();

                table_entry.value_ptr.* = try self.readValue();
                self.scanner.setState(.inline_key);
                errdefer table_entry.value_ptr.deinitRecursive(self.allocator);
                try self.nextToken();

                if (try self.consumeToken(.newline) != null) try self.nextToken();
                if (try self.consumeToken(.delimeter) == null) {
                    if (try self.consumeToken(.newline) != null) try self.nextToken();
                    try self.expectToken(.inline_table_end);
                    break;
                }
                try self.nextToken();
            }

            return table_value;
        }

        pub fn readValue(self: *ParserT) Error!Value {
            try self.consumeWhitespace();
            if (self.current_token == null) return Error.UnexpectedEof;
            const token_contents = self.scanner.tokenContents(self.current_token.?);
            const value = switch (self.current_token.?.kind) {
                .whitespace,
                .key,
                .access,
                .equals,
                .table_start,
                .table_end,
                .many_table_start,
                .many_table_end,
                => unreachable,
                .comment,
                .newline,
                .delimeter,
                .array_end,
                .inline_table_end,
                => return Error.UnexpectedToken,
                .array_start,
                .inline_table_start,
                => return try self.consumeInlineTableValue() orelse
                    try self.consumeArrayValue() orelse return Error.UnexpectedToken,
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
            };
            return value;
        }

        pub fn readRootTableValue(self: *ParserT) !Value.Table {
            var root_table: Value = .{ .table = .empty };
            errdefer deinitTable(self.allocator, &root_table.table);

            var active_table = &root_table.table;

            forever: while (true) {
                self.scanner.setState(.key);
                try self.nextToken();

                if (self.isEof()) break;

                while (try self.consumeToken(.newline) != null) {
                    try self.nextToken();
                    if (self.isEof()) break :forever;
                }

                if (try self.consumeTableAccessGetValuePtr(active_table, .inline_table, .equals)) |table_entry| {
                    if (table_entry.value_ptr.* != .none) return Error.DuplicateKey;
                    errdefer table_entry.remove();

                    if (try self.consumeToken(.equals) == null) return Error.UnexpectedToken;
                    self.scanner.setState(.value);
                    try self.nextToken();
                    table_entry.value_ptr.* = try self.readValue();
                    errdefer table_entry.value_ptr.deinitRecursive(self.allocator);
                    continue;
                }

                if (try self.consumeToken(.table_start) != null) {
                    self.scanner.setState(.table_key);
                    try self.nextToken();

                    const table_entry = try self.consumeTableAccessGetValuePtr(&root_table.table, .root, .table_end) orelse return Error.UnexpectedToken;
                    const created_new = switch (table_entry.value_ptr.*) {
                        .none => blk: {
                            table_entry.value_ptr.* = .{ .table = .empty };
                            break :blk true;
                        },
                        .table => false,
                        else => return Error.DuplicateKey,
                    };
                    errdefer if (created_new) table_entry.remove();
                    active_table = &table_entry.value_ptr.table;

                    try self.expectToken(.table_end);
                    continue;
                }

                if (try self.consumeToken(.many_table_start) != null) {
                    try self.nextToken();
                    self.scanner.setState(.table_key);

                    const table_entry = try self.consumeTableAccessGetValuePtr(&root_table.table, .root, .many_table_end) orelse return Error.UnexpectedToken;
                    const created_new = switch (table_entry.value_ptr.*) {
                        .none => blk: {
                            table_entry.value_ptr.* = .{ .array_of_tables = .empty };
                            break :blk true;
                        },
                        .array_of_tables => false,
                        else => return Error.DuplicateKey,
                    };
                    errdefer if (created_new) table_entry.remove();
                    try table_entry.value_ptr.array_of_tables.append(self.allocator, .empty);

                    active_table = &table_entry.value_ptr.array_of_tables.items[table_entry.value_ptr.array_of_tables.items.len - 1];
                    try self.expectToken(.many_table_end);
                    continue;
                }

                return Error.UnexpectedToken;
            }
            return root_table.table;
        }
    };
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

pub fn fromSliceOwned(arena: std.mem.Allocator, slice: []const u8) !Value.Table {
    var scanner: Scanner = .{ .buffer = slice };
    return try fromScannerOwned(arena, &scanner);
}

pub fn fromReaderOwned(arena: std.mem.Allocator, reader: anytype) !Value.Table {
    var scanner: Scanner.bufferedReaderScanner(reader) = .{ .reader = reader };
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
        \\ints = [1, 2, 3, ]
        \\floats = [1.1, 2.1, 3.1]
        \\strings = ["a", "b", "c"]
        \\dates = [
        \\  1987-07-05T17:45:00Z,
        \\  1979-05-27T07:32:00Z,
        \\  2006-06-01T11:00:00Z,
        \\]
        \\comments = [
        \\         1,
        \\         2, #this is ok
        \\]
        \\
    );
    defer res.deinit();
}
