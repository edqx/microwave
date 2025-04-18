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
                _ = try self.consumeToken(.comment);
            }
            const next_token = self.current_token orelse return null;
            if (next_token.kind != token_kind) return null;
            try self.nextToken();
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

        pub fn consumeAnyKey(self: *ParserT) !?Scanner.Token {
            try self.consumeWhitespace();
            const next_token = self.current_token orelse return null;
            switch (next_token.kind) {
                .key, .literal_string, .literal_base_integer, .literal_integer, .literal_float, .literal_inf, .literal_nan, .literal_bool, .literal_offset_date_time, .literal_local_date_time, .literal_local_date, .literal_local_time => {},
                else => return null,
            }
            try self.nextToken();
            return next_token;
        }

        pub fn consumeTableAccessGetValuePtr(self: *ParserT, table: *Value.Table, mode: AccessMode) !?*Value {
            const first_key_token = try self.consumeAnyKey() orelse return null;
            var parent_table: *Value.Table = table;
            var last_key_token: Scanner.Token = first_key_token;

            var created_table_value_root: ?*Value = null;
            errdefer if (created_table_value_root) |table_value| table_value.deinitRecursive(self.allocator);

            var expecting_key = false;
            while (true) {
                if (try self.consumeAnyKey()) |key_token| {
                    if (!expecting_key) return Error.UnexpectedToken;
                    const key_contents = self.scanner.tokenContents(last_key_token);
                    const nested_table_value = try parent_table.getOrPut(self.allocator, key_contents);
                    errdefer _ = parent_table.orderedRemove(key_contents);
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
                    last_key_token = key_token;
                    expecting_key = false;
                } else if (try self.consumeToken(.access) != null) {
                    if (expecting_key) return Error.UnexpectedToken;
                    expecting_key = true;
                } else {
                    if (expecting_key) return Error.UnexpectedToken;
                    break;
                }
            }

            const key_contents = self.scanner.tokenContents(last_key_token);
            const last_value = try parent_table.getOrPut(self.allocator, key_contents);

            if (!last_value.found_existing) last_value.value_ptr.* = .none;
            return last_value.value_ptr;
        }

        pub fn consumeArrayValue(self: *ParserT) !?Value {
            if (try self.consumeToken(.array_start_or_table_start) == null) return null;

            var array_value: Value = .{ .array = .empty };
            errdefer array_value.deinitRecursive(self.allocator);

            var i: usize = 0;
            while (true) : (i += 1) {
                _ = try self.consumeToken(.newline);
                const has_delimeter = if (i > 0) try self.consumeToken(.delimeter) != null else false;
                _ = try self.consumeToken(.newline);
                if (try self.consumeToken(.array_end_or_table_end) != null) break;
                if (i != 0 and !has_delimeter) return Error.UnexpectedToken;
                try array_value.array.append(self.allocator, try self.readValue());
            }

            return array_value;
        }

        pub fn consumeInlineTableValue(self: *ParserT) !?Value {
            if (try self.consumeToken(.inline_table_start) == null) return null;

            var table_value: Value = .{ .table = .empty };
            errdefer table_value.deinitRecursive(self.allocator);

            var i: usize = 0;
            while (true) : (i += 1) {
                _ = try self.consumeToken(.newline);
                const has_delimeter = if (i > 0) try self.consumeToken(.delimeter) != null else false;
                _ = try self.consumeToken(.newline);
                if (try self.consumeToken(.inline_table_end) != null) break;
                if (i != 0 and !has_delimeter) return Error.UnexpectedToken;
                const table_entry = try self.consumeTableAccessGetValuePtr(&table_value.table, .inline_table) orelse
                    return Error.UnexpectedToken;
                if (table_entry.* != .none) return Error.DuplicateKey;
                try self.expectToken(.equals);
                table_entry.* = try self.readValue();
                errdefer table_entry.deinitRecursive(self.allocator);
            }

            return table_value;
        }

        pub fn readValue(self: *ParserT) Error!Value {
            try self.consumeWhitespace();
            if (self.current_token == null) return Error.UnexpectedEof;
            const token_contents = self.scanner.tokenContents(self.current_token.?);
            const value = switch (self.current_token.?.kind) {
                .whitespace,
                => unreachable,
                .comment,
                .newline,
                .delimeter,
                .key,
                .access,
                .equals,
                .array_start_or_table_start,
                .array_end_or_table_end,
                .inline_table_start,
                .inline_table_end,
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
            try self.nextToken();
            return value;
        }

        pub fn readRootTableValue(self: *ParserT) !Value.Table {
            var root_table: Value = .{ .table = .empty };
            errdefer deinitTable(self.allocator, &root_table.table);

            var active_table = &root_table.table;

            try self.nextToken();

            while (true) {
                if (try self.consumeToken(.newline) != null) continue;
                if (self.isEof()) break;
                if (try self.consumeToken(.array_start_or_table_start) != null) {
                    const is_many_table = try self.consumeToken(.array_start_or_table_start) != null;

                    const table_entry = try self.consumeTableAccessGetValuePtr(&root_table.table, .root) orelse return Error.UnexpectedToken;
                    if (is_many_table) {
                        switch (table_entry.*) {
                            .none => {
                                table_entry.* = .{ .array_of_tables = .empty };
                            },
                            .array_of_tables => {},
                            else => return Error.DuplicateKey,
                        }
                        try table_entry.array_of_tables.append(self.allocator, .empty);
                        active_table = &table_entry.array_of_tables.items[table_entry.array_of_tables.items.len - 1];
                        if (try self.consumeToken(.array_end_or_table_end) == null) return Error.UnexpectedToken;
                    } else {
                        switch (table_entry.*) {
                            .none => {
                                table_entry.* = .{ .table = .empty };
                            },
                            .table => {},
                            else => return Error.DuplicateKey,
                        }
                        active_table = &table_entry.table;
                    }
                    if (try self.consumeToken(.array_end_or_table_end) == null) return Error.UnexpectedToken;
                } else if (try self.consumeTableAccessGetValuePtr(active_table, .inline_table)) |table_entry| {
                    if (table_entry.* != .none) return Error.DuplicateKey;
                    if (try self.consumeToken(.equals) == null) return Error.UnexpectedToken;
                    table_entry.* = try self.readValue();
                    errdefer table_entry.deinitRecursive(self.allocator);
                } else {
                    return Error.UnexpectedToken;
                }
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
        \\str4 = """Here are two quotation marks: "". Simple enough."""
        \\# str5 = """Here are three quotation marks: """."""  # INVALID
        \\str5 = """Here are three quotation marks: ""\"."""
        \\str6 = """Here are fifteen quotation marks: ""\"""\"""\"""\"""\"."""
        \\
        \\# "This," she said, "is just a pointless statement."
        \\str7 = """"This," she said, "is just a pointless statement.""""
    );
    defer res.deinit();
}
