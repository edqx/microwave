const std = @import("std");

const Scanner = @import("Scanner.zig");

pub const Error = error{ UnexpectedToken, UnexpectedEof, InvalidKeyAccess, DuplicateKey };

pub const Value = union(enum) {
    pub const Table = std.StringHashMapUnmanaged(Value);
    pub const Array = std.ArrayListUnmanaged(Value);
    pub const ArrayOfTables = std.ArrayListUnmanaged(Table);

    pub const DateTime = struct {
        date: ?[]const u8 = null,
        time: ?[]const u8 = null,
        offset: ?[]const u8 = null,
    };

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

        allocator: std.mem.Allocator,
        scanner: *ScannerType,

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

        pub const TableEntry = struct {
            value_ptr: *Value,
            first_value_token: Scanner.Token,
        };

        pub fn readTableAccess(self: ParserT, first_token: Scanner.Token, table: *Value.Table) !TableEntry {
            std.debug.assert(first_token.kind == .key);

            var parent_table: *Value.Table = table;
            var last_key_token: Scanner.Token = first_token;

            var created_table_value_root: ?*Value = null;
            errdefer if (created_table_value_root) |table_value| table_value.deinitRecursive(self.allocator);

            var next_inner_token = try self.scanner.next();
            var expecting_key = false;
            const first_value_token: Scanner.Token = while (next_inner_token) |inner_token| : (next_inner_token = try self.scanner.next()) {
                switch (inner_token.kind) {
                    .ignored,
                    .comment,
                    .table_start,
                    .table_end,
                    .many_table_start,
                    .many_table_end,
                    => unreachable,
                    .key => {
                        if (!expecting_key) return Error.UnexpectedToken;
                        const key_contents = self.scanner.tokenContents(last_key_token);
                        const nested_table_value = try table.getOrPut(self.allocator, key_contents);
                        if (nested_table_value.found_existing) {
                            if (nested_table_value.value_ptr.* != .table) return Error.InvalidKeyAccess;
                        } else {
                            nested_table_value.value_ptr.* = .{ .table = .empty };
                            created_table_value_root = nested_table_value.value_ptr;
                        }
                        parent_table = &nested_table_value.value_ptr.table;
                        last_key_token = inner_token;
                    },
                    .access => {
                        expecting_key = true;
                    },
                    .literal_string,
                    .literal_base_integer,
                    .literal_integer,
                    .literal_float,
                    .literal_inf,
                    .literal_nan,
                    .literal_bool,
                    .literal_offset_date_time,
                    .literal_local_date_time,
                    .literal_local_date,
                    .literal_local_time,
                    .array_start,
                    .inline_table_start,
                    .array_end,
                    .inline_table_end,
                    => {
                        if (expecting_key) return Error.UnexpectedToken;
                        break inner_token;
                    },
                }
            } else return Error.UnexpectedEof;

            const key_contents = self.scanner.tokenContents(last_key_token);
            const last_value = try table.getOrPut(self.allocator, key_contents);
            if (last_value.found_existing) return Error.DuplicateKey;

            return .{
                .first_value_token = first_value_token,
                .value_ptr = last_value.value_ptr,
            };
        }

        pub fn readValueToken(self: ParserT, token: Scanner.Token) !Value {
            const token_contents = self.scanner.tokenContents(token);
            return switch (token.kind) {
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
                .array_start => blk: {
                    var array_value: Value = .{ .array = .empty };
                    errdefer array_value.deinitRecursive(self.allocator);
                    var next_inner_token = try self.scanner.next();
                    while (next_inner_token) |inner_token| : (next_inner_token = try self.scanner.next()) {
                        if (inner_token.kind == .array_end) break;
                        try array_value.array.append(self.allocator, try self.readValueToken(inner_token));
                    }
                    break :blk array_value;
                },
                .inline_table_start => blk: {
                    var table_value: Value = .{ .table = .empty };
                    errdefer table_value.deinitRecursive(self.allocator);
                    var next_inner_token = try self.scanner.next();
                    while (next_inner_token) |inner_token| : (next_inner_token = try self.scanner.next()) {
                        if (inner_token.kind == .inline_table_end) break;
                        const table_entry = try self.readTableAccess(inner_token, &table_value.table);
                        table_entry.value_ptr.* = try self.readValueToken(table_entry.first_value_token);
                        errdefer table_entry.value_ptr.deinitRecursive(self.allocator);
                    }
                    break :blk table_value;
                },
            };
        }

        pub fn readRootTable(self: ParserT) !Value {
            var table_value: Value = .{ .table = .empty };
            errdefer table_value.deinitRecursive(self.allocator);
            var next_inner_token = try self.scanner.next();
            while (next_inner_token) |inner_token| : (next_inner_token = try self.scanner.next()) {
                if (inner_token.kind == .inline_table_end) break;
                const table_entry = try self.readTableAccess(inner_token, &table_value.table);
                table_entry.value_ptr.* = try self.readValueToken(table_entry.first_value_token);
                errdefer table_entry.value_ptr.deinitRecursive(self.allocator);
            }
            return table_value;
        }
    };
}

allocator: std.mem.Allocator,

test Parser {
    const buf: []const u8 =
        \\name = 5
    ;

    var scanner = Scanner{ .buffer = buf };
    var parser: Parser(Scanner) = .{ .allocator = std.testing.allocator, .scanner = &scanner };

    var root_table = try parser.readRootTable();
    defer root_table.deinitRecursive(std.testing.allocator);

    std.debug.print("table: {}\n", .{
        root_table.table.get("name").?.integer,
    });
}
