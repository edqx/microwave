const std = @import("std");

const Scanner = @import("Scanner.zig");

pub const Value = union(enum) {
    pub const Table = std.StringArrayHashMapUnmanaged(Value);
    pub const Array = std.ArrayListUnmanaged(Value);
    pub const ArrayOfTables = std.ArrayListUnmanaged(Table);

    pub const DateTime = union(enum) {
        pub const Year = std.math.IntFittingRange(0, 10000);
        pub const Month = std.math.IntFittingRange(1, 12);
        pub const Day = std.math.IntFittingRange(1, 31);

        pub const Hour = std.math.IntFittingRange(0, 23);
        pub const OffsetHour = std.math.IntFittingRange(-24, 24);
        pub const Minute = std.math.IntFittingRange(0, 59);
        pub const Second = std.math.IntFittingRange(0, 59);
        pub const Millisecond = u64;

        pub const Date = struct {
            year: Year,
            month: Month,
            day: Day,
        };

        pub const Offset = struct {
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
                    for (table_value.values()) |*item_ptr| item_ptr.deinitRecursive(allocator);
                    table_value.deinit(allocator);
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
    for (table_value.values()) |*item_ptr| item_ptr.deinitRecursive(allocator);
    table_value.deinit(allocator);
}

pub fn Parser(ScannerType: type) type {
    return struct {
        const ParserT = @This();

        pub const Error = std.mem.Allocator.Error ||
            std.fmt.ParseIntError ||
            std.fmt.ParseFloatError ||
            ScannerType.Error ||
            error{ InvalidUtf8, Utf8ExpectedContinuation, Utf8OverlongEncoding, Utf8EncodesSurrogateHalf, Utf8CodepointTooLarge } ||
            error{ UnexpectedToken, UnexpectedEof, UnexpectedEol, InvalidKeyAccess, DuplicateKey, InvalidDateTime, LeadingZero };

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
            var result: std.ArrayListUnmanaged(u8) = try .initCapacity(self.allocator, token_contents.len);
            defer result.deinit(self.allocator);

            var writer = result.writer(self.allocator);

            const codepoints = try std.unicode.Utf8View.init(token_contents);
            var iter = codepoints.iterator();

            while (iter.nextCodepointSlice()) |slice| {
                const decoded_codepoint = try std.unicode.utf8Decode(slice);

                if (decoded_codepoint == '\\') {
                    // handle escapes
                }

                _ = try writer.write(slice);
            }

            const string_contents = try result.toOwnedSlice(self.allocator);
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

        pub fn parseOffsetDateTimeValue(self: ParserT, token_contents: []const u8) !Value {
            const offset_index = if (std.mem.lastIndexOfAny(u8, token_contents, "Zz") == token_contents.len - 1)
                token_contents.len - 1
            else blk: {
                const offset_separator = std.mem.lastIndexOfAny(u8, token_contents, "+-") orelse unreachable;
                break :blk offset_separator;
            };

            const datetime_contents = token_contents[0..offset_index];
            const offset_contents = token_contents[offset_index..];

            const local_date_time = try self.parseLocalDateTimeValue(datetime_contents);

            const offset: Value.DateTime.Offset = if (offset_contents.len == 1 and (offset_contents[0] == 'Z' or offset_contents[0] == 'z')) .{
                .hour = 0,
                .minute = 0,
            } else parse_offset: {
                var time_parts = std.mem.tokenizeAny(u8, offset_contents, ":");

                const hour_part = time_parts.next().?;
                if (hour_part.len < 2) return Error.InvalidDateTime;
                const minute_part = time_parts.next().?;
                if (minute_part.len < 2) return Error.InvalidDateTime;

                var offset: Value.DateTime.Offset = .{
                    .hour = 0,
                    .minute = 0,
                };
                offset.hour = std.fmt.parseInt(Value.DateTime.OffsetHour, hour_part, 10) catch return Error.InvalidDateTime;
                if (offset.hour <= -24 or offset.hour >= 24) return Error.InvalidDateTime;
                offset.minute = std.fmt.parseInt(Value.DateTime.Minute, minute_part, 10) catch return Error.InvalidDateTime;
                if (offset.minute >= 60) return Error.InvalidDateTime;

                break :parse_offset offset;
            };

            return .{
                .date_time = .{
                    .offset_date_time = .{
                        .date = local_date_time.date_time.local_date_time.date,
                        .time = local_date_time.date_time.local_date_time.time,
                        .offset = offset,
                    },
                },
            };
        }

        pub fn parseLocalDateTimeValue(self: ParserT, token_contents: []const u8) !Value {
            const time_index = (std.mem.indexOfAny(u8, token_contents, "Tt ") orelse unreachable) + 1;
            const date_contents = token_contents[0 .. time_index - 1];
            const time_contents = token_contents[time_index..];

            const local_date_value = try self.parseLocalDateValue(date_contents);
            const local_time_value = try self.parseLocalTimeValue(time_contents);

            return .{
                .date_time = .{
                    .local_date_time = .{
                        .date = local_date_value.date_time.just_date,
                        .time = local_time_value.date_time.just_time,
                    },
                },
            };
        }

        pub fn parseLocalDateValue(self: ParserT, token_contents: []const u8) !Value {
            _ = self;

            var time_parts = std.mem.tokenizeAny(u8, token_contents, "-");

            const year_part = time_parts.next().?;
            const month_part = time_parts.next().?;
            if (month_part.len < 2) return Error.InvalidDateTime;

            const day_part = time_parts.next().?;
            if (day_part.len < 2) return Error.InvalidDateTime;

            var date: Value.DateTime.Date = .{
                .year = 0,
                .month = 0,
                .day = 0,
            };
            date.year = std.fmt.parseInt(Value.DateTime.Year, year_part, 10) catch return Error.InvalidDateTime;
            if (date.year >= 10_000) return Error.InvalidDateTime;
            date.month = std.fmt.parseInt(Value.DateTime.Month, month_part, 10) catch return Error.InvalidDateTime;
            if (date.month < 1 or date.month > 12) return Error.InvalidDateTime;
            date.day = std.fmt.parseInt(Value.DateTime.Day, day_part, 10) catch return Error.InvalidDateTime;
            if (date.day < 1 or date.day > 31) return Error.InvalidDateTime;

            const is_leap_year = (date.year % 4 == 0) and (date.year % 100 != 0 or date.year % 400 == 0);
            const month_dates: []const usize = &.{ 31, if (is_leap_year) 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
            if (date.day > month_dates[date.month - 1]) return Error.InvalidDateTime;

            return .{
                .date_time = .{
                    .just_date = date,
                },
            };
        }

        pub fn parseLocalTimeValue(self: ParserT, token_contents: []const u8) !Value {
            _ = self;

            var millisecond_parts = std.mem.tokenizeAny(u8, token_contents, ".");

            const non_millisecond_part = millisecond_parts.next().?;
            var time_parts = std.mem.tokenizeAny(u8, non_millisecond_part, ":");

            const hour_part = time_parts.next().?;
            if (hour_part.len < 2) return Error.InvalidDateTime;
            const minute_part = time_parts.next().?;
            if (minute_part.len < 2) return Error.InvalidDateTime;

            const second_part = time_parts.next();
            const millisecond_part = millisecond_parts.next();

            var time: Value.DateTime.Time = .{
                .hour = 0,
                .minute = 0,
                .second = null,
                .millisecond = null,
            };
            time.hour = std.fmt.parseInt(Value.DateTime.Hour, hour_part, 10) catch return Error.InvalidDateTime;
            if (time.hour >= 24) return Error.InvalidDateTime;
            time.minute = std.fmt.parseInt(Value.DateTime.Minute, minute_part, 10) catch return Error.InvalidDateTime;
            if (time.minute >= 60) return Error.InvalidDateTime;
            if (second_part) |second_str| {
                if (second_str.len < 2) return Error.InvalidDateTime;
                time.second = std.fmt.parseInt(Value.DateTime.Second, second_str, 10) catch return Error.InvalidDateTime;
                if (time.second.? >= 60) return Error.InvalidDateTime;
                if (millisecond_part) |millisecond_str| {
                    time.millisecond = std.fmt.parseInt(Value.DateTime.Millisecond, millisecond_str, 10) catch return Error.InvalidDateTime;
                }
            }

            return .{
                .date_time = .{
                    .just_time = time,
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

            while (try self.consumeToken(.newline) != null) {
                try self.nextToken();
            }
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

                while (try self.consumeToken(.newline) != null) try self.nextToken();
                if (try self.consumeToken(.delimeter) == null) {
                    while (try self.consumeToken(.newline) != null) try self.nextToken();
                    try self.expectToken(.array_end);
                    break;
                }
                try self.nextToken();
                while (try self.consumeToken(.newline) != null) try self.nextToken();
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

            while (try self.consumeToken(.newline) != null) try self.nextToken();
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

                while (try self.consumeToken(.newline) != null) try self.nextToken();
                if (try self.consumeToken(.delimeter) == null) {
                    while (try self.consumeToken(.newline) != null) try self.nextToken();
                    try self.expectToken(.inline_table_end);
                    break;
                }
                try self.nextToken();
                while (try self.consumeToken(.newline) != null) try self.nextToken();
                if (try self.consumeToken(.inline_table_end) != null) break;
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
                .literal_offset_date_time => try self.parseOffsetDateTimeValue(token_contents),
                .literal_local_date_time => try self.parseLocalDateTimeValue(token_contents),
                .literal_local_date => try self.parseLocalDateValue(token_contents),
                .literal_local_time => try self.parseLocalTimeValue(token_contents),
            };
            return value;
        }

        pub fn readRootTableValue(self: *ParserT) !Value.Table {
            var root_table: Value = .{ .table = .empty };
            errdefer deinitTable(self.allocator, &root_table.table);

            var active_table = &root_table.table;

            var i: usize = 0;
            while (true) : (i += 1) {
                self.scanner.setState(.root);
                try self.nextToken();

                if (self.isEof()) break;

                if (i > 0) {
                    self.expectToken(.newline) catch |e| switch (e) {
                        Error.UnexpectedToken => if (self.isEof()) break else return e,
                        else => return e,
                    };
                    try self.nextToken();
                }

                while (try self.consumeToken(.newline) != null) try self.nextToken();
                if (self.isEof()) break;

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
        \\# Top comment.
        \\  # Top comment.
        \\# Top comment.
        \\
        \\# [no-extraneous-groups-please]
        \\
        \\[group] # Comment
        \\answer = 42 # Comment
        \\# no-extraneous-keys-please = 999
        \\# Inbetween comment.
        \\more = [ # Comment
        \\  # What about multiple # comments?
        \\  # Can you handle it?
        \\  #
        \\          # Evil.
        \\# Evil.
        \\  42, 42, # Comments within arrays are fun.
        \\  # What about multiple # comments?
        \\  # Can you handle it?
        \\  #
        \\          # Evil.
        \\# Evil.
        \\# ] Did I fool you?
        \\] # Hopefully not.
        \\
        \\# Make sure the space between the datetime and "#" isn't lexed.
        \\dt = 1979-05-27T07:32:12-07:00  # c
        \\d = 1979-05-27 # Comment
    );
    defer res.deinit();
}
