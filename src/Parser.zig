const std = @import("std");

const Scanner = @import("Scanner.zig");

const Parser = @This();

const Error = error{ EndOfTokenStream, UnexpectedToken };

const DeepKey = std.ArrayListUnmanaged([]const u8);

const Value = struct {
    const Array = std.ArrayListUnmanaged(Value);
    const Table = std.StringHashMapUnmanaged(Value);

    string: []const u8,
    bool: bool,
    integer: i64,
    integer_string: []const u8,
    float: f64,

    datetime: []const u8,

    array: Array,
    inline_table: Table,

    pub fn deinitDeep(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            inline .string, .integer_string, .datetime => |str| allocator.free(str),
            .bool => {},
        }
    }
};

allocator: std.mem.Allocator,
scanner: *Scanner,

// we store the last token here for trailing comma detection in arrays- that is,
// a trailing comma then expects a value, but ] will return UnexpectedToken. catching
// this error and checking this value should reveal the end of the array
last_token: ?Scanner.Token,

fn parseBoolIdentifier(slice: []u8) error{UnexpectedIdentifier}!bool {
    if (std.mem.eql(u8, slice, "true")) return true;
    if (std.mem.eql(u8, slice, "false")) return false;
    return error.UnexpectedIdentifier;
}

fn takeToken(self: *Parser) !?Scanner.Token {
    self.last_token = try self.scanner.takeToken();
    return self.last_token;
}

// Consumes ending ]
fn consumeArrayValues(self: *Parser, array_value: *Value) !void {
    while (true) {
        const value = self.takeValue() catch |e| {
            switch (e) {
                error.UnexpectedToken => {
                    const last_token = self.last_token orelse return e;
                    if (last_token.?.kind == .table_or_array_end) break;
                },
            }
            return e;
        };

        try array_value.array.append(self.allocator, value);
    }
}

fn consumeInlineTableKeys(self: *Parser, inline_table_value: *Value) !void {
    while (true) {}
}

// Consumes ending =
fn consumeAndAccessDeepKey(self: *Parser, table: *Value.Table) !*Value {
    while (true) {
        const next_token = try self.takeToken() orelse return error.EndOfTokenStream;
        switch (next_token.kind) {
            .newline,
            .comment,
            .value_delimiter,
            .offset_date_time,
            .local_date_time,
            .local_date,
            .local_time,
            .table_or_array_start,
            .table_or_array_end,
            .inline_table_start,
            .inline_table_end,
            => return error.UnexpectedToken,

            .identifier, .literal_string, .integer, .base_integer, .float => {},

            .string => {
                // TODO: escape
            },
        }
    }
}

fn takeValue(self: *Parser) !Value {
    const next_token = try self.takeToken() orelse return error.EndOfTokenStream;
    switch (next_token.kind) {
        .newline,
        .comment,
        .access,
        .equals,
        .value_delimiter,
        .table_or_array_end,
        .inline_table_end,
        => return error.UnexpectedToken,

        .identifier => { // may be 'true' or 'false' for values
            return .{ .bool = try parseBoolIdentifier(next_token.contents) };
        },

        .string => {
            const duped_string = try self.allocator.dupe(u8, next_token.contents);
            errdefer self.allocator.free(duped_string);
            // TODO: handle escapes
            return .{ .string = duped_string };
        },
        .literal_string => {
            const duped_string = try self.allocator.dupe(u8, next_token.contents);
            errdefer self.allocator.free(duped_string);
            return .{ .string = duped_string };
        },

        inline .integer, .base_integer => {
            // std.fmt.parseInt happens to satisfy the TOML spec for integers.
            const parsed_integer = try std.fmt.parseInt(i64, next_token.contents, 0) catch |e| switch (e) {
                error.Overflow => {
                    const duped_string = try self.allocator.dupe(u8, next_token.contents);
                    errdefer self.allocator.free(duped_string);
                    return .{ .integer_string = duped_string };
                },
                error.InvalidCharacter => unreachable,
            };
            return .{ .integer = parsed_integer };
        },

        .float => {
            // std.fmt.parseFloat happens to satisfy the TOML spec for floats.
            const float = try std.fmt.parseFloat(f64, next_token.contents) catch |e| switch (e) {
                error.InvalidCharacter => unreachable,
            };
            return .{ .float = float };
        },

        .positive_inf => {
            return .{ .float = std.math.inf(f64) };
        },
        .negative_inf => {
            return .{ .float = -std.math.inf(f64) };
        },
        .positive_nan => {
            return .{ .float = std.math.nan(f64) };
        },
        .negative_nan => {
            return .{ .float = -std.math.nan(f64) };
        },

        inline .offset_date_time, .local_date_time, .local_date, .local_time => {
            // TODO: actually parse date time (invalid dates don't satisfy TOML spec)
            const duped_string = try self.allocator.dupe(u8, next_token.contents);
            errdefer self.allocator.free(duped_string);
            return .{ .string = duped_string };
        },

        .table_or_array_start => {
            var array_value: Value = .{ .array = .empty };
            try self.consumeArrayValues(&array_value);
            return array_value;
        },
    }
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
