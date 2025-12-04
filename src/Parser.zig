const std = @import("std");

const Scanner = @import("Scanner.zig");

const rfc3339 = @import("rfc3339.zig");
const DateTime = rfc3339.DateTime;

const escape_string = @import("escape_string.zig");

const Parser = @This();

pub const Error = std.mem.Allocator.Error ||
    Scanner.Error ||
    error{ EndOfTokenStream, UnexpectedToken, AstError } ||
    error{ WriteFailed, InvalidEscape, InvalidUtf8 };

pub const Value = union(enum) {
    pub const Array = std.ArrayListUnmanaged(Value);
    pub const Table = std.StringArrayHashMapUnmanaged(Value);

    string: []const u8,
    bool: bool,
    integer: i64,
    integer_string: []const u8,
    float: f64,

    datetime: DateTime,

    array: Array,
    array_of_tables: Array,

    table: Table,
    inline_table: Table,
    implicit_table: Table,

    pub fn anyTableOrNull(self: Value) ?Table {
        return switch (self) {
            .table, .inline_table, .implicit_table => |table| table,
            else => null,
        };
    }

    pub fn anyArrayOrNull(self: Value) ?Array {
        return switch (self) {
            .array, .array_of_tables => |arr| arr,
            else => null,
        };
    }

    pub fn deinitDeep(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .string, .integer_string => |str| {
                allocator.free(str);
            },
            .bool, .integer, .float, .datetime => {},

            .array, .array_of_tables => |arr| {
                var arr_var = arr;
                for (arr_var.items) |value| value.deinitDeep(allocator);
                arr_var.deinit(allocator);
            },

            .table, .implicit_table, .inline_table => |table| {
                var table_var = table;
                for (table_var.values()) |value| value.deinitDeep(allocator);
                table_var.deinit(allocator);
            },
        }
    }
};

pub fn deinitTable(allocator: std.mem.Allocator, table: Value.Table) void {
    var value: Value = .{ .table = table };
    value.deinitDeep(allocator);
}

const KeySet = std.StringHashMapUnmanaged(void);

pub fn deinitKeySet(allocator: std.mem.Allocator, key_set: KeySet) void {
    var key_set_var = key_set;
    var iter = key_set_var.keyIterator();
    while (iter.next()) |key| {
        allocator.free(key.*);
    }
    key_set_var.deinit(allocator);
}

const AccessKind = enum {
    definition,
    pair,
};

allocator: std.mem.Allocator,
scanner: *Scanner,

key_allocator: std.mem.Allocator,
key_set: KeySet = .empty,

// we store the last token here for trailing comma detection in arrays- that is,
// a trailing comma then expects a value, but ] will return UnexpectedToken. catching
// this error and checking this value should reveal the end of the array
last_token: ?Scanner.Token = null,

fn parseBoolIdentifier(slice: []u8) error{UnexpectedToken}!bool {
    if (std.mem.eql(u8, slice, "true")) return true;
    if (std.mem.eql(u8, slice, "false")) return false;
    return error.UnexpectedToken;
}

pub fn deinit(parser: Parser) void {
    deinitKeySet(parser.key_allocator, parser.key_set);
}

fn getInternedKey(parser: *Parser, key: []const u8) ![]const u8 {
    const intern_result = try parser.key_set.getOrPut(parser.key_allocator, key);
    if (!intern_result.found_existing) {
        const distinct_key = try parser.key_allocator.dupe(u8, key);
        errdefer parser.allocator.free(distinct_key);
        intern_result.key_ptr.* = distinct_key;
    }
    return intern_result.key_ptr.*;
}

fn takeToken(parser: *Parser) !?Scanner.Token {
    parser.last_token = try parser.scanner.takeToken();
    return parser.last_token;
}

// Consumes ending ]
fn consumeArrayValues(parser: *Parser, array_value: *Value) !void {
    while (true) {
        const value_token = while (true) {
            const next_token = try parser.takeToken() orelse return error.EndOfTokenStream;
            if (next_token.kind == .newline) continue;
            if (next_token.kind == .comment) continue;
            break next_token;
        };
        if (value_token.kind == .table_or_array_end) break;

        const value = try parser.parseValue(value_token);
        errdefer value.deinitDeep(parser.allocator);
        try array_value.array.append(parser.allocator, value);

        const array_ended = while (true) {
            const delim_token = try parser.takeToken() orelse return error.EndOfTokenStream;
            if (delim_token.kind == .newline) continue;
            if (delim_token.kind == .comment) continue;
            if (delim_token.kind == .value_delimiter) break false;
            if (delim_token.kind == .table_or_array_end) break true;
            return error.UnexpectedToken;
        };
        if (array_ended) break;
    }
}

fn consumeInlineTableKeys(parser: *Parser, inline_table_value: *Value) !void {
    var first = true;
    while (true) {
        defer first = false;

        var key_depth: usize = 0;
        const parent_table, const key = parser.consumeAndAccessDeepKey(&inline_table_value.inline_table, .equals, .pair, &key_depth) catch |e| switch (e) {
            error.UnexpectedToken => {
                const last_token = parser.last_token.?;
                if (key_depth == 0 and last_token.kind == .inline_table_end) {
                    break;
                }
                return e;
            },
            else => return e,
        };
        const value = try parser.takeValue();
        errdefer value.deinitDeep(parser.allocator);

        _ = try parser.createTableValue(parent_table, key, value);

        const table_ended = while (true) {
            const delim_token = try parser.takeToken() orelse return error.EndOfTokenStream;
            if (delim_token.kind == .newline) continue;
            if (delim_token.kind == .comment) continue;
            if (delim_token.kind == .value_delimiter) break false;
            if (delim_token.kind == .inline_table_end) break true;
            return error.UnexpectedToken;
        };
        if (table_ended) break;
    }
}

fn consumeRootTableKeys(
    parser: *Parser,
    root_table_value: *Value,
    // see .consumeAndAccessDeepkey for why we take key_depth
    key_depth: *usize,
) !void {
    while (true) {
        key_depth.* = 0;
        const parent_table, const key = try parser.consumeAndAccessDeepKey(&root_table_value.table, .equals, .pair, key_depth);
        const value = try parser.takeValue();
        errdefer value.deinitDeep(parser.allocator);

        _ = try parser.createTableValue(parent_table, key, value);

        while (true) {
            const next_token = try parser.takeToken() orelse break;
            if (next_token.kind == .newline) break;
            if (next_token.kind == .comment) continue;
            return error.UnexpectedToken;
        }
    }
}

// Consumes ending =
fn consumeAndAccessDeepKey(
    parser: *Parser,
    root_table: *Value.Table,
    end_token_kind: Scanner.Token.Kind,
    access_kind: AccessKind,
    // this is a bit of a hack, but it's useful to know whether the _first_ identifier was really a [, for identifying
    // the start of an array-of-tables declaration, or whether it was really a newline
    depth: ?*usize,
) !struct { *Value.Table, []const u8 } {
    var expect_access = false;
    var parent_table: *Value.Table = root_table;
    var last_interned_key: ?[]const u8 = null;

    while (true) {
        const next_token = try parser.takeToken() orelse return error.EndOfTokenStream;
        if (next_token.kind == end_token_kind) break;
        switch (next_token.kind) {
            .equals,
            .value_delimiter,
            .offset_date_time,
            .local_date_time,
            .local_time,
            .table_or_array_start,
            .table_or_array_end,
            .inline_table_start,
            .inline_table_end,
            => {
                return error.UnexpectedToken;
            },
            .newline => {
                if (access_kind == .pair) continue;
            },

            .comment => {
                if (access_kind == .pair) {
                    const newline = try parser.takeToken() orelse return error.EndOfTokenStream;
                    if (newline.kind == .newline) continue;
                }
                return error.UnexpectedToken;
            },

            .access => {
                if (!expect_access) return error.UnexpectedToken;
                expect_access = false;
            },

            .identifier, .string, .literal_string, .integer, .base_integer, .float, .inf, .nan, .local_date => {
                // TODO: float keys! annoying!

                if (expect_access) return error.UnexpectedToken;

                if (last_interned_key) |key| {
                    const path_table_value = try parser.getOrCreateImplicitTableValuePath(parent_table, key, access_kind);

                    parent_table = resolve_parent: {
                        if (access_kind == .definition) {
                            if (path_table_value.* == .array_of_tables) {
                                break :resolve_parent &path_table_value.array_of_tables.items[path_table_value.array_of_tables.items.len - 1].table;
                            } else if (path_table_value.* == .table) {
                                break :resolve_parent &path_table_value.*.table;
                            }
                        }
                        break :resolve_parent &path_table_value.*.implicit_table;
                    };
                }

                if (next_token.kind == .string) {
                    const escaped = try escape_string.parseEscapedStringAlloc(parser.key_allocator, next_token.contents);
                    defer parser.key_allocator.free(escaped);

                    last_interned_key = try parser.getInternedKey(escaped);
                } else {
                    last_interned_key = try parser.getInternedKey(next_token.contents);
                }
                expect_access = true;
                if (depth) |d| d.* += 1;
            },
        }
    }

    // we reached the end before we encountered an identifier
    return .{ parent_table, last_interned_key orelse return error.UnexpectedToken };
}

fn getOrCreateImplicitTableValuePath(
    parser: *Parser,
    root_table_value: *Value.Table,
    interned_key: []const u8,
    access_kind: AccessKind,
) !*Value {
    const put_table_path_result = try root_table_value.getOrPut(parser.allocator, interned_key);
    if (put_table_path_result.found_existing) {
        const existing_value = put_table_path_result.value_ptr.*;
        switch (access_kind) {
            .definition => {
                if (existing_value != .implicit_table and existing_value != .table and existing_value != .array_of_tables) return error.AstError;
            },
            .pair => {
                if (existing_value != .implicit_table) return error.AstError;
            },
        }
    } else {
        put_table_path_result.value_ptr.* = .{ .implicit_table = .empty };
    }
    return put_table_path_result.value_ptr;
}

fn createTableValue(
    parser: *Parser,
    root_table_value: *Value.Table,
    interned_key: []const u8,
    value: Value,
) !*Value {
    const put_table_path_result = try root_table_value.getOrPut(parser.allocator, interned_key);
    if (put_table_path_result.found_existing) {
        return error.AstError;
    } else {
        put_table_path_result.value_ptr.* = value;
    }
    return put_table_path_result.value_ptr;
}

fn takeValue(parser: *Parser) Error!Value {
    const next_token = try parser.takeToken() orelse return error.EndOfTokenStream;
    return try parser.parseValue(next_token);
}

fn parseValue(parser: *Parser, next_token: Scanner.Token) Error!Value {
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
            const escaped_string = try escape_string.parseEscapedStringAlloc(parser.allocator, next_token.contents);
            errdefer parser.allocator.free(escaped_string);
            return .{ .string = escaped_string };
        },
        .literal_string => {
            const duped_string = try parser.allocator.dupe(u8, escape_string.trimInitialNewlineFromString(next_token.contents));
            errdefer parser.allocator.free(duped_string);
            return .{ .string = duped_string };
        },

        inline .integer, .base_integer => {
            // std.fmt.parseInt happens to satisfy the TOML spec for integers.
            const parsed_integer = std.fmt.parseInt(i64, next_token.contents, 0) catch |e| switch (e) {
                error.Overflow => {
                    const duped_string = try parser.allocator.dupe(u8, next_token.contents);
                    errdefer parser.allocator.free(duped_string);
                    return .{ .integer_string = duped_string };
                },
                error.InvalidCharacter => return error.AstError,
            };
            return .{ .integer = parsed_integer };
        },

        .float => {
            // std.fmt.parseFloat happens to satisfy the TOML spec for floats.
            const float = std.fmt.parseFloat(f64, next_token.contents) catch |e| switch (e) {
                error.InvalidCharacter => unreachable,
            };
            return .{ .float = float };
        },

        .inf => return switch (next_token.contents[0]) {
            '+' => .{ .float = std.math.inf(f64) },
            '-' => .{ .float = -std.math.inf(f64) },
            else => .{ .float = std.math.inf(f64) },
        },
        .nan => return switch (next_token.contents[0]) {
            '+' => .{ .float = std.math.nan(f64) },
            '-' => .{ .float = -std.math.nan(f64) },
            else => .{ .float = std.math.nan(f64) },
        },

        .offset_date_time => {
            return .{ .datetime = try rfc3339.parseOffsetDateTimeValue(next_token.contents) };
        },
        .local_date_time => {
            return .{ .datetime = try rfc3339.parseLocalDateTimeValue(next_token.contents) };
        },
        .local_date => {
            return .{ .datetime = try rfc3339.parseLocalDateValue(next_token.contents) };
        },
        .local_time => {
            return .{ .datetime = try rfc3339.parseLocalTimeValue(next_token.contents) };
        },

        .table_or_array_start => {
            var array_value: Value = .{ .array = .empty };
            try parser.consumeArrayValues(&array_value);
            errdefer array_value.deinitDeep(parser.allocator);
            return array_value;
        },

        .inline_table_start => {
            var inline_table_value: Value = .{ .inline_table = .empty };
            try parser.consumeInlineTableKeys(&inline_table_value);
            errdefer inline_table_value.deinitDeep(parser.allocator);
            return inline_table_value;
        },
    }
}

fn consumeTableDefinitionDeepKey(
    parser: *Parser,
    root_table_value: *Value,
) (Error || error{ArrayOfTablesDefinition})!struct { *Value.Table, []const u8 } {
    var key_depth: usize = 0;
    return parser.consumeAndAccessDeepKey(&root_table_value.table, .table_or_array_end, .definition, &key_depth) catch |e| switch (e) {
        error.UnexpectedToken => {
            if (key_depth == 0) {
                const token = parser.last_token orelse return error.UnexpectedToken;
                if (token.kind == .table_or_array_start) {
                    return error.ArrayOfTablesDefinition;
                }
            }
            return error.UnexpectedToken;
        },
        else => return e,
    };
}

fn consumeArrayOfTablesDefinition(parser: *Parser, root_table_value: *Value) Error!*Value {
    const parent_table, const key = parser.consumeTableDefinitionDeepKey(root_table_value) catch |e| switch (e) {
        error.ArrayOfTablesDefinition => return error.UnexpectedToken,
        else => |f| return f,
    };

    const put_table_path_result = try parent_table.getOrPut(parser.allocator, key);
    if (put_table_path_result.found_existing) {
        const existing_value = put_table_path_result.value_ptr.*;
        if (existing_value != .array_of_tables) return error.AstError;
    } else {
        put_table_path_result.value_ptr.* = .{ .array_of_tables = .empty };
    }

    const array_value = put_table_path_result.value_ptr;
    const value = try array_value.array_of_tables.addOne(parser.allocator);
    value.* = .{ .table = .empty };
    return value;
}

fn consumeTableDefinition(parser: *Parser, root_table_value: *Value) Error!*Value {
    const parent_table, const key = parser.consumeTableDefinitionDeepKey(root_table_value) catch |e| switch (e) {
        error.ArrayOfTablesDefinition => {
            return try parser.consumeArrayOfTablesDefinition(root_table_value);
        },
        else => |f| return f,
    };

    const put_table_path_result = try parent_table.getOrPut(parser.allocator, key);
    if (put_table_path_result.found_existing) {
        const existing_value = put_table_path_result.value_ptr.*;
        if (existing_value == .implicit_table) {
            put_table_path_result.value_ptr.* = .{ .table = existing_value.implicit_table };
        } else return error.AstError;
    } else {
        put_table_path_result.value_ptr.* = .{ .table = .empty };
    }
    return put_table_path_result.value_ptr;
}

pub fn takeDocumentTable(parser: *Parser) !Value.Table {
    var document_table_value: Value = .{ .table = .empty };
    errdefer document_table_value.deinitDeep(parser.allocator);
    var root_table_value = &document_table_value;

    while (true) {
        var key_depth: usize = 0;
        parser.consumeRootTableKeys(root_table_value, &key_depth) catch |e| switch (e) {
            error.UnexpectedToken => {
                if (key_depth == 0) {
                    const token = parser.last_token orelse return error.UnexpectedToken;
                    if (token.kind == .table_or_array_start) {
                        root_table_value = try parser.consumeTableDefinition(&document_table_value);
                        const next_token = try parser.takeToken() orelse return error.EndOfTokenStream;
                        if (next_token.kind == .newline) continue;
                        continue;
                    }
                    if (token.kind == .newline) continue;
                }
                return e;
            },
            error.EndOfTokenStream => {
                if (key_depth == 0) break;
                return e;
            },
            else => return e,
        };
    }

    return document_table_value.table;
}

test Parser {
    // const buf: []const u8 =
    //     \\barney.name = "Barney"
    //     \\barney.age = 16
    //     \\barney.breed = "unknown"
    //     \\
    //     \\sprout = { name = "Sprout", age = 15, breed = "cairn-terrier x jack russell" }
    //     \\
    //     \\[barney.colours]
    //     \\head = "white"
    //     \\body = "brown"
    //     \\tail = "red"
    //     \\
    //     \\[[other_dog]]
    //     \\name = "Bo"
    //     \\colour = "White"
    //     \\origin = "Egypt"
    //     \\
    //     \\[[other_dog]]
    //     \\name = "Lala"
    //     \\colour = "Black"
    //     \\origin = "Serbia"
    //     \\
    //     \\[other_dog.colours]
    //     \\head = "black"
    //     \\body = "black"
    //     \\tail = "black"
    // ;
    //

    const buf: []const u8 =
        \\barney.name = "Barney"
        \\barney.age = 17
        \\
        \\sprout = { name = "Sprout", age = 15, breed = "cairn terrier x jack russell" }
        \\
        \\[barney.colours]
        \\head = "white"
        \\
        \\[[other_dog]]
        \\name = "Bo"
        \\
        \\[[other_dog]]
        \\name = "Lala"
        \\
        \\[other_dog.meta]
        \\age = 3
    ;

    var reader: std.Io.Reader = .fixed(buf);

    var scanner = Scanner{ .reader = &reader };
    var parser: Parser = .{
        .allocator = std.testing.allocator,
        .key_allocator = std.testing.allocator,
        .key_set = .empty,
        .scanner = &scanner,
    };
    defer parser.deinit();

    const root_table = try parser.takeDocumentTable();
    defer deinitTable(std.testing.allocator, root_table);

    try std.testing.expectEqualSlices(u8, "Barney", root_table.get("barney").?.implicit_table.get("name").?.string);
    try std.testing.expectEqual(17, root_table.get("barney").?.implicit_table.get("age").?.integer);
    try std.testing.expectEqualSlices(u8, "Sprout", root_table.get("sprout").?.inline_table.get("name").?.string);
    try std.testing.expectEqual(15, root_table.get("sprout").?.inline_table.get("age").?.integer);

    try std.testing.expectEqualSlices(u8, "Bo", root_table.get("other_dog").?.array_of_tables.items[0].table.get("name").?.string);
    try std.testing.expectEqualSlices(u8, "Lala", root_table.get("other_dog").?.array_of_tables.items[1].table.get("name").?.string);
    try std.testing.expectEqual(3, root_table.get("other_dog").?.array_of_tables.items[1].table.get("meta").?.table.get("age").?.integer);
}
