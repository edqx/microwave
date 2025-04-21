const std = @import("std");

const toml_test_files = @import("toml_test_files");
const microwave = @import("microwave");

pub const FormatDate = std.fmt.Formatter(struct {
    pub fn fmt(
        date: microwave.parse.Value.DateTime.Date,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{d:0>4}-{d:0>2}-{d:0>2}", .{ date.year, date.month, date.day });
    }
}.fmt);

pub const FormatTime = std.fmt.Formatter(struct {
    pub fn fmt(
        time: microwave.parse.Value.DateTime.Time,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{d:0>2}:{d:0>2}", .{ time.hour, time.minute });
        if (time.second) |second| {
            try writer.print(":{d:0>2}", .{second});
            if (time.millisecond) |millisecond|
                try writer.print(".{d:0<3}", .{millisecond});
        } else {
            try writer.print(":00", .{});
        }
    }
}.fmt);

pub const FormatOffset = std.fmt.Formatter(struct {
    pub fn fmt(
        offset: microwave.parse.Value.DateTime.Offset,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (offset.isUtc()) {
            try writer.print("Z", .{});
        } else {
            try writer.print("{s}{d:0>2}:{d:0>2}", .{
                if (offset.negative) "-" else "+",
                offset.hour,
                offset.minute,
            });
        }
    }
}.fmt);

pub fn checkIncorrectToml(arena: std.mem.Allocator, toml_data: []const u8) !bool {
    _ = microwave.parse.fromSlice(arena, toml_data) catch return true;
    return false;
}

fn jsonTomlEql(arena: std.mem.Allocator, json_value: std.json.Value, toml_value: microwave.parse.Value) !bool {
    if (json_value == .array) {
        switch (toml_value) {
            inline .array, .array_of_tables => |inner_array, tag| {
                if (json_value.array.items.len != inner_array.items.len) return false;
                for (json_value.array.items, inner_array.items) |json_elem, toml_elem| {
                    const toml_elem_value: microwave.parse.Value = switch (tag) {
                        .array => toml_elem,
                        .array_of_tables => .{ .table = toml_elem },
                        else => unreachable,
                    };
                    if (!try jsonTomlEql(arena, json_elem, toml_elem_value)) return false;
                }
                return true;
            },
            else => return false,
        }
    }

    const json_object = json_value.object;

    if (!json_object.contains("type") or !json_object.contains("value")) {
        if (toml_value != .table) return false;
        var iterator = json_object.iterator();
        while (iterator.next()) |json_entry| {
            const toml_entry_value = toml_value.table.get(json_entry.key_ptr.*) orelse return false;
            if (!try jsonTomlEql(arena, json_entry.value_ptr.*, toml_entry_value)) return false;
        }

        var iterator2 = toml_value.table.iterator();
        while (iterator2.next()) |toml_entry| {
            if (!json_object.contains(toml_entry.key_ptr.*)) return false;
        }

        return true;
    }

    const json_type = json_object.get("type").?.string;
    const inner_value = json_object.get("value").?.string;

    if (std.mem.eql(u8, json_type, "string")) {
        if (toml_value != .string) return false;
    } else if (std.mem.eql(u8, json_type, "integer")) {
        if (toml_value != .integer) return false;
    } else if (std.mem.eql(u8, json_type, "float")) {
        if (toml_value != .float) return false;
        // floats are messed up in the toml json validation documents, let's just
        // parse them and check them directly
        if (std.mem.eql(u8, inner_value, "nan")) return std.math.isNan(toml_value.float);
        return std.fmt.parseFloat(f64, inner_value) catch unreachable == toml_value.float;
    } else if (std.mem.eql(u8, json_type, "bool")) {
        if (toml_value != .boolean) return false;
    } else if (std.mem.eql(u8, json_type, "datetime")) {
        if (toml_value != .date_time or toml_value.date_time != .offset_date_time) return false;
    } else if (std.mem.eql(u8, json_type, "datetime-local")) {
        if (toml_value != .date_time or toml_value.date_time != .local_date_time) return false;
    } else if (std.mem.eql(u8, json_type, "date-local")) {
        if (toml_value != .date_time or toml_value.date_time != .just_date) return false;
    } else if (std.mem.eql(u8, json_type, "time-local")) {
        if (toml_value != .date_time or toml_value.date_time != .just_time) return false;
    }

    const stringified_value = switch (toml_value) {
        .none, .table, .array, .array_of_tables => unreachable,
        .string => |val| val,
        .integer => |int| try std.fmt.allocPrint(arena, "{d}", .{int}),
        .float => |float| try std.fmt.allocPrint(arena, "{d}", .{float}),
        .boolean => |boolean| try std.fmt.allocPrint(arena, "{}", .{boolean}),
        .date_time => |date_time| switch (date_time) {
            .just_date => |date| try std.fmt.allocPrint(arena, "{}", .{@as(FormatDate, .{ .data = date })}),
            .just_time => |time| try std.fmt.allocPrint(arena, "{}", .{@as(FormatTime, .{ .data = time })}),
            .local_date_time => |both| try std.fmt.allocPrint(arena, "{}T{}", .{
                @as(FormatDate, .{ .data = both.date }),
                @as(FormatTime, .{ .data = both.time }),
            }),
            .offset_date_time => |all| try std.fmt.allocPrint(arena, "{}T{}{}", .{
                @as(FormatDate, .{ .data = all.date }),
                @as(FormatTime, .{ .data = all.time }),
                @as(FormatOffset, .{ .data = all.offset }),
            }),
        },
    };

    return std.mem.eql(u8, inner_value, stringified_value);
}

pub fn validateCorrectToml(arena: std.mem.Allocator, toml_data: []const u8, json_validate_data: []const u8) !bool {
    const doc = microwave.parse.fromSlice(arena, toml_data) catch return false;
    const json_doc = try std.json.parseFromSlice(std.json.Value, arena, json_validate_data, .{});

    if (!try jsonTomlEql(arena, json_doc.value, .{ .table = doc.root_table })) return false;

    return true;
}

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer std.debug.assert(gpa.deinit() == .ok);

    const stdout_writer = std.io.getStdOut().writer();

    const allocator = gpa.allocator();

    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();

    var failed_tests: std.ArrayListUnmanaged([]const u8) = try .initCapacity(arena.allocator(), @typeInfo(toml_test_files).@"struct".decls.len);
    defer failed_tests.deinit(arena.allocator());

    var pass: usize = 0;
    var fail: usize = 0;

    inline for (@typeInfo(toml_test_files).@"struct".decls) |decl| {
        if (std.mem.endsWith(u8, decl.name, ".toml")) {
            const json_name = decl.name[0 .. decl.name.len - 5] ++ ".json";

            const toml_data = @field(toml_test_files, decl.name);

            std.debug.print("========TESTING {s}....\n", .{decl.name});

            if (@hasDecl(toml_test_files, json_name)) {
                const json_validate_data = @field(toml_test_files, json_name);
                if (try validateCorrectToml(arena.allocator(), toml_data, json_validate_data)) {
                    pass += 1;
                } else {
                    fail += 1;
                    failed_tests.appendAssumeCapacity(decl.name);
                }
            } else {
                if (try checkIncorrectToml(arena.allocator(), toml_data)) {
                    pass += 1;
                } else {
                    fail += 1;
                    failed_tests.appendAssumeCapacity(decl.name);
                }
            }
        }
    }

    for (failed_tests.items) |failed_test_path| {
        try stdout_writer.print("- fail: {s}\n", .{failed_test_path});
    }
    try stdout_writer.print("passing: {}/{}\n", .{ pass, pass + fail });
}
