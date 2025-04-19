const std = @import("std");

const toml_test_files = @import("toml_test_files");
const microwave = @import("microwave");

pub fn checkIncorrectToml(arena: std.mem.Allocator, toml_data: []const u8) !bool {
    _ = microwave.parse.fromSlice(arena, toml_data) catch return true;
    return false;
}

pub fn validateCorrectToml(arena: std.mem.Allocator, toml_data: []const u8, json_validate_data: []const u8) !bool {
    const doc = microwave.parse.fromSlice(arena, toml_data) catch return false;

    _ = doc;

    _ = json_validate_data;
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
    try stdout_writer.print("{}/{}\n", .{ pass, pass + fail });
}
