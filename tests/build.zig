const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const microwave_dependency = b.dependency("microwave", .{});

    const generated_test_file_path = try generateTestFile(b);

    const test_files_module = b.createModule(.{
        .root_source_file = generated_test_file_path,
    });

    const mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    mod.addImport("toml_test_files", test_files_module);
    mod.addImport("microwave", microwave_dependency.module("microwave"));

    const exe = b.addExecutable(.{
        .name = "microwave_tests",
        .root_module = mod,
    });

    const run_test = b.addRunArtifact(exe);
    b.default_step.dependOn(&run_test.step);
}

fn generateTestFile(b: *std.Build) !std.Build.LazyPath {
    const toml_test_dependency = b.dependency("toml_test", .{});

    const toml_files_list_path = toml_test_dependency.builder.pathJoin(&.{ "tests", "files-toml-1.1.0" });
    const toml_files_list_data = try toml_test_dependency.builder.build_root.handle.readFileAlloc(b.allocator, toml_files_list_path, std.math.maxInt(usize));
    defer b.allocator.free(toml_files_list_data);

    var generated_data: std.ArrayListUnmanaged(u8) = .empty;
    errdefer generated_data.deinit(b.allocator);

    const writer = generated_data.writer(b.allocator);

    var test_paths = std.mem.tokenizeAny(u8, toml_files_list_data, &std.ascii.whitespace);
    while (test_paths.next()) |test_path| {
        const test_data = try toml_test_dependency.builder.build_root.handle.readFileAlloc(b.allocator, toml_test_dependency.builder.pathJoin(&.{ "tests", test_path }), std.math.maxInt(usize));
        defer b.allocator.free(test_data);

        try writer.print("pub const @\"{s}\": []const u8 = &.{any};\n", .{ test_path, test_data });

        // var test_lines = std.mem.tokenizeAny(u8, test_data, "\r\n");
        // while (test_lines.next()) |test_line| {
        //     try writer.print("    \\\\{s}\n", .{test_line});
        // }
        // try writer.print("\n;", .{});
    }

    const write_files = b.addWriteFiles();
    return write_files.add("test_file_data.zig", generated_data.items);
}
