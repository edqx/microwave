const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("microwave", .{
        .root_source_file = b.path("src/microwave.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_exe = b.addTest(.{
        .root_module = mod,
    });

    const test_step = b.step("test", "Test Microwave");
    test_step.dependOn(&b.addRunArtifact(test_exe).step);

    const docs_lib = b.addLibrary(.{
        .name = "microwave",
        .root_module = mod,
    });

    const install_docs = b.addInstallDirectory(.{
        .source_dir = docs_lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
        .exclude_extensions = &.{".html"},
    });

    const install_html = b.addInstallFile(b.path("docs/index.html"), "docs/index.html");

    const docs_step = b.step("docs", "Generate docs");
    docs_step.dependOn(&install_docs.step);
    docs_step.dependOn(&install_html.step);
}
