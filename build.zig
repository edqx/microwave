const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("microwave", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    _ = mod;

    const test_mod = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
    });

    const test_step = b.step("test", "Test Microwave");
    test_step.dependOn(&b.addRunArtifact(test_mod).step);
}
