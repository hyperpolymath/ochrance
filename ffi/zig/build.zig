// SPDX-License-Identifier: PMPL-1.0-or-later
//
// Minimum Zig version: 0.11.0
// Required for: std.crypto.hash.Blake3, C ABI export, @memcpy builtin
//
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Build libochrance.so shared library
    const lib = b.addStaticLibrary(.{
        .name = "ochrance",
        .root_source_file = .{ .cwd_relative = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);

    // Also create a shared library version
    const shared_lib = b.addStaticLibrary(.{
        .name = "ochrance-shared",
        .root_source_file = .{ .cwd_relative = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    shared_lib.linkage = .dynamic;
    b.installArtifact(shared_lib);

    // Tests
    const tests = b.addTest(.{
        .root_source_file = .{ .cwd_relative = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);
}
