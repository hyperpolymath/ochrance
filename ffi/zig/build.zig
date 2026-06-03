// SPDX-License-Identifier: MPL-2.0
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

    lib.linkLibC(); // std.heap.c_allocator + C ABI
    b.installArtifact(lib);

    // Shared library (libochrance.so) — this is what the Idris2 %foreign
    // declarations link against at runtime (C:blake3_hash,libochrance ...).
    const shared_lib = b.addSharedLibrary(.{
        .name = "ochrance",
        .root_source_file = .{ .cwd_relative = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    shared_lib.linkLibC();
    b.installArtifact(shared_lib);

    // Tests
    const tests = b.addTest(.{
        .root_source_file = .{ .cwd_relative = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    tests.linkLibC();

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);
}
