// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
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

    // libc is required by std.heap.c_allocator (used by the handle lifecycle)
    // and is the natural choice for a C-ABI FFI library.
    lib.linkLibC();
    b.installArtifact(lib);

    // Also create a shared library version (libochrance.so) — this is what the
    // Idris2 FFI loads at runtime. The name MUST be "ochrance" so the artifact is
    // libochrance.so: the Idris bindings declare `%foreign "C:blake3_hash,
    // libochrance"` (ochrance-core/Ochrance/FFI/Crypto.idr), so the runtime loader
    // resolves the soname libochrance.so. Any other shared-library name would never
    // be found. A static libochrance.a and a shared libochrance.so share the base
    // name without clashing (distinct extensions).
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
