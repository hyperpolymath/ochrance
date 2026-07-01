// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Minimum Zig version: 0.15.0 (Build.addLibrary / root_module API; for the
// old 0.11 addStaticLibrary/addSharedLibrary form see git history)
// Required for: std.crypto.hash.Blake3, C ABI export, @memcpy builtin
//
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // One module definition shared by every artifact. libc is required by
    // std.heap.c_allocator (used by the handle lifecycle) and is the natural
    // choice for a C-ABI FFI library.
    const mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    // Static libochrance.a
    const static_lib = b.addLibrary(.{
        .name = "ochrance",
        .linkage = .static,
        .root_module = mod,
    });
    b.installArtifact(static_lib);

    // Shared libochrance.so — this is what the Idris2 FFI loads at runtime.
    // The name MUST be "ochrance" so the artifact is libochrance.so: the Idris
    // bindings declare `%foreign "C:blake3_hash, libochrance"`
    // (ochrance-core/Ochrance/FFI/Crypto.idr), so the runtime loader resolves
    // the soname libochrance.so. Any other shared-library name would never be
    // found. A static libochrance.a and a shared libochrance.so share the base
    // name without clashing (distinct extensions).
    const shared_lib = b.addLibrary(.{
        .name = "ochrance",
        .linkage = .dynamic,
        .root_module = mod,
    });
    b.installArtifact(shared_lib);

    // Unit tests (in-module test blocks: KAT vectors, Ed25519 round-trip)
    const tests = b.addTest(.{
        .root_module = mod,
    });

    // Integration tests: extern declarations linked against libochrance.a,
    // exercising the C ABI exactly as an external consumer (Idris2) does.
    const integration_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/integration_test.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    integration_tests.linkLibrary(static_lib);

    const run_tests = b.addRunArtifact(tests);
    const run_integration_tests = b.addRunArtifact(integration_tests);
    const test_step = b.step("test", "Run unit + integration tests");
    test_step.dependOn(&run_tests.step);
    test_step.dependOn(&run_integration_tests.step);
}
