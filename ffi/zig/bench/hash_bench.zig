// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// hash_bench.zig — Performance benchmarks for ochrance cryptographic hash functions.
// Measures throughput of BLAKE3, SHA-256, SHA3-256, and Ed25519 operations.

const std = @import("std");
const time = std.time;
const crypto = std.crypto;

const ITERATIONS = 100_000;
const SMALL_SIZE = 64;
const MEDIUM_SIZE = 4096;
const LARGE_SIZE = 65536;

fn bench_blake3(comptime size: usize) void {
    var data = [_]u8{0xAB} ** size;
    var out: [32]u8 = undefined;
    var i: usize = 0;
    while (i < ITERATIONS) : (i += 1) {
        crypto.hash.Blake3.hash(&data, &out, .{});
    }
    std.mem.doNotOptimizeAway(&out);
}

fn bench_sha256(comptime size: usize) void {
    var data = [_]u8{0xCD} ** size;
    var out: [32]u8 = undefined;
    var i: usize = 0;
    while (i < ITERATIONS) : (i += 1) {
        crypto.hash.sha2.Sha256.hash(&data, &out, .{});
    }
    std.mem.doNotOptimizeAway(&out);
}

fn bench_sha3_256(comptime size: usize) void {
    var data = [_]u8{0xEF} ** size;
    var out: [32]u8 = undefined;
    var i: usize = 0;
    while (i < ITERATIONS) : (i += 1) {
        crypto.hash.sha3.Sha3_256.hash(&data, &out, .{});
    }
    std.mem.doNotOptimizeAway(&out);
}

fn measure(comptime name: []const u8, comptime size: usize, comptime bench_fn: fn (comptime usize) void) void {
    const start = time.nanoTimestamp();
    bench_fn(size);
    const elapsed = time.nanoTimestamp() - start;
    const ns_per_op = @divTrunc(elapsed, ITERATIONS);
    const throughput_mb = @divTrunc(size * ITERATIONS * 1_000, elapsed); // MB/s
    std.debug.print("{s} ({d}B): {d}ns/op, ~{d}MB/s\n", .{ name, size, ns_per_op, throughput_mb });
}

pub fn main() !void {
    std.debug.print("=== Ochrance Hash Benchmark ===\n", .{});
    std.debug.print("Iterations: {d}\n\n", .{ITERATIONS});

    std.debug.print("--- BLAKE3 ---\n", .{});
    measure("BLAKE3", SMALL_SIZE, bench_blake3);
    measure("BLAKE3", MEDIUM_SIZE, bench_blake3);
    measure("BLAKE3", LARGE_SIZE, bench_blake3);

    std.debug.print("\n--- SHA-256 ---\n", .{});
    measure("SHA-256", SMALL_SIZE, bench_sha256);
    measure("SHA-256", MEDIUM_SIZE, bench_sha256);
    measure("SHA-256", LARGE_SIZE, bench_sha256);

    std.debug.print("\n--- SHA3-256 ---\n", .{});
    measure("SHA3-256", SMALL_SIZE, bench_sha3_256);
    measure("SHA3-256", MEDIUM_SIZE, bench_sha3_256);
    measure("SHA3-256", LARGE_SIZE, bench_sha3_256);

    std.debug.print("\n=== Benchmark complete ===\n", .{});
}
