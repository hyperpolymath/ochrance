// SPDX-License-Identifier: PMPL-1.0-or-later
//
// libochrance - C-compatible FFI implementation for cryptographic hashing
//
// This library provides memory-safe, formally-verified-ABI hash functions
// callable from Idris2 via C FFI. All functions use platform-independent
// byte representations.

const std = @import("std");
const crypto = std.crypto;

// ============================================================================
// BLAKE3 Implementation
// ============================================================================

/// Hash arbitrary data with BLAKE3
/// ABI Contract: data (const uint8_t*), len (size_t), out (uint8_t[32])
export fn blake3_hash(data: [*c]const u8, len: usize, out: [*c]u8) void {
    const input = data[0..len];
    var hasher = crypto.hash.Blake3.init(.{});
    hasher.update(input);
    
    var output: [32]u8 = undefined;
    hasher.final(&output);
    
    // Copy to output buffer (ABI: contiguous 32 bytes)
    @memcpy(out[0..32], &output);
}

// ============================================================================
// SHA-256 Implementation
// ============================================================================

/// Hash arbitrary data with SHA-256
/// ABI Contract: data (const uint8_t*), len (size_t), out (uint8_t[32])
export fn sha256_hash(data: [*c]const u8, len: usize, out: [*c]u8) void {
    const input = data[0..len];
    var hasher = crypto.hash.sha2.Sha256.init(.{});
    hasher.update(input);
    
    var output: [32]u8 = undefined;
    hasher.final(&output);
    
    @memcpy(out[0..32], &output);
}

// ============================================================================
// SHA3-256 Implementation
// ============================================================================

/// Hash arbitrary data with SHA3-256
/// ABI Contract: data (const uint8_t*), len (size_t), out (uint8_t[32])
export fn sha3_256_hash(data: [*c]const u8, len: usize, out: [*c]u8) void {
    const input = data[0..len];
    var hasher = crypto.hash.sha3.Sha3_256.init(.{});
    hasher.update(input);
    
    var output: [32]u8 = undefined;
    hasher.final(&output);
    
    @memcpy(out[0..32], &output);
}

// ============================================================================
// Tests
// ============================================================================

test "blake3 empty string" {
    const expected = "\xaf\x13\x49\xb9\xf5\xf9\xa1\xa6\xa0\x40\x4d\xea\x36\xdc\xc9\x49" ++
                     "\x9b\xcb\x25\xc9\xad\xc1\x12\xb7\xcc\x9a\x93\xca\xe4\x1f\x32\x62";
    
    var output: [32]u8 = undefined;
    blake3_hash("", 0, &output);
    
    try std.testing.expectEqualSlices(u8, expected, &output);
}

test "sha256 empty string" {
    const expected = "\xe3\xb0\xc4\x42\x98\xfc\x1c\x14\x9a\xfb\xf4\xc8\x99\x6f\xb9\x24" ++
                     "\x27\xae\x41\xe4\x64\x9b\x93\x4c\xa4\x95\x99\x1b\x78\x52\xb8\x55";
    
    var output: [32]u8 = undefined;
    sha256_hash("", 0, &output);
    
    try std.testing.expectEqualSlices(u8, expected, &output);
}

test "sha3_256 empty string" {
    const expected = "\xa7\xff\xc6\xf8\xbf\x1e\xd7\x66\x51\xc1\x47\x56\xa0\x61\xd6\x62" ++
                     "\xf5\x80\xff\x4d\xe4\x3b\x49\xfa\x82\xd8\x0a\x4b\x80\xf8\x43\x4a";
    
    var output: [32]u8 = undefined;
    sha3_256_hash("", 0, &output);
    
    try std.testing.expectEqualSlices(u8, expected, &output);
}

test "blake3 abc" {
    const input = "abc";
    const expected = "\x6a\x99\xd6\x59\xa8\x17\xfe\xea\x61\x63\x9d\x5d\xd3\x6f\x57\x66" ++
                     "\x0c\x87\xfe\xeb\xf7\xf3\xd3\x06\xe4\xd5\xe6\xa6\x8e\x4e\xd7\x6f";
    
    var output: [32]u8 = undefined;
    blake3_hash(input, input.len, &output);
    
    try std.testing.expectEqualSlices(u8, expected, &output);
}
