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
// Ed25519 Signature Verification
// ============================================================================

/// Verify an Ed25519 signature
/// ABI Contract:
///   signature: const uint8_t[64] - Ed25519 signature bytes
///   public_key: const uint8_t[32] - Ed25519 public key bytes
///   message: const uint8_t* - Message that was signed
///   msg_len: size_t - Length of message
/// Returns: 1 if valid, 0 if invalid
export fn ed25519_verify(
    signature: [*c]const u8,
    public_key: [*c]const u8,
    message: [*c]const u8,
    msg_len: usize
) c_int {
    // Convert C pointers to Zig arrays
    const sig_bytes: *const [64]u8 = @ptrCast(signature);
    const pubkey_bytes: *const [32]u8 = @ptrCast(public_key);
    const msg = message[0..msg_len];

    // Parse signature and public key
    const ed_sig = crypto.sign.Ed25519.Signature.fromBytes(sig_bytes.*);
    const ed_pubkey = crypto.sign.Ed25519.PublicKey.fromBytes(pubkey_bytes.*) catch {
        return 0; // Invalid public key format
    };

    // Verify signature
    ed_sig.verify(msg, ed_pubkey) catch {
        return 0; // Verification failed
    };

    return 1; // Verification succeeded
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
    const expected = "\x64\x37\xb3\xac\x38\x46\x51\x33\xff\xb6\x3b\x75\x27\x3a\x8d\xb5" ++
                     "\x48\xc5\x58\x46\x5d\x79\xdb\x03\xfd\x35\x9c\x6c\xd5\xbd\x9d\x85";

    var output: [32]u8 = undefined;
    blake3_hash(input, input.len, &output);

    try std.testing.expectEqualSlices(u8, expected, &output);
}

test "ed25519 valid signature" {
    // Generate a keypair for testing
    const seed: [32]u8 = [_]u8{1} ** 32;
    const keypair = try crypto.sign.Ed25519.KeyPair.generateDeterministic(seed);

    // Sign a message
    const message = "test message";
    const signature = try keypair.sign(message, null);

    // Convert signature to bytes
    const sig_bytes = signature.toBytes();
    const pubkey_bytes = keypair.public_key.toBytes();

    // Verify signature through FFI
    const result = ed25519_verify(
        &sig_bytes,
        &pubkey_bytes,
        message.ptr,
        message.len
    );

    try std.testing.expectEqual(@as(c_int, 1), result);
}

test "ed25519 invalid signature" {
    // Generate a keypair
    const seed: [32]u8 = [_]u8{1} ** 32;
    const keypair = try crypto.sign.Ed25519.KeyPair.generateDeterministic(seed);

    // Create an invalid signature (all zeros)
    const invalid_sig: [64]u8 = [_]u8{0} ** 64;
    const pubkey_bytes = keypair.public_key.toBytes();

    // Try to verify invalid signature
    const message = "test message";
    const result = ed25519_verify(
        &invalid_sig,
        &pubkey_bytes,
        message.ptr,
        message.len
    );

    try std.testing.expectEqual(@as(c_int, 0), result);
}
