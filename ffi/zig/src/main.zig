// OCHRANCE FFI Implementation
//
// This module implements the C-compatible FFI declared in src/abi/Foreign.idr
// All types and layouts must match the Idris2 ABI definitions.
//
// SPDX-License-Identifier: MPL-2.0

const std = @import("std");

// Version information (keep in sync with project)
const VERSION = "0.1.0";
const BUILD_INFO = "OCHRANCE built with Zig " ++ @import("builtin").zig_version_string;

/// Thread-local error storage
threadlocal var last_error: ?[]const u8 = null;

/// Set the last error message
fn setError(msg: []const u8) void {
    last_error = msg;
}

/// Clear the last error
fn clearError() void {
    last_error = null;
}

//==============================================================================
// Core Types (must match src/abi/Types.idr)
//==============================================================================

/// Result codes (must match Idris2 Result type)
pub const Result = enum(c_int) {
    ok = 0,
    @"error" = 1,
    invalid_param = 2,
    out_of_memory = 3,
    null_pointer = 4,
};

/// Library handle (opaque to prevent direct access)
pub const Handle = opaque {
    // Internal state hidden from C
    allocator: std.mem.Allocator,
    initialized: bool,
    // Add your fields here
};

//==============================================================================
// Library Lifecycle
//==============================================================================

/// Initialize the library
/// Returns a handle, or null on failure
export fn ochrance_init() ?*Handle {
    const allocator = std.heap.c_allocator;

    const handle = allocator.create(Handle) catch {
        setError("Failed to allocate handle");
        return null;
    };

    // Initialize handle
    handle.* = .{
        .allocator = allocator,
        .initialized = true,
    };

    clearError();
    return handle;
}

/// Free the library handle
export fn ochrance_free(handle: ?*Handle) void {
    const h = handle orelse return;
    const allocator = h.allocator;

    // Clean up resources
    h.initialized = false;

    allocator.destroy(h);
    clearError();
}

//==============================================================================
// Core Operations
//==============================================================================

/// Process data (example operation)
export fn ochrance_process(handle: ?*Handle, input: u32) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return .@"error";
    }

    // Example processing logic
    _ = input;

    clearError();
    return .ok;
}

//==============================================================================
// String Operations
//==============================================================================

/// Get a string result (example)
/// Caller must free the returned string
export fn ochrance_get_string(handle: ?*Handle) ?[*:0]const u8 {
    const h = handle orelse {
        setError("Null handle");
        return null;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return null;
    }

    // Example: allocate and return a string
    const result = h.allocator.dupeZ(u8, "Example result") catch {
        setError("Failed to allocate string");
        return null;
    };

    clearError();
    return result.ptr;
}

/// Free a string allocated by the library
export fn ochrance_free_string(str: ?[*:0]const u8) void {
    const s = str orelse return;
    const allocator = std.heap.c_allocator;

    const slice = std.mem.span(s);
    allocator.free(slice);
}

//==============================================================================
// Array/Buffer Operations
//==============================================================================

/// Process an array of data
export fn ochrance_process_array(
    handle: ?*Handle,
    buffer: ?[*]const u8,
    len: u32,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    const buf = buffer orelse {
        setError("Null buffer");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return .@"error";
    }

    // Access the buffer
    const data = buf[0..len];
    _ = data;

    // Process data here

    clearError();
    return .ok;
}

//==============================================================================
// Error Handling
//==============================================================================

/// Get the last error message
/// Returns null if no error
export fn ochrance_last_error() ?[*:0]const u8 {
    const err = last_error orelse return null;

    // Return C string (static storage, no need to free)
    const allocator = std.heap.c_allocator;
    const c_str = allocator.dupeZ(u8, err) catch return null;
    return c_str.ptr;
}

//==============================================================================
// Version Information
//==============================================================================

/// Get the library version
export fn ochrance_version() [*:0]const u8 {
    return VERSION.ptr;
}

/// Get build information
export fn ochrance_build_info() [*:0]const u8 {
    return BUILD_INFO.ptr;
}

//==============================================================================
// Callback Support
//==============================================================================

/// Callback function type (C ABI)
pub const Callback = *const fn (u64, u32) callconv(.C) u32;

/// Register a callback
export fn ochrance_register_callback(
    handle: ?*Handle,
    callback: ?Callback,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    const cb = callback orelse {
        setError("Null callback");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return .@"error";
    }

    // Store callback for later use
    _ = cb;

    clearError();
    return .ok;
}

//==============================================================================
// Utility Functions
//==============================================================================

/// Check if handle is initialized
export fn ochrance_is_initialized(handle: ?*Handle) u32 {
    const h = handle orelse return 0;
    return if (h.initialized) 1 else 0;
}

//==============================================================================
// Cryptographic Hash + Signature FFI
//
// C ABI — must match the %foreign declarations in
// ochrance-core/Ochrance/FFI/Crypto.idr:
//   void blake3_hash   (const uint8_t* data, size_t len, uint8_t out[32]);
//   void sha256_hash   (const uint8_t* data, size_t len, uint8_t out[32]);
//   void sha3_256_hash (const uint8_t* data, size_t len, uint8_t out[32]);
//   int  ed25519_verify(const uint8_t sig[64], const uint8_t pk[32],
//                       const uint8_t* msg, size_t msg_len);  // 1=valid, 0=invalid
//
// Backed by Zig's audited std.crypto (Zig 0.11.0). These replace the XOR/zero
// placeholder hashes; see ROADMAP Phase 1 and docs/VALENCE_SHELL_BRIDGE.adoc,
// which gate cryptographic-integrity claims on closing exactly this gap.
//==============================================================================

const Ed25519 = std.crypto.sign.Ed25519;

/// BLAKE3 → 32-byte digest.
export fn blake3_hash(data: [*]const u8, len: usize, out: [*]u8) void {
    std.crypto.hash.Blake3.hash(data[0..len], out[0..32], .{});
}

/// SHA-256 → 32-byte digest.
export fn sha256_hash(data: [*]const u8, len: usize, out: [*]u8) void {
    std.crypto.hash.sha2.Sha256.hash(data[0..len], out[0..32], .{});
}

/// SHA3-256 → 32-byte digest.
export fn sha3_256_hash(data: [*]const u8, len: usize, out: [*]u8) void {
    std.crypto.hash.sha3.Sha3_256.hash(data[0..len], out[0..32], .{});
}

/// Verify an Ed25519 signature over `msg`.
/// Returns 1 if valid, 0 otherwise (including a malformed public key or
/// signature). Never traps on attacker-controlled input.
export fn ed25519_verify(
    sig: [*]const u8,
    pk: [*]const u8,
    msg: [*]const u8,
    msg_len: usize,
) c_int {
    const sig_bytes: [64]u8 = sig[0..64].*;
    const pk_bytes: [32]u8 = pk[0..32].*;
    const public_key = Ed25519.PublicKey.fromBytes(pk_bytes) catch return 0;
    const signature = Ed25519.Signature.fromBytes(sig_bytes);
    signature.verify(msg[0..msg_len], public_key) catch return 0;
    return 1;
}

//==============================================================================
// Tests
//==============================================================================

test "lifecycle" {
    const handle = ochrance_init() orelse return error.InitFailed;
    defer ochrance_free(handle);

    try std.testing.expect(ochrance_is_initialized(handle) == 1);
}

test "error handling" {
    const result = ochrance_process(null, 0);
    try std.testing.expectEqual(Result.null_pointer, result);

    const err = ochrance_last_error();
    try std.testing.expect(err != null);
}

test "version" {
    const ver = ochrance_version();
    const ver_str = std.mem.span(ver);
    try std.testing.expectEqualStrings(VERSION, ver_str);
}

//==============================================================================
// Cryptographic Tests — known-answer vectors + Ed25519 round-trip.
// These call the exported C-ABI functions directly, so they validate the FFI
// surface, not just the underlying std.crypto.
//==============================================================================

fn expectDigest(out: [32]u8, comptime hex: []const u8) !void {
    var expected: [32]u8 = undefined;
    _ = try std.fmt.hexToBytes(&expected, hex);
    try std.testing.expectEqualSlices(u8, &expected, &out);
}

test "blake3_hash known-answer vectors" {
    var out: [32]u8 = undefined;
    const empty: []const u8 = "";
    blake3_hash(empty.ptr, empty.len, &out);
    try expectDigest(out, "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262");
    const abc: []const u8 = "abc";
    blake3_hash(abc.ptr, abc.len, &out);
    try expectDigest(out, "6437b3ac38465133ffb63b75273a8db548c558465d79db03fd359c6cd5bd9d85");
}

test "sha256_hash known-answer vectors" {
    var out: [32]u8 = undefined;
    const empty: []const u8 = "";
    sha256_hash(empty.ptr, empty.len, &out);
    try expectDigest(out, "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
    const abc: []const u8 = "abc";
    sha256_hash(abc.ptr, abc.len, &out);
    try expectDigest(out, "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
}

test "sha3_256_hash known-answer vectors" {
    var out: [32]u8 = undefined;
    const empty: []const u8 = "";
    sha3_256_hash(empty.ptr, empty.len, &out);
    try expectDigest(out, "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a");
    const abc: []const u8 = "abc";
    sha3_256_hash(abc.ptr, abc.len, &out);
    try expectDigest(out, "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532");
}

test "ed25519_verify accepts valid and rejects tampered" {
    const seed = [_]u8{42} ** 32;
    const kp = try Ed25519.KeyPair.create(seed);
    const message: []const u8 = "ochrance attestation";
    const signature = try kp.sign(message, null);
    const sig_bytes = signature.toBytes();
    const pk_bytes = kp.public_key.bytes;

    // Valid signature verifies.
    try std.testing.expectEqual(
        @as(c_int, 1),
        ed25519_verify(&sig_bytes, &pk_bytes, message.ptr, message.len),
    );

    // A tampered signature is rejected, not trapped on.
    var tampered = sig_bytes;
    tampered[0] ^= 0xFF;
    try std.testing.expectEqual(
        @as(c_int, 0),
        ed25519_verify(&tampered, &pk_bytes, message.ptr, message.len),
    );

    // The wrong public key is rejected.
    var wrong_pk = pk_bytes;
    wrong_pk[0] ^= 0xFF;
    try std.testing.expectEqual(
        @as(c_int, 0),
        ed25519_verify(&sig_bytes, &wrong_pk, message.ptr, message.len),
    );
}
