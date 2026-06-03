// OCHRANCE FFI Implementation
//
// This module implements the C-compatible FFI declared in src/abi/Foreign.idr
// All types and layouts must match the Idris2 ABI definitions.
//
// SPDX-License-Identifier: MPL-2.0

const std = @import("std");

// Version information (keep in sync with project)
const VERSION = "0.1.0";
const BUILD_INFO = std.fmt.comptimePrint("OCHRANCE built with Zig {s}", .{@import("builtin").zig_version_string});

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

/// Library handle. Declared as a regular struct so the Zig side can carry
/// internal state; C only ever receives an opaque `*Handle` pointer and never
/// inspects the layout, so this remains ABI-safe across the FFI boundary.
pub const Handle = struct {
    allocator: std.mem.Allocator,
    initialized: bool,
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
// Cryptographic Hash & Signature FFI
//
// These functions back the Idris2 %foreign declarations in
// ochrance-core/Ochrance/FFI/Crypto.idr and src/abi/Ochrance/ABI/Foreign.idr.
// ABI contracts:
//   void  blake3_hash   (const u8* data, usize len, u8 out[32])
//   void  sha256_hash   (const u8* data, usize len, u8 out[32])
//   void  sha3_256_hash (const u8* data, usize len, u8 out[32])
//   c_int ed25519_verify(const u8 sig[64], const u8 pk[32],
//                        const u8* msg, usize msg_len)  -> 1 valid, 0 invalid
//==============================================================================

/// BLAKE3 digest (32 bytes) of data[0..len], written to out[0..32].
export fn blake3_hash(data: [*]const u8, len: usize, out: [*]u8) void {
    std.crypto.hash.Blake3.hash(data[0..len], out[0..32], .{});
}

/// SHA-256 digest (32 bytes) of data[0..len], written to out[0..32].
export fn sha256_hash(data: [*]const u8, len: usize, out: [*]u8) void {
    std.crypto.hash.sha2.Sha256.hash(data[0..len], out[0..32], .{});
}

/// SHA3-256 digest (32 bytes) of data[0..len], written to out[0..32].
export fn sha3_256_hash(data: [*]const u8, len: usize, out: [*]u8) void {
    std.crypto.hash.sha3.Sha3_256.hash(data[0..len], out[0..32], .{});
}

/// Verify an Ed25519 signature. Returns 1 if valid, 0 otherwise.
export fn ed25519_verify(
    sig: [*]const u8,
    pk: [*]const u8,
    msg: [*]const u8,
    msg_len: usize,
) c_int {
    const Ed25519 = std.crypto.sign.Ed25519;
    const signature = Ed25519.Signature.fromBytes(sig[0..64].*);
    const public_key = Ed25519.PublicKey.fromBytes(pk[0..32].*) catch return 0;
    signature.verify(msg[0..msg_len], public_key) catch return 0;
    return 1;
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
    return VERSION;
}

/// Get build information
export fn ochrance_build_info() [*:0]const u8 {
    return BUILD_INFO;
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

test "blake3 known-answer (abc)" {
    var out: [32]u8 = undefined;
    const msg = "abc";
    blake3_hash(msg.ptr, msg.len, &out);
    const expected = [_]u8{
        0x64, 0x37, 0xb3, 0xac, 0x38, 0x46, 0x51, 0x33,
        0xff, 0xb6, 0x3b, 0x75, 0x27, 0x3a, 0x8d, 0xb5,
        0x48, 0xc5, 0x58, 0x46, 0x5d, 0x79, 0xdb, 0x03,
        0xfd, 0x35, 0x9c, 0x6c, 0xd5, 0xbd, 0x9d, 0x85,
    };
    try std.testing.expectEqualSlices(u8, &expected, &out);
}

test "sha256 known-answer (abc)" {
    var out: [32]u8 = undefined;
    const msg = "abc";
    sha256_hash(msg.ptr, msg.len, &out);
    const expected = [_]u8{
        0xba, 0x78, 0x16, 0xbf, 0x8f, 0x01, 0xcf, 0xea,
        0x41, 0x41, 0x40, 0xde, 0x5d, 0xae, 0x22, 0x23,
        0xb0, 0x03, 0x61, 0xa3, 0x96, 0x17, 0x7a, 0x9c,
        0xb4, 0x10, 0xff, 0x61, 0xf2, 0x00, 0x15, 0xad,
    };
    try std.testing.expectEqualSlices(u8, &expected, &out);
}

test "sha3-256 known-answer (abc)" {
    var out: [32]u8 = undefined;
    const msg = "abc";
    sha3_256_hash(msg.ptr, msg.len, &out);
    const expected = [_]u8{
        0x3a, 0x98, 0x5d, 0xa7, 0x4f, 0xe2, 0x25, 0xb2,
        0x04, 0x5c, 0x17, 0x2d, 0x6b, 0xd3, 0x90, 0xbd,
        0x85, 0x5f, 0x08, 0x6e, 0x3e, 0x9d, 0x52, 0x5b,
        0x46, 0xbf, 0xe2, 0x45, 0x11, 0x43, 0x15, 0x32,
    };
    try std.testing.expectEqualSlices(u8, &expected, &out);
}

test "ed25519 verify (RFC 8032 test 1)" {
    const pk = [_]u8{
        0xd7, 0x5a, 0x98, 0x01, 0x82, 0xb1, 0x0a, 0xb7,
        0xd5, 0x4b, 0xfe, 0xd3, 0xc9, 0x64, 0x07, 0x3a,
        0x0e, 0xe1, 0x72, 0xf3, 0xda, 0xa6, 0x23, 0x25,
        0xaf, 0x02, 0x1a, 0x68, 0xf7, 0x07, 0x51, 0x1a,
    };
    const sig = [_]u8{
        0xe5, 0x56, 0x43, 0x00, 0xc3, 0x60, 0xac, 0x72,
        0x90, 0x86, 0xe2, 0xcc, 0x80, 0x6e, 0x82, 0x8a,
        0x84, 0x87, 0x7f, 0x1e, 0xb8, 0xe5, 0xd9, 0x74,
        0xd8, 0x73, 0xe0, 0x65, 0x22, 0x49, 0x01, 0x55,
        0x5f, 0xb8, 0x82, 0x15, 0x90, 0xa3, 0x3b, 0xac,
        0xc6, 0x1e, 0x39, 0x70, 0x1c, 0xf9, 0xb4, 0x6b,
        0xd2, 0x5b, 0xf5, 0xf0, 0x59, 0x5b, 0xbe, 0x24,
        0x65, 0x51, 0x41, 0x43, 0x8e, 0x7a, 0x10, 0x0b,
    };
    const msg = [_]u8{};
    try std.testing.expectEqual(@as(c_int, 1), ed25519_verify(&sig, &pk, &msg, msg.len));
    // A corrupted signature must be rejected.
    var bad = sig;
    bad[0] ^= 0xff;
    try std.testing.expectEqual(@as(c_int, 0), ed25519_verify(&bad, &pk, &msg, msg.len));
}
