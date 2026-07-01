// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// Ochránce Integration Tests
//
// These tests exercise the C ABI of libochrance exactly as an external
// consumer would: extern declarations resolved at link time against the
// built library (see build.zig `test` step), not Zig-internal calls. This
// verifies symbol names, calling convention, and behavioural contract —
// the same contract Idris2's %foreign bindings depend on at runtime.

const std = @import("std");
const testing = std.testing;

// C ABI imports — must match ffi/zig/src/main.zig exports and the
// Idris2 ABI declarations in src/abi/Ochrance/ABI/Foreign.idr.
extern fn ochrance_init() ?*anyopaque;
extern fn ochrance_free(handle: ?*anyopaque) void;
extern fn ochrance_process(handle: ?*anyopaque, input: u32) c_int;
extern fn ochrance_process_array(handle: ?*anyopaque, buffer: ?[*]const u8, len: u32) c_int;
extern fn ochrance_get_string(handle: ?*anyopaque) ?[*:0]const u8;
extern fn ochrance_free_string(str: ?[*:0]const u8) void;
extern fn ochrance_last_error() ?[*:0]const u8;
extern fn ochrance_version() [*:0]const u8;
extern fn ochrance_is_initialized(handle: ?*anyopaque) u32;
extern fn blake3_hash(data: [*]const u8, len: usize, out: [*]u8) void;

// Result codes (must match the Result enum in src/main.zig / ABI Types.idr)
const RESULT_OK: c_int = 0;
const RESULT_NULL_POINTER: c_int = 4;

//==============================================================================
// Lifecycle Tests
//==============================================================================

test "create and destroy handle" {
    const handle = ochrance_init() orelse return error.InitFailed;
    defer ochrance_free(handle);
}

test "handle is initialized" {
    const handle = ochrance_init() orelse return error.InitFailed;
    defer ochrance_free(handle);

    try testing.expectEqual(@as(u32, 1), ochrance_is_initialized(handle));
}

test "null handle is not initialized" {
    try testing.expectEqual(@as(u32, 0), ochrance_is_initialized(null));
}

test "free null is safe" {
    ochrance_free(null); // guarded by orelse in the implementation
}

//==============================================================================
// Operation Tests
//==============================================================================

test "process with valid handle" {
    const handle = ochrance_init() orelse return error.InitFailed;
    defer ochrance_free(handle);

    try testing.expectEqual(RESULT_OK, ochrance_process(handle, 42));
}

test "process with null handle returns null_pointer" {
    try testing.expectEqual(RESULT_NULL_POINTER, ochrance_process(null, 42));
}

test "process array with valid buffer" {
    const handle = ochrance_init() orelse return error.InitFailed;
    defer ochrance_free(handle);

    const data = [_]u8{ 1, 2, 3, 4 };
    try testing.expectEqual(RESULT_OK, ochrance_process_array(handle, &data, data.len));
}

test "process array with null buffer returns null_pointer" {
    const handle = ochrance_init() orelse return error.InitFailed;
    defer ochrance_free(handle);

    try testing.expectEqual(RESULT_NULL_POINTER, ochrance_process_array(handle, null, 0));
}

//==============================================================================
// String Tests
//==============================================================================

test "get string result" {
    const handle = ochrance_init() orelse return error.InitFailed;
    defer ochrance_free(handle);

    const str = ochrance_get_string(handle);
    defer ochrance_free_string(str);

    try testing.expect(str != null);
    try testing.expect(std.mem.span(str.?).len > 0);
}

test "get string with null handle" {
    try testing.expect(ochrance_get_string(null) == null);
}

test "free null string is safe" {
    ochrance_free_string(null); // guarded by orelse in the implementation
}

//==============================================================================
// Error Handling Tests
//==============================================================================

test "last error after null handle operation" {
    _ = ochrance_process(null, 0);

    const err = ochrance_last_error();
    try testing.expect(err != null);
    try testing.expect(std.mem.span(err.?).len > 0);
}

test "no error after successful operation" {
    const handle = ochrance_init() orelse return error.InitFailed;
    defer ochrance_free(handle);

    try testing.expectEqual(RESULT_OK, ochrance_process(handle, 0));
    try testing.expect(ochrance_last_error() == null);
}

//==============================================================================
// Version Tests
//==============================================================================

test "version string is semantic version format" {
    const ver_str = std.mem.span(ochrance_version());
    try testing.expect(ver_str.len > 0);
    try testing.expect(std.mem.count(u8, ver_str, ".") >= 1);
}

//==============================================================================
// Crypto ABI Test — the linked library computes real BLAKE3
//==============================================================================

test "blake3_hash known-answer vector across the C ABI" {
    var out: [32]u8 = undefined;
    const abc: []const u8 = "abc";
    blake3_hash(abc.ptr, abc.len, &out);

    var expected: [32]u8 = undefined;
    _ = try std.fmt.hexToBytes(
        &expected,
        "6437b3ac38465133ffb63b75273a8db548c558465d79db03fd359c6cd5bd9d85",
    );
    try testing.expectEqualSlices(u8, &expected, &out);
}

//==============================================================================
// Memory Safety Tests
//==============================================================================

test "multiple handles are independent" {
    const h1 = ochrance_init() orelse return error.InitFailed;
    defer ochrance_free(h1);

    const h2 = ochrance_init() orelse return error.InitFailed;
    defer ochrance_free(h2);

    try testing.expect(h1 != h2);

    try testing.expectEqual(RESULT_OK, ochrance_process(h1, 1));
    try testing.expectEqual(RESULT_OK, ochrance_process(h2, 2));
}

//==============================================================================
// Thread Safety Tests
//==============================================================================

test "concurrent operations" {
    const handle = ochrance_init() orelse return error.InitFailed;
    defer ochrance_free(handle);

    const thread_fn = struct {
        fn run(h: *anyopaque, id: u32) void {
            _ = ochrance_process(h, id);
        }
    }.run;

    var threads: [4]std.Thread = undefined;
    for (&threads, 0..) |*thread, i| {
        thread.* = try std.Thread.spawn(.{}, thread_fn, .{ handle, @as(u32, @intCast(i)) });
    }

    for (threads) |thread| {
        thread.join();
    }
}
