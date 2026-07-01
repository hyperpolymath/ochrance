// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
const std = @import("std");
const Ed25519 = std.crypto.sign.Ed25519;

pub fn main() !void {
    // Create a keypair
    const seed: [32]u8 = [_]u8{1} ** 32;
    const keypair = try Ed25519.KeyPair.generateDeterministic(seed);
    
    // Sign something
    const message = "test";
    const sig = try keypair.sign(message, null);
    
    // See what type sig is
    std.debug.print("Signature type: {s}\n", .{@typeName(@TypeOf(sig))});
    
    // Try to construct from bytes
    const sig_bytes = sig.toBytes();
    std.debug.print("Signature bytes length: {}\n", .{sig_bytes.len});
    
    // Try fromBytes (infallible since Zig 0.12: returns Signature directly)
    const sig2 = Ed25519.Signature.fromBytes(sig_bytes);
    _ = sig2;
    
    std.debug.print("Success!\n", .{});
}
