// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
const std = @import("std");
const Ed25519 = std.crypto.sign.Ed25519;

pub fn main() !void {
    // Check what fields Signature has
    const T = @typeInfo(Ed25519.Signature);
    std.debug.print("Signature type: {}\n", .{T});
}
