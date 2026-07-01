// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
const std = @import("std");
const Ed25519 = std.crypto.sign.Ed25519;

pub fn main() !void {
    // Check what fields Signature has (std.builtin.Type is comptime-only, so
    // enumerate field names rather than printing the Type value itself)
    inline for (@typeInfo(Ed25519.Signature).@"struct".fields) |field| {
        std.debug.print("Signature field: {s}: {s}\n", .{ field.name, @typeName(field.type) });
    }
}
