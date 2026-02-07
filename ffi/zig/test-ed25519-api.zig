const std = @import("std");
const Ed25519 = std.crypto.sign.Ed25519;

pub fn main() !void {
    // Check what fields Signature has
    const T = @typeInfo(Ed25519.Signature);
    std.debug.print("Signature type: {}\n", .{T});
}
