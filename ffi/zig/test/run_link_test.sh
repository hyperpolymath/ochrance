#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# run_link_test.sh — build libochrance.so and run the C-ABI runtime link test.
#
# This proves the *runtime* contract the Idris verification flow depends on:
# `libochrance.so` is produced with the right soname and its exported C symbols
# (blake3_hash, sha256_hash, sha3_256_hash, ed25519_verify, ochrance_version)
# are resolvable via dlopen/dlsym and compute the real primitives. Idris2's FFI
# loads the same .so by the same mechanism.
#
# Overridable: ZIG (default "zig"), CC (default "cc").
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"   # ffi/zig
cd "$HERE"

ZIG="${ZIG:-zig}"
CC="${CC:-cc}"

echo "[link-test] $ZIG build"
"$ZIG" build

SO="$HERE/zig-out/lib/libochrance.so"
if [ ! -f "$SO" ]; then
    echo "ERROR: $SO was not produced by '$ZIG build'." >&2
    echo "       The shared library must be named 'ochrance' in build.zig." >&2
    exit 1
fi

echo "[link-test] $CC test/link_test.c -ldl"
"$CC" -Wall -Wextra -O2 -o "$HERE/zig-out/link_test" test/link_test.c -ldl

echo "[link-test] running against $SO"
"$HERE/zig-out/link_test" "$SO"
