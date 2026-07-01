#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# run_ffi_test.sh — end-to-end Idris -> FFI -> libochrance.so runtime test.
#
# Builds the Zig shared library, installs the ochrance package, builds the Idris
# FFI test executable, and runs it with libochrance.so on the loader path. A pass
# means the production verification path (blake3 / rootHashBytesIO / ...) computes
# real BLAKE3 across the %foreign boundary — not the XOR spec combiner.
#
# Requires: idris2 (>= 0.8.0) and zig (>= 0.15.0). Overridable: ZIG, IDRIS2.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

ZIG="${ZIG:-zig}"
IDRIS2="${IDRIS2:-idris2}"

echo "[ffi-test] building libochrance.so"
( cd ffi/zig && "$ZIG" build )

SO_DIR="$ROOT/ffi/zig/zig-out/lib"
if [ ! -f "$SO_DIR/libochrance.so" ]; then
    echo "ERROR: $SO_DIR/libochrance.so not built (check ffi/zig/build.zig name)." >&2
    exit 1
fi

echo "[ffi-test] installing ochrance package"
"$IDRIS2" --install ochrance.ipkg

echo "[ffi-test] building Idris FFI test executable"
"$IDRIS2" --build tests/ffi/ffi-test.ipkg

echo "[ffi-test] running with LD_LIBRARY_PATH=$SO_DIR"
LD_LIBRARY_PATH="$SO_DIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
    tests/ffi/build/exec/crypto-ffi-test
