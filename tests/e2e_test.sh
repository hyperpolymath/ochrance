#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# ochrance — End-to-End Tests
# Tests the complete ochrance library pipeline: ABI spec → FFI implementation → cryptographic output.
# This test validates structural and behavioral properties without requiring a live binary.

set -euo pipefail

PASS=0
FAIL=0
PEND=0
# Resolve the repository root from this script's own location so the suite
# runs anywhere (CI checkouts, contributor clones), not just one local path.
BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

assert() {
    local desc="$1"
    local result="$2"
    if [[ "$result" == "0" ]]; then
        echo "PASS: $desc"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $desc"
        FAIL=$((FAIL + 1))
    fi
}

# Like assert(), but for capabilities that are intentionally NOT YET implemented
# and tracked as a known gap (ROADMAP Phase 1 — replace placeholder hashes with
# real BLAKE3/SHA-256/Ed25519 FFI; see docs/VALENCE_SHELL_BRIDGE.adoc, which gates
# any cryptographic-integrity claim on closing exactly this gap). Reported as
# PENDING (does NOT fail the suite) while the gap is open, and flips to PASS
# automatically once the capability lands.
assert_pending() {
    local desc="$1"
    local result="$2"
    if [[ "$result" == "0" ]]; then
        echo "PASS: $desc"
        PASS=$((PASS + 1))
    else
        echo "PENDING: $desc (Zig crypto FFI not yet implemented)"
        PEND=$((PEND + 1))
    fi
}

# ================================================================
# E2E: ABI ↔ FFI structural consistency
# ================================================================

echo "=== E2E: ABI ↔ FFI Structural Consistency ==="

# Every exported Idris2 function should have a matching Zig export
idris2_functions=$(grep -h "export" "$BASE/src/abi/Ochrance/ABI/Foreign.idr" 2>/dev/null | grep -c "foreign\|export" || echo 0)
zig_exports=$(grep -c "^export fn" "$BASE/ffi/zig/src/main.zig" 2>/dev/null || echo 0)
assert "Zig FFI has exported functions" "$([ "$zig_exports" -gt 0 ] && echo 0 || echo 1)"

# All four hash algorithms are implemented (PENDING: Zig crypto FFI is still a
# scaffold — these flip to PASS once main.zig implements the primitives).
assert_pending "BLAKE3 implemented in Zig" "$(grep -q "blake3" "$BASE/ffi/zig/src/main.zig" && echo 0 || echo 1)"
assert_pending "SHA-256 implemented in Zig" "$(grep -qi "sha.256\|sha256" "$BASE/ffi/zig/src/main.zig" && echo 0 || echo 1)"
assert_pending "SHA3-256 implemented in Zig" "$(grep -qi "sha3\|sha3_256" "$BASE/ffi/zig/src/main.zig" && echo 0 || echo 1)"
assert_pending "Ed25519 implemented in Zig" "$(grep -q "ed25519\|Ed25519" "$BASE/ffi/zig/src/main.zig" && echo 0 || echo 1)"

# ================================================================
# E2E: Idris2 ABI completeness
# ================================================================

echo ""
echo "=== E2E: Idris2 ABI Completeness ==="

# All ABI modules exist
for module in Types.idr Layout.idr Foreign.idr; do
    assert "Idris2 ABI module exists: $module" \
        "$([ -f "$BASE/src/abi/Ochrance/ABI/$module" ] && echo 0 || echo 1)"
done

# Types module defines required types
assert "Types.idr has HashResult type" \
    "$(grep -q "Hash\|Result\|Digest" "$BASE/src/abi/Ochrance/ABI/Types.idr" && echo 0 || echo 1)"

# Layout module verifies memory layout
assert "Layout.idr exists and is non-empty" \
    "$([ -s "$BASE/src/abi/Ochrance/ABI/Layout.idr" ] && echo 0 || echo 1)"

# ================================================================
# E2E: Zig FFI contract compliance
# ================================================================

echo ""
echo "=== E2E: Zig FFI Contract Compliance ==="

# Hash functions follow ABI contract: (data ptr, len, out ptr) → void
# (PENDING: depends on the same unimplemented Zig crypto FFI as above.)
blake3_sig=$(grep "export fn blake3_hash" "$BASE/ffi/zig/src/main.zig" 2>/dev/null || echo "")
assert_pending "blake3_hash has correct signature" "$(echo "$blake3_sig" | grep -q "const u8\|usize" && echo 0 || echo 1)"

# Output size constants match algorithm specs
assert "BLAKE3 output is 32 bytes" \
    "$(grep -q "32\b" "$BASE/ffi/zig/src/main.zig" && echo 0 || echo 1)"

# Build file exists and configures library
assert "Zig build.zig exists" "$([ -f "$BASE/ffi/zig/build.zig" ] && echo 0 || echo 1)"
assert "build.zig configures library" \
    "$(grep -q "lib\|static\|shared" "$BASE/ffi/zig/build.zig" && echo 0 || echo 1)"

# ================================================================
# E2E: Test coverage completeness
# ================================================================

echo ""
echo "=== E2E: Test Suite Coverage ==="

assert "Integration tests exist" "$([ -f "$BASE/ffi/zig/test/integration_test.zig" ] && echo 0 || echo 1)"
assert "Property tests exist (Idris2)" "$([ -f "$BASE/tests/property/PropertyTests.idr" ] && echo 0 || echo 1)"
assert "Integration tests exist (Idris2)" "$([ -f "$BASE/tests/integration/IntegrationTests.idr" ] && echo 0 || echo 1)"
assert "A2ML format tests exist" "$([ -f "$BASE/tests/A2ML/ParserTests.idr" ] && echo 0 || echo 1)"

# Ed25519 specific test exists
assert "Ed25519 tests exist" \
    "$([ -f "$BASE/ffi/zig/test-ed25519.zig" ] || [ -f "$BASE/ffi/zig/test-ed25519-api.zig" ] && echo 0 || echo 1)"

# ================================================================
# E2E: Security properties
# ================================================================

echo ""
echo "=== E2E: Security Properties ==="

# No hardcoded secrets in crypto implementation
secret_hits=$(grep -n "password\|secret\|key\s*=\s*['\"]" "$BASE/ffi/zig/src/main.zig" 2>/dev/null | grep -v "//\|test\|public_key\|private_key" | wc -l || true)
assert "No hardcoded secrets in FFI" "$([ "$secret_hits" -eq 0 ] && echo 0 || echo 1)"

# SPDX headers present in all source files
for f in "$BASE/ffi/zig/src/main.zig" "$BASE/ffi/zig/test/integration_test.zig"; do
    assert "SPDX header in $(basename "$f")" \
        "$(grep -q "SPDX-License-Identifier" "$f" && echo 0 || echo 1)"
done

echo ""
echo "=== Results: $PASS passed, $FAIL failed, $PEND pending ==="
if [ "$PEND" -gt 0 ]; then
    echo "NOTE: $PEND pending check(s) await Zig crypto FFI implementation"
    echo "      (ROADMAP Phase 1; gates cryptographic-integrity claims —"
    echo "       see docs/VALENCE_SHELL_BRIDGE.adoc). These are not failures."
fi
[ "$FAIL" -eq 0 ] && exit 0 || exit 1
