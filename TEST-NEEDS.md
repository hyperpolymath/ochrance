# TEST-NEEDS.md — ochrance

> Generated 2026-03-29 by punishing audit.

## Current State

| Category     | Count | Notes |
|-------------|-------|-------|
| Unit tests   | 0     | None |
| Integration  | 1     | ffi/zig/test/integration_test.zig |
| E2E          | 0     | None |
| Benchmarks   | 0     | None |

**Source modules:** 32 Idris2 files total including ABI (Types, Layout, Foreign), 5 Zig files. Core logic appears to be in Idris2 formal specifications.

## What's Missing

### P2P (Property-Based) Tests
- [ ] ABI layout: property tests for struct alignment invariants
- [ ] Type encoding: property tests for Idris2->Zig type mapping

### E2E Tests
- [ ] Full ochrance workflow: define -> compile -> verify -> deploy
- [ ] ABI/FFI round-trip: Idris2 spec -> C header -> Zig impl -> verification

### Aspect Tests
- **Security:** No tests for formal specification bypass, FFI boundary safety
- **Performance:** No compilation or verification benchmarks
- **Concurrency:** N/A
- **Error handling:** No tests for invalid specifications, malformed ABI definitions

### Build & Execution
- [ ] Idris2 compilation of all 32 .idr files
- [ ] Zig build + test

### Benchmarks Needed
- [ ] Idris2 type checking time
- [ ] FFI call overhead

### Self-Tests
- [ ] All Idris2 proofs type-check successfully
- [ ] ABI version agreement

## Priority

**HIGH.** 32 Idris2 formal specification files with 0 unit tests and 1 FFI integration test. The formal proofs should be self-verifying (type-checking IS testing for Idris2), but there should be explicit test suites validating the properties claimed by the specifications.

## FAKE-FUZZ ALERT

- `tests/fuzz/placeholder.txt` is a scorecard placeholder inherited from rsr-template-repo — it does NOT provide real fuzz testing
- Replace with an actual fuzz harness (see rsr-template-repo/tests/fuzz/README.adoc) or remove the file
- Priority: P2 — creates false impression of fuzz coverage
