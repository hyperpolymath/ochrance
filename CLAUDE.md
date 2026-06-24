<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
# CLAUDE.md - Ochránce

## Project Overview

**Ochránce** is a neurosymbolic filesystem verification framework using Idris2 dependent types. It integrates with ECHIDNA for neural proof synthesis.

**Repository**: https://github.com/hyperpolymath/ochrance

## Architecture

```
ochrance/
├── ochrance-core/           # Idris2 core library
│   ├── A2ML/                # Attestation & Audit Markup Language
│   │   ├── Types.idr        # Core types (Manifest, Hash, Ref)
│   │   ├── Lexer.idr        # Total lexer (structural recursion)
│   │   ├── Parser.idr       # Total parser (sized types)
│   │   ├── Validator.idr    # Semantic validation
│   │   └── Serializer.idr   # Roundtrip serialization
│   ├── Framework/           # Verification framework
│   │   ├── Interface.idr    # VerifiedSubsystem interface
│   │   ├── Proof.idr        # Proof witnesses
│   │   └── Error.idr        # q/p/z error taxonomy
│   ├── Filesystem/          # Reference VerifiedSubsystem
│   │   ├── Types.idr        # FSState, Block, FSSnapshot
│   │   ├── Merkle.idr       # Verified Merkle tree + merkleCorrect theorem
│   │   ├── Verify.idr       # Verification logic
│   │   └── Repair.idr       # Linear type repair
│   └── FFI/
│       ├── Crypto.idr       # FFI to libochrance.so (BLAKE3/SHA-256/Ed25519)
│       └── Echidna.idr      # FFI to libechidna.so
├── tests/                   # Test suite
└── ochrance.ipkg            # Core package (includes the filesystem subsystem)
```

## Build Commands

```bash
# Type-check core (includes the filesystem subsystem)
idris2 --build ochrance.ipkg

# Check single file
idris2 --check ochrance-core/Ochrance/A2ML/Lexer.idr

# REPL
idris2 --repl ochrance.ipkg
```

## Critical Rules

1. **All functions must be total** - use `%default total` in every module
2. **Structural recursion only** - no partial or assert_total
3. **Idris2 0.8.0+** required
4. **BLAKE3/SHA-256 via FFI** - real crypto is implemented in the Zig FFI (`ffi/zig/src/main.zig`: BLAKE3/SHA-256/SHA3-256/Ed25519 via `std.crypto`, with known-answer-vector tests) and wired into the Idris production path (`blake3`/`sha256`/`sha3_256`/`hashPairBlake3`/`rootHashBytesIO`/`ed25519Verify`) via `%foreign "C:...,libochrance"`. `build.zig` emits `libochrance.so` with the correct soname; the runtime C-ABI contract is CI-gated by a dlopen link test (`ffi/zig/test/link_test.c`, KAT vectors), and `tests/ffi/CryptoFFITest.idr` confirms the production Merkle root is the real BLAKE3 fold (≠ the XOR root). The dead stub fallbacks (`blake3Stub`/`sha256Stub`/`sha3_256Stub`/`ed25519VerifyStub`) are removed; the pure XOR combiner is renamed `xorCombiner` — the totality-friendly *spec instance* of the combiner-generic theorems, not a fallback. The one irreducible crypto assumption is `CollisionResistant` (pigeonhole-false, isolated in `Filesystem.MerkleAssumption`).
5. **Linear types for repair** - repair operations consume old state (Quantity 1)

## Error Taxonomy

- **q/** - Query errors (user input validation)
- **p/** - Proof errors (verification/hash failures)
- **z/** - Zone errors (system/IO/FFI)

## Related Projects

- **echidna** - Rust/Julia neurosymbolic prover (provides libechidna.so)
- **idris2-echidna** - Idris2 prover abstraction layer
- **proven** - Idris2 formally verified library
