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
│   └── FFI/
│       └── Echidna.idr      # FFI to libechidna.so
├── modules/
│   └── filesystem/          # Reference VerifiedSubsystem
│       ├── Types.idr        # FSState, Block, FSSnapshot
│       ├── Merkle.idr       # Verified Merkle tree
│       ├── Verify.idr       # Verification logic
│       └── Repair.idr       # Linear type repair
├── tests/                   # Test suite
├── ochrance.ipkg            # Core package
└── ochrance-fs.ipkg         # Filesystem module package
```

## Build Commands

```bash
# Type-check core
idris2 --build ochrance.ipkg

# Type-check filesystem module
idris2 --build ochrance-fs.ipkg

# Check single file
idris2 --check ochrance-core/A2ML/Lexer.idr

# REPL
idris2 --repl ochrance.ipkg
```

## Critical Rules

1. **All functions must be total** - use `%default total` in every module
2. **Structural recursion only** - no partial or assert_total
3. **Idris2 0.8.0+** required
4. **BLAKE3/SHA-256 via FFI** - placeholder XOR hashes in Merkle.idr must be replaced
5. **Linear types for repair** - repair operations consume old state (Quantity 1)

## Error Taxonomy

- **q/** - Query errors (user input validation)
- **p/** - Proof errors (verification/hash failures)
- **z/** - Zone errors (system/IO/FFI)

## Related Projects

- **echidna** - Rust/Julia neurosymbolic prover (provides libechidna.so)
- **idris2-echidna** - Idris2 prover abstraction layer
- **proven** - Idris2 formally verified library
