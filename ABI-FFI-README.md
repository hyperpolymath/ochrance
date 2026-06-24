<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
# Ochrance ABI/FFI Documentation

## Overview

**Ochrance** is a neurosymbolic filesystem verification framework. Its
cryptographic core follows the **Hyperpolymath RSR Standard** for ABI and FFI
design:

- **ABI (Application Binary Interface)** defined in **Idris2** with formal
  proofs — `src/abi/Ochrance/ABI/`
- **FFI (Foreign Function Interface)** implemented in **Zig** for C
  compatibility — `ffi/zig/src/main.zig`
- **Idris2 FFI declarations** in the core library bridge into the Zig
  shared object — `ochrance-core/Ochrance/FFI/Crypto.idr`

## Architecture

```
┌─────────────────────────────────────────────────┐
│  ABI Definitions (Idris2)                       │
│  src/abi/Ochrance/ABI/                          │
│  - Types.idr    PlatformContext, Result, Handle  │
│  - Layout.idr   Memory layout proofs, CCompat   │
│  - Foreign.idr  FFI declarations (blake3, sha)   │
└─────────────────┬───────────────────────────────┘
                  │
                  │ defines C-compatible signatures
                  ▼
┌─────────────────────────────────────────────────┐
│  Idris2 FFI Declarations                        │
│  ochrance-core/Ochrance/FFI/Crypto.idr          │
│  - %foreign "C:blake3_hash,libochrance"          │
│  - %foreign "C:sha256_hash,libochrance"          │
│  - %foreign "C:sha3_256_hash,libochrance"        │
│  - %foreign "C:ed25519_verify,libochrance"       │
│  - Buffer allocation, read, write helpers        │
└─────────────────┬───────────────────────────────┘
                  │
                  │ links at runtime to
                  ▼
┌─────────────────────────────────────────────────┐
│  FFI Implementation (Zig)                       │
│  ffi/zig/src/main.zig                           │
│  - blake3_hash    : BLAKE3 digest (32 bytes)    │
│  - sha256_hash    : SHA-256 digest (32 bytes)   │
│  - sha3_256_hash  : SHA3-256 digest (32 bytes)  │
│  - ed25519_verify : Ed25519 sig check (0 or 1)  │
│  Compiles to: libochrance.so / libochrance.a    │
└─────────────────┬───────────────────────────────┘
                  │
                  │ callable from any C-ABI language
                  ▼
┌─────────────────────────────────────────────────┐
│  Consumers                                      │
│  - Ochrance Merkle tree verification            │
│  - A2ML manifest attestation                    │
│  - ECHIDNA neural proof synthesis (future)      │
└─────────────────────────────────────────────────┘
```

## ABI Definitions (Idris2)

### Types.idr — Core ABI Types

| Type | Description |
|------|-------------|
| `Platform` | Compile-time target: Linux, Windows, MacOS, BSD, WASM |
| `Result` | Security check outcome: Ok, Error, InvalidParam, OutOfMemory, NullPointer |
| `Handle` | Opaque non-null pointer with type-level `So (ptr /= 0)` proof |

```idris
-- Non-null handle guaranteed at the type level
data Handle : Type where
  MkHandle : (ptr : Bits64) -> {auto 0 nonNull : So (ptr /= 0)} -> Handle
```

### Layout.idr — Memory Layout Proofs

Provides `Layout` records (size + alignment) and two key proof types:

- **`PlatformIndependent`** — witnesses that a layout is portable across
  architectures
- **`CCompatible`** — proves a type matches C struct packing rules, safe for
  FFI

Also includes `generateCDecl` for emitting C `typedef` declarations from
proven layouts.

### Foreign.idr — FFI Declarations

Declares `prim__blake3` and `prim__sha256` using `%foreign "C:...,libochrance"`
with `Buffer`-based signatures matching the Zig exports.

## Zig FFI Implementation

All functions in `ffi/zig/src/main.zig` use Zig's `std.crypto` library:

### Hash Functions

All three hash functions share the same ABI contract:

```c
void hash_fn(const uint8_t* data, size_t len, uint8_t out[32]);
```

| Function | Algorithm | Output |
|----------|-----------|--------|
| `blake3_hash` | BLAKE3 | 32 bytes |
| `sha256_hash` | SHA-256 | 32 bytes |
| `sha3_256_hash` | SHA3-256 | 32 bytes |

### Signature Verification

```c
int ed25519_verify(
    const uint8_t signature[64],
    const uint8_t public_key[32],
    const uint8_t* message,
    size_t msg_len
);
// Returns: 1 if valid, 0 if invalid
```

## How They Connect

1. **Zig compiles** `ffi/zig/src/main.zig` into `libochrance.so` (shared
   library with C-compatible exports)
2. **Idris2 FFI declarations** in `Ochrance.FFI.Crypto` use
   `%foreign "C:blake3_hash,libochrance"` to bind primitives
3. **Buffer management** in `Crypto.idr` allocates `Data.Buffer` objects,
   writes input bytes, calls the FFI primitive, reads output bytes
4. **Public API** (`blake3`, `sha256`, `sha3_256`, `ed25519Verify`) wraps
   everything in `HasIO io` for safe usage
5. **Merkle tree** (`Ochrance.Filesystem.Merkle`) calls `hashPairBlake3`
   which delegates to `blake3` for cryptographic tree hashing
6. **ABI modules** (`src/abi/`) provide the formal foundation: type
   definitions with dependent-type proofs, memory layout verification,
   and C-header generation

## Directory Structure

```
ochrance/
├── src/abi/Ochrance/ABI/         # Idris2 ABI definitions
│   ├── Types.idr                  #   Platform, Result, Handle
│   ├── Layout.idr                 #   Memory layout proofs, CCompatible
│   └── Foreign.idr                #   FFI primitive declarations
│
├── ochrance-core/Ochrance/FFI/   # Idris2 FFI wrappers
│   ├── Crypto.idr                 #   blake3, sha256, sha3_256, ed25519Verify
│   └── Echidna.idr                #   ECHIDNA neural prover (stub)
│
├── ffi/zig/                       # Zig FFI implementation
│   ├── build.zig                  #   Build configuration
│   ├── build.zig.zon              #   Dependencies
│   └── src/
│       └── main.zig               #   blake3_hash, sha256_hash, sha3_256_hash,
│                                  #   ed25519_verify (with tests)
│
├── generated/abi/                 # Auto-generated C headers (future)
│   └── ochrance.h
│
└── ochrance-core/Ochrance/       # Core library consuming the FFI
    ├── Filesystem/Merkle.idr      #   Uses hashPairBlake3 for tree hashing
    ├── Filesystem/Verify.idr      #   Uses blake3 for block verification
    └── A2ML/                      #   A2ML manifest parsing/validation
```

## Building

### Build the Zig FFI Library

```bash
cd ffi/zig
zig build                         # Build debug (libochrance.so)
zig build -Doptimize=ReleaseFast  # Build optimized
zig build test                    # Run Zig unit tests
```

### Build the Idris2 Core

```bash
# Ensure libochrance.so is on LD_LIBRARY_PATH
export LD_LIBRARY_PATH="$PWD/ffi/zig/zig-out/lib:$LD_LIBRARY_PATH"

# Type-check and build (the core package includes the filesystem subsystem)
idris2 --build ochrance.ipkg
```

### Cross-Compile

```bash
cd ffi/zig
zig build -Dtarget=x86_64-linux
zig build -Dtarget=aarch64-linux
zig build -Dtarget=aarch64-macos
```

## Testing

### Zig Unit Tests

The Zig source includes inline tests for all four FFI functions:

```bash
cd ffi/zig
zig build test
```

Tests cover: empty string hashing (BLAKE3, SHA-256, SHA3-256), known-answer
tests ("abc" for BLAKE3), valid Ed25519 signature verification, and invalid
signature rejection.

### Idris2 Integration Tests

```bash
idris2 --build tests/integration.ipkg
```

## Contributing

When modifying the ABI/FFI:

1. **Update ABI types** (`src/abi/Ochrance/ABI/Types.idr`) — add or modify
   type definitions with proofs
2. **Update FFI declarations** (`ochrance-core/Ochrance/FFI/Crypto.idr`) —
   add `%foreign` declarations and buffer management
3. **Update Zig implementation** (`ffi/zig/src/main.zig`) — implement the
   C-compatible function matching the declaration
4. **Add tests** — Zig inline tests for the new function, Idris2 integration
   tests for the wrapper
5. **Verify totality** — all Idris2 modules must pass `%default total`

## License

MPL-2.0

## See Also

- [Idris2 Documentation](https://idris2.readthedocs.io)
- [Zig Documentation](https://ziglang.org/documentation/master/)
- [Rhodium Standard Repositories](https://github.com/hyperpolymath/rhodium-standard-repositories)
