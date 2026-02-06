# Ochránce ABI Layer

Formally verified Application Binary Interface (ABI) for cryptographic hashing operations.

## Architecture

```
Idris2 (ABI)  →  C Headers  →  Zig (FFI)
   ↓                             ↓
Type Proofs              Memory-Safe Impl
```

## Modules

### `Ochrance.ABI.Types`
- **HashAlg n** - Hash algorithms indexed by output size
- **HashValue n** - Exactly n bytes with size proof
- **HashDigest** - Algorithm + value with consistency proof
- **StableABI** - Proof of platform-independent layout

### `Ochrance.ABI.Layout`
- Memory layout descriptors
- Alignment constraints
- Platform independence proofs
- C compatibility verification

### `Ochrance.ABI.Foreign`
- FFI function signatures
- Type-safe wrappers with size guarantees
- Determinism assertions
- Test vectors for ABI compliance

## Guarantees

1. **Size Safety**: Hash output size encoded in types
   ```idris
   blake3Hash : Vect n Byte -> IO (HashValue 32)  -- Always 32 bytes
   ```

2. **No Padding**: Byte arrays are tightly packed
   ```idris
   hashValueNoPadding : NoPadding (HashValue n)
   ```

3. **Platform Independent**: Layout doesn't depend on architecture
   ```idris
   hashValueLayoutIndependent : PlatformIndependent (hashValueLayout n)
   ```

4. **C Compatible**: Matches `uint8_t data[n]` layout
   ```idris
   hashValueCCompatible : CCompatible (HashValue n)
   ```

## Building

### Build Idris2 ABI
```bash
idris2 --build ochrance-abi.ipkg
```

### Build Zig FFI
```bash
cd ffi/zig
zig build
```

### Run Tests
```bash
zig build test  # Zig unit tests
idris2 --build ochrance-abi.ipkg  # Idris2 type checking
```

## FFI Contract

| Function | Input | Output | Safety |
|----------|-------|--------|--------|
| `blake3_hash` | `const uint8_t*, size_t` | `uint8_t[32]` | Buffer overflow impossible (Zig slice) |
| `sha256_hash` | `const uint8_t*, size_t` | `uint8_t[32]` | Buffer overflow impossible (Zig slice) |
| `sha3_256_hash` | `const uint8_t*, size_t` | `uint8_t[32]` | Buffer overflow impossible (Zig slice) |

## Test Vectors

Empty string hashes (for ABI verification):
- BLAKE3: `af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262`
- SHA-256: `e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`
- SHA3-256: `a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a`
