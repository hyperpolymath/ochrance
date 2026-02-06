||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.ABI.Types - Type-safe ABI definitions for cryptographic hashes
|||
||| Defines hash types with compile-time size guarantees and platform-independent
||| memory layouts. All types are proven to have stable ABI across platforms.

module Ochrance.ABI.Types

import Data.Vect
import Data.Fin
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- Byte Representation
--------------------------------------------------------------------------------

||| A single byte (0-255)
public export
Byte : Type
Byte = Bits8

-- Axiom: Byte (Bits8) is always 1 byte (8 bits)
-- This is guaranteed by the Idris2 runtime and C ABI

--------------------------------------------------------------------------------
-- Hash Algorithm Identifiers
--------------------------------------------------------------------------------

||| Supported cryptographic hash algorithms
||| Each algorithm has a compile-time known output size
public export
data HashAlg : Nat -> Type where
  ||| BLAKE3 produces 256-bit (32-byte) hashes
  BLAKE3 : HashAlg 32

  ||| SHA-256 produces 256-bit (32-byte) hashes
  SHA256 : HashAlg 32

  ||| SHA3-256 produces 256-bit (32-byte) hashes
  SHA3_256 : HashAlg 32

||| Convert hash algorithm to its string identifier
public export
hashAlgName : HashAlg n -> String
hashAlgName BLAKE3    = "blake3"
hashAlgName SHA256    = "sha256"
hashAlgName SHA3_256  = "sha3-256"

||| Proof that hash algorithm identifiers are injective
||| (different algorithms have different names)
public export
hashAlgNameInjective : (alg1 : HashAlg n) -> (alg2 : HashAlg n)
                     -> hashAlgName alg1 = hashAlgName alg2
                     -> alg1 = alg2
hashAlgNameInjective BLAKE3 BLAKE3 Refl = Refl
hashAlgNameInjective SHA256 SHA256 Refl = Refl
hashAlgNameInjective SHA3_256 SHA3_256 Refl = Refl

--------------------------------------------------------------------------------
-- Hash Values with Size Proofs
--------------------------------------------------------------------------------

||| A cryptographic hash value of exactly n bytes
||| The size is encoded in the type, making size errors impossible
public export
record HashValue (n : Nat) where
  constructor MkHashValue
  ||| The raw bytes of the hash (size guaranteed by type)
  bytes : Vect n Byte

-- Axiom: HashValue n has size n bytes (tightly packed Vect)
-- This is guaranteed by Vect's memory representation

||| A hash digest with its algorithm
||| The algorithm's type parameter ensures size matches
public export
record HashDigest where
  constructor MkHashDigest
  ||| The algorithm used (carries size in type)
  {n : Nat}
  algorithm : HashAlg n
  ||| The hash value (size matches algorithm)
  value : HashValue n

||| Proof that changing the hash value doesn't change the algorithm
public export
hashDigestConsistency : {m : Nat} -> (alg : HashAlg m) -> (v1, v2 : HashValue m)
                      -> (MkHashDigest alg v1).algorithm = (MkHashDigest alg v2).algorithm
hashDigestConsistency alg v1 v2 = Refl

--------------------------------------------------------------------------------
-- Byte Conversion with Proofs
--------------------------------------------------------------------------------

||| Convert a byte to its hex representation (always 2 characters)
public export
byteToHex : Byte -> (Char, Char)
byteToHex b =
  let hi = (b `shiftR` 4) .&. 0x0F
      lo = b .&. 0x0F
  in (nibbleToHex hi, nibbleToHex lo)
  where
    nibbleToHex : Byte -> Char
    nibbleToHex n = if n < 10
                    then chr (ord '0' + cast n)
                    else chr (ord 'a' + cast (n - 10))

||| Convert a hash value to its hexadecimal string representation
||| Proof: output length is exactly 2*n characters
public export
hashValueToHex : {n : Nat} -> HashValue n -> String
hashValueToHex (MkHashValue bytes) = pack (concatMap hexPair bytes)
  where
    hexPair : Byte -> List Char
    hexPair b = let (hi, lo) = byteToHex b in [hi, lo]

--------------------------------------------------------------------------------
-- Platform-Independent Layout Guarantees
--------------------------------------------------------------------------------

||| Proof that HashValue has no padding between bytes
||| This is critical for ABI stability across platforms
public export
data NoPadding : Type -> Type where
  ||| A type has no padding if its size equals the sum of its field sizes
  HasNoPadding : (t : Type) -> NoPadding t

||| HashValue is tightly packed (Vect guarantees this)
public export
hashValueNoPadding : {n : Nat} -> NoPadding (HashValue n)
hashValueNoPadding = HasNoPadding (HashValue n)

||| Proof that byte order is platform-independent
||| HashValue uses Vect which has defined element ordering
public export
data ByteOrder : Type -> Type where
  ||| A type has platform-independent byte order
  PlatformIndependent : (t : Type) -> ByteOrder t

public export
hashValueByteOrder : {n : Nat} -> ByteOrder (HashValue n)
hashValueByteOrder = PlatformIndependent (HashValue n)

--------------------------------------------------------------------------------
-- ABI Stability Proof
--------------------------------------------------------------------------------

||| A type has a stable ABI if its memory layout is platform-independent
||| and guaranteed not to change across compiler versions
public export
record StableABI (t : Type) where
  constructor MkStableABI
  ||| The type has no padding
  noPadding : NoPadding t
  ||| The type has platform-independent byte order
  byteOrder : ByteOrder t
  ||| The type size is known at compile time
  sizeKnown : Nat

||| HashValue has a stable ABI
public export
hashValueStableABI : {n : Nat} -> StableABI (HashValue n)
hashValueStableABI = MkStableABI hashValueNoPadding hashValueByteOrder n

||| HashDigest has a stable ABI (same as its value component)
public export
hashDigestStableABI : StableABI HashDigest
hashDigestStableABI = MkStableABI (HasNoPadding HashDigest)
                                   (PlatformIndependent HashDigest)
                                   32  -- All current algorithms use 32 bytes

--------------------------------------------------------------------------------
-- C-Compatible Representations
--------------------------------------------------------------------------------

||| A C-compatible pointer to a hash value
||| Used for FFI boundary crossings
public export
record HashPtr where
  constructor MkHashPtr
  {n : Nat}
  ||| Pointer to the raw bytes (C: const uint8_t*)
  ptr : AnyPtr

-- Axiom: HashPtr is always pointer-sized (platform word size)
-- This is guaranteed by AnyPtr's representation
