||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.FFI.Crypto - FFI bindings to libochrance cryptographic functions
|||
||| Provides access to BLAKE3, SHA-256, and SHA3-256 hashing via Zig
||| implementation. All functions are memory-safe with defined ABIs.

module Ochrance.FFI.Crypto

import Data.Vect
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- Foreign Declarations (Stubbed - FFI not fully implemented)
--------------------------------------------------------------------------------

-- FFI declarations removed for Phase 1 build
-- TODO: Add proper FFI declarations when libochrance.so is integrated

--------------------------------------------------------------------------------
-- Stub Wrappers (FFI not integrated yet)
--------------------------------------------------------------------------------

||| Hash bytes with BLAKE3 (stub - returns placeholder)
export
blake3 : HasIO io => List Bits8 -> io (Vect 32 Bits8)
blake3 bytes = pure (replicate 32 0)  -- Stub: returns zeros

||| Hash bytes with SHA-256 (stub - returns placeholder)
export
sha256 : HasIO io => List Bits8 -> io (Vect 32 Bits8)
sha256 bytes = pure (replicate 32 0)  -- Stub: returns zeros

||| Hash bytes with SHA3-256 (stub - returns placeholder)
export
sha3_256 : HasIO io => List Bits8 -> io (Vect 32 Bits8)
sha3_256 bytes = pure (replicate 32 0)  -- Stub: returns zeros

--------------------------------------------------------------------------------
-- Pure Hash Combiners (for Merkle trees)
--------------------------------------------------------------------------------

||| Combine two 32-byte hashes using BLAKE3
||| This is a pure function stub - actual hashing requires IO
||| Use this in IO context: hashPairBlake3 h1 h2
export
hashPairStub : Vect 32 Bits8 -> Vect 32 Bits8 -> Vect 32 Bits8
hashPairStub h1 h2 =
  -- STUB: In production, this would call blake3 via unsafePerformIO
  -- For now, return XOR placeholder to maintain totality
  zipWith xor h1 h2

||| IO version: Combine two 32-byte hashes using BLAKE3
export
hashPairBlake3 : HasIO io => Vect 32 Bits8 -> Vect 32 Bits8 -> io (Vect 32 Bits8)
hashPairBlake3 h1 h2 = blake3 (toList h1 ++ toList h2)
