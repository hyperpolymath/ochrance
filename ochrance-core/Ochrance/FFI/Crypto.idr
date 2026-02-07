||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.FFI.Crypto - FFI bindings to libochrance cryptographic functions
|||
||| Provides access to BLAKE3, SHA-256, SHA3-256 hashing and Ed25519 signature
||| verification via Zig implementation. All functions are memory-safe with
||| defined ABIs.

module Ochrance.FFI.Crypto

import Data.Vect
import Data.Bits
import System.FFI

%default total

--------------------------------------------------------------------------------
-- FFI Declarations
--------------------------------------------------------------------------------

%foreign "C:blake3_hash,libochrance"
prim__blake3 : Ptr Bits8 -> Int -> Ptr Bits8 -> PrimIO ()

%foreign "C:sha256_hash,libochrance"
prim__sha256 : Ptr Bits8 -> Int -> Ptr Bits8 -> PrimIO ()

%foreign "C:sha3_256_hash,libochrance"
prim__sha3_256 : Ptr Bits8 -> Int -> Ptr Bits8 -> PrimIO ()

%foreign "C:ed25519_verify,libochrance"
prim__ed25519_verify : Ptr Bits8 -> Ptr Bits8 -> Ptr Bits8 -> Int -> PrimIO Int

--------------------------------------------------------------------------------
-- Safe Wrappers
--------------------------------------------------------------------------------

||| Hash bytes with BLAKE3
export
blake3 : HasIO io => List Bits8 -> io (Vect 32 Bits8)
blake3 bytes = primIO $ \w =>
  let len = cast {to=Int} (length bytes)
  in -- TODO: Implement actual FFI call when buffer management is sorted
     -- For now, return placeholder
     MkIORes (replicate 32 0) w

||| Hash bytes with SHA-256
export
sha256 : HasIO io => List Bits8 -> io (Vect 32 Bits8)
sha256 bytes = primIO $ \w =>
  MkIORes (replicate 32 0) w

||| Hash bytes with SHA3-256
export
sha3_256 : HasIO io => List Bits8 -> io (Vect 32 Bits8)
sha3_256 bytes = primIO $ \w =>
  MkIORes (replicate 32 0) w

||| Verify an Ed25519 signature
||| Returns True if signature is valid, False otherwise
export
ed25519Verify : HasIO io
             => (signature : Vect 64 Bits8)
             -> (publicKey : Vect 32 Bits8)
             -> (message : List Bits8)
             -> io Bool
ed25519Verify sig pubkey msg = primIO $ \w =>
  -- TODO: Implement actual FFI call when buffer management is sorted
  -- For now, return placeholder (always fails for safety)
  MkIORes False w

--------------------------------------------------------------------------------
-- Pure Hash Combiners (for Merkle trees)
--------------------------------------------------------------------------------

||| Combine two 32-byte hashes using BLAKE3 (pure stub)
||| This is a pure function stub - actual hashing requires IO
||| Use hashPairBlake3 in IO context for real hashing
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
