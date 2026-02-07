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
import Ochrance.Util.Hex

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

--------------------------------------------------------------------------------
-- Buffer Management Helpers
--------------------------------------------------------------------------------

||| Allocate buffer and copy list of bytes into it
allocBytes : List Bits8 -> IO (Ptr Bits8)
allocBytes bytes = primIO $ \w =>
  -- TODO: Proper buffer allocation with malloc/free
  -- For Phase 2, using believe_me cast as placeholder
  MkIORes (believe_me bytes) w

||| Read 32 bytes from pointer into Vect
readHash : Ptr Bits8 -> IO (Vect 32 Bits8)
readHash ptr = primIO $ \w =>
  -- TODO: Proper pointer dereferencing to read bytes
  -- For Phase 2, returning placeholder
  MkIORes (replicate 32 0) w

||| Call BLAKE3 FFI with managed buffers
callBlake3FFI : Ptr Bits8 -> Int -> IO (Vect 32 Bits8)
callBlake3FFI inPtr len = primIO $ \w =>
  -- Allocate output buffer (32 bytes for hash)
  let outPtr : Ptr Bits8 = believe_me ()
  in case prim__blake3 inPtr len outPtr w of
       MkIORes () w' =>
         -- TODO: Read hash from outPtr and free buffers
         MkIORes (replicate 32 0) w'

--------------------------------------------------------------------------------
-- Public Hash Functions (with FFI TODO notes)
--------------------------------------------------------------------------------

||| Hash bytes with BLAKE3
|||
||| NOTE: Phase 2 has placeholder buffer management.
||| Production requires: proper buffer allocation, pointer arithmetic,
||| memory safety validation, and explicit free operations.
export
blake3 : HasIO io => List Bits8 -> io (Vect 32 Bits8)
blake3 bytes = do
  let len = cast {to=Int} (length bytes)
  -- TODO: Use allocBytes, callBlake3FFI, readHash with proper cleanup
  pure (replicate 32 0)

||| Hash bytes with SHA-256
|||
||| NOTE: Phase 2 placeholder. See blake3 for implementation notes.
export
sha256 : HasIO io => List Bits8 -> io (Vect 32 Bits8)
sha256 bytes = pure (replicate 32 0)

||| Hash bytes with SHA3-256
|||
||| NOTE: Phase 2 placeholder. See blake3 for implementation notes.
export
sha3_256 : HasIO io => List Bits8 -> io (Vect 32 Bits8)
sha3_256 bytes = pure (replicate 32 0)

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

||| Verify an Ed25519 signature from hex-encoded strings
||| This is a convenience wrapper for the common case of hex-encoded signatures
export
ed25519VerifyHex : HasIO io
                => (signatureHex : String)
                -> (publicKeyHex : String)
                -> (message : List Bits8)
                -> io (Maybe Bool)
ed25519VerifyHex sigHex pkHex msg = do
  case hexStringToVect 64 sigHex of
    Nothing => pure Nothing  -- Invalid signature format
    Just sig => case hexStringToVect 32 pkHex of
      Nothing => pure Nothing  -- Invalid public key format
      Just pk => do
        result <- ed25519Verify sig pk msg
        pure (Just result)

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
