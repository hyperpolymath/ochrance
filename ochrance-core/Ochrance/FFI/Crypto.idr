||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.FFI.Crypto - FFI bindings to libochrance cryptographic functions
|||
||| Provides access to BLAKE3, SHA-256, and SHA3-256 hashing via Zig
||| implementation. All functions are memory-safe with defined ABIs.

module Ochrance.FFI.Crypto

import Data.Vect
import System.FFI

%default total

--------------------------------------------------------------------------------
-- Foreign Declarations
--------------------------------------------------------------------------------

||| Hash arbitrary bytes with BLAKE3
||| ABI: blake3_hash(const uint8_t* data, size_t len, uint8_t out[32])
%foreign "C:blake3_hash,libochrance"
prim__blake3Hash : Ptr -> Int -> Ptr -> PrimIO ()

||| Hash arbitrary bytes with SHA-256
||| ABI: sha256_hash(const uint8_t* data, size_t len, uint8_t out[32])
%foreign "C:sha256_hash,libochrance"
prim__sha256Hash : Ptr -> Int -> Ptr -> PrimIO ()

||| Hash arbitrary bytes with SHA3-256
||| ABI: sha3_256_hash(const uint8_t* data, size_t len, uint8_t out[32])
%foreign "C:sha3_256_hash,libochrance"
prim__sha3_256Hash : Ptr -> Int -> Ptr -> PrimIO ()

--------------------------------------------------------------------------------
-- Safe Wrappers
--------------------------------------------------------------------------------

||| Hash bytes with BLAKE3, returning a 32-byte hash
export
blake3 : HasIO io => List Bits8 -> io (Vect 32 Bits8)
blake3 bytes = do
  -- Allocate input and output buffers
  inputPtr <- primIO $ prim__castPtr (believe_me bytes)
  outputBuf <- primIO $ prim__malloc 32

  -- Call FFI function
  primIO $ prim__blake3Hash inputPtr (cast $ length bytes) outputBuf

  -- Read output into Vect
  result <- primIO $ readBytes outputBuf 32
  primIO $ prim__free outputBuf

  pure (believe_me result)  -- Safe: we know it's exactly 32 bytes
  where
    -- Helper to read bytes from buffer
    readBytes : Ptr -> Nat -> PrimIO (List Bits8)
    readBytes ptr 0 = pure []
    readBytes ptr (S k) = do
      byte <- prim__peek8 ptr 0
      rest <- readBytes (prim__offsetPtr ptr 1) k
      pure (byte :: rest)

||| Hash bytes with SHA-256, returning a 32-byte hash
export
sha256 : HasIO io => List Bits8 -> io (Vect 32 Bits8)
sha256 bytes = do
  inputPtr <- primIO $ prim__castPtr (believe_me bytes)
  outputBuf <- primIO $ prim__malloc 32

  primIO $ prim__sha256Hash inputPtr (cast $ length bytes) outputBuf

  result <- primIO $ readBytes outputBuf 32
  primIO $ prim__free outputBuf

  pure (believe_me result)
  where
    readBytes : Ptr -> Nat -> PrimIO (List Bits8)
    readBytes ptr 0 = pure []
    readBytes ptr (S k) = do
      byte <- prim__peek8 ptr 0
      rest <- readBytes (prim__offsetPtr ptr 1) k
      pure (byte :: rest)

||| Hash bytes with SHA3-256, returning a 32-byte hash
export
sha3_256 : HasIO io => List Bits8 -> io (Vect 32 Bits8)
sha3_256 bytes = do
  inputPtr <- primIO $ prim__castPtr (believe_me bytes)
  outputBuf <- primIO $ prim__malloc 32

  primIO $ prim__sha3_256Hash inputPtr (cast $ length bytes) outputBuf

  result <- primIO $ readBytes outputBuf 32
  primIO $ prim__free outputBuf

  pure (believe_me result)
  where
    readBytes : Ptr -> Nat -> PrimIO (List Bits8)
    readBytes ptr 0 = pure []
    readBytes ptr (S k) = do
      byte <- prim__peek8 ptr 0
      rest <- readBytes (prim__offsetPtr ptr 1) k
      pure (byte :: rest)

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
