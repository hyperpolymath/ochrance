-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.FFI.Crypto - FFI bindings to libochrance cryptographic functions
|||
||| Provides access to BLAKE3, SHA-256, SHA3-256 hashing and Ed25519 signature
||| verification via Zig implementation. All functions are memory-safe with
||| defined ABIs.
|||
||| Architecture:
|||   Idris2 (this module) -> C FFI declarations -> Zig (ffi/zig/src/main.zig)
|||   The Zig code compiles to libochrance.so which is linked at runtime.
|||
||| Buffer protocol:
|||   1. Allocate input Buffer, write bytes into it
|||   2. Allocate output Buffer (32 bytes for hashes)
|||   3. Call FFI primitive
|||   4. Read result bytes from output Buffer
|||   5. Free both buffers (via GC / scope exit)

module Ochrance.FFI.Crypto

import Data.Vect
import Data.Bits
import Data.Buffer
import System.FFI
import Ochrance.Util.Hex
import Ochrance.Framework.Error

%default total

--------------------------------------------------------------------------------
-- FFI Declarations
--
-- These map directly to the C-compatible exports in ffi/zig/src/main.zig.
-- ABI contract for hash functions:
--   void hash_fn(const uint8_t* data, size_t len, uint8_t out[32])
-- ABI contract for ed25519_verify:
--   int ed25519_verify(const uint8_t[64] sig, const uint8_t[32] pk,
--                      const uint8_t* msg, size_t msg_len) -> 0|1
--------------------------------------------------------------------------------

%foreign "C:blake3_hash,libochrance"
prim__blake3 : Buffer -> Int -> Buffer -> PrimIO ()

%foreign "C:sha256_hash,libochrance"
prim__sha256 : Buffer -> Int -> Buffer -> PrimIO ()

%foreign "C:sha3_256_hash,libochrance"
prim__sha3_256 : Buffer -> Int -> Buffer -> PrimIO ()

%foreign "C:ed25519_verify,libochrance"
prim__ed25519_verify : Buffer -> Buffer -> Buffer -> Int -> PrimIO Int

--------------------------------------------------------------------------------
-- Buffer Helpers
--------------------------------------------------------------------------------

||| Write a list of bytes into a Buffer starting at the given offset.
||| Uses setBits8 (non-deprecated API).
writeBytesToBuffer : Buffer -> List Bits8 -> (offset : Int) -> IO ()
writeBytesToBuffer buf [] _ = pure ()
writeBytesToBuffer buf (b :: bs) offset = do
  setBits8 buf offset b
  writeBytesToBuffer buf bs (offset + 1)

||| Read n bytes from a Buffer starting at the given offset into a List.
||| Uses getBits8 (non-deprecated API).
readBytesFromBuffer : Buffer -> (remaining : Nat) -> (offset : Int) -> IO (List Bits8)
readBytesFromBuffer buf Z _ = pure []
readBytesFromBuffer buf (S k) offset = do
  b <- getBits8 buf offset
  rest <- readBytesFromBuffer buf k (offset + 1)
  pure (b :: rest)

||| Convert a List of exactly n elements to a Vect n, returning a default if
||| the list length does not match. This is safe because we control the buffer
||| read length.
listToVect32 : List Bits8 -> Vect 32 Bits8
listToVect32 bs = case toVect 32 bs of
  Just v  => v
  Nothing => replicate 32 0  -- Defensive: should never happen when called correctly
  where
    toVect : (n : Nat) -> List Bits8 -> Maybe (Vect n Bits8)
    toVect Z [] = Just []
    toVect (S k) (x :: xs) = map (x ::) (toVect k xs)
    toVect _ _ = Nothing

--------------------------------------------------------------------------------
-- Generic hash helper
--------------------------------------------------------------------------------

||| Call a hash FFI primitive with proper buffer management.
||| Allocates input and output buffers, calls the primitive, reads the result.
||| Returns Left on buffer allocation failure (z/out-of-memory).
callHashFFI : (Buffer -> Int -> Buffer -> PrimIO ())
           -> List Bits8
           -> IO (Either OchranceError (Vect 32 Bits8))
callHashFFI primFn bytes = do
  let len = cast {to=Int} (length bytes)
  -- Allocate input buffer (minimum 1 byte to avoid zero-length allocation)
  Just inBuf <- newBuffer (max 1 len)
    | Nothing => pure (Left (ZError OutOfMemory))
  -- Write input data
  writeBytesToBuffer inBuf bytes 0
  -- Allocate output buffer (32 bytes for hash digest)
  Just outBuf <- newBuffer 32
    | Nothing => pure (Left (ZError OutOfMemory))
  -- Call the FFI primitive
  primIO (primFn inBuf len outBuf)
  -- Read result bytes from output buffer
  resultBytes <- readBytesFromBuffer outBuf 32 0
  -- Note: Idris2 manages buffer lifecycle via garbage collection.
  -- Both inBuf and outBuf become unreachable after this scope exits
  -- and will be reclaimed by the GC. No explicit free is needed.
  pure (Right (listToVect32 resultBytes))

--------------------------------------------------------------------------------
-- Public Hash Functions (real FFI implementations)
--------------------------------------------------------------------------------

||| Hash bytes with BLAKE3 via libochrance.so FFI
|||
||| Allocates buffers, calls the Zig BLAKE3 implementation, returns 32-byte digest.
||| Returns Left on buffer allocation failure (z/out-of-memory).
export
blake3 : HasIO io => List Bits8 -> io (Either OchranceError (Vect 32 Bits8))
blake3 bytes = liftIO (callHashFFI prim__blake3 bytes)

||| Hash bytes with SHA-256 via libochrance.so FFI
|||
||| Allocates buffers, calls the Zig SHA-256 implementation, returns 32-byte digest.
||| Returns Left on buffer allocation failure (z/out-of-memory).
export
sha256 : HasIO io => List Bits8 -> io (Either OchranceError (Vect 32 Bits8))
sha256 bytes = liftIO (callHashFFI prim__sha256 bytes)

||| Hash bytes with SHA3-256 via libochrance.so FFI
|||
||| Allocates buffers, calls the Zig SHA3-256 implementation, returns 32-byte digest.
||| Returns Left on buffer allocation failure (z/out-of-memory).
export
sha3_256 : HasIO io => List Bits8 -> io (Either OchranceError (Vect 32 Bits8))
sha3_256 bytes = liftIO (callHashFFI prim__sha3_256 bytes)

--------------------------------------------------------------------------------
-- Stub fallbacks (pure, no FFI required — for testing without libochrance.so)
--------------------------------------------------------------------------------

||| Stub BLAKE3: returns XOR-folded placeholder. Use only for testing.
export
blake3Stub : List Bits8 -> Vect 32 Bits8
blake3Stub bytes = replicate 32 0

||| Stub SHA-256: returns zero hash. Use only for testing.
export
sha256Stub : List Bits8 -> Vect 32 Bits8
sha256Stub bytes = replicate 32 0

||| Stub SHA3-256: returns zero hash. Use only for testing.
export
sha3_256Stub : List Bits8 -> Vect 32 Bits8
sha3_256Stub bytes = replicate 32 0

--------------------------------------------------------------------------------
-- Ed25519 Signature Verification
--------------------------------------------------------------------------------

||| Verify an Ed25519 signature via libochrance.so FFI
||| Returns Right True if signature is valid, Right False if invalid.
||| Returns Left on buffer allocation failure (z/out-of-memory).
|||
||| Buffer layout:
|||   sigBuf  : 64 bytes (Ed25519 signature)
|||   pkBuf   : 32 bytes (Ed25519 public key)
|||   msgBuf  : variable length (message bytes)
export
ed25519Verify : HasIO io
             => (signature : Vect 64 Bits8)
             -> (publicKey : Vect 32 Bits8)
             -> (message : List Bits8)
             -> io (Either OchranceError Bool)
ed25519Verify sig pubkey msg = liftIO $ do
  let msgLen = cast {to=Int} (length msg)
  -- Allocate signature buffer (64 bytes)
  Just sigBuf <- newBuffer 64
    | Nothing => pure (Left (ZError OutOfMemory))
  writeBytesToBuffer sigBuf (toList sig) 0
  -- Allocate public key buffer (32 bytes)
  Just pkBuf <- newBuffer 32
    | Nothing => pure (Left (ZError OutOfMemory))
  writeBytesToBuffer pkBuf (toList pubkey) 0
  -- Allocate message buffer (minimum 1 byte to avoid zero-length allocation)
  Just msgBuf <- newBuffer (max 1 msgLen)
    | Nothing => pure (Left (ZError OutOfMemory))
  writeBytesToBuffer msgBuf msg 0
  -- Call FFI - all buffers become unreachable after scope exit (GC reclaims)
  result <- primIO (prim__ed25519_verify sigBuf pkBuf msgBuf msgLen)
  pure (Right (result == 1))

||| Stub Ed25519 verification: always returns False. Use only for testing.
export
ed25519VerifyStub : Vect 64 Bits8 -> Vect 32 Bits8 -> List Bits8 -> Bool
ed25519VerifyStub _ _ _ = False

||| Verify an Ed25519 signature from hex-encoded strings.
||| This is a convenience wrapper for the common case of hex-encoded signatures.
||| Returns Right (Just True/False) on success, Right Nothing if hex parsing fails,
||| Left on buffer allocation failure.
export
ed25519VerifyHex : HasIO io
                => (signatureHex : String)
                -> (publicKeyHex : String)
                -> (message : List Bits8)
                -> io (Either OchranceError (Maybe Bool))
ed25519VerifyHex sigHex pkHex msg = do
  case hexStringToVect 64 sigHex of
    Nothing => pure (Right Nothing)  -- Invalid signature format
    Just sig => case hexStringToVect 32 pkHex of
      Nothing => pure (Right Nothing)  -- Invalid public key format
      Just pk => do
        result <- ed25519Verify sig pk msg
        case result of
          Left err   => pure (Left err)
          Right valid => pure (Right (Just valid))

--------------------------------------------------------------------------------
-- Pure Hash Combiners (for Merkle trees)
--------------------------------------------------------------------------------

||| Combine two 32-byte hashes using XOR (pure stub for totality).
||| Use hashPairBlake3 in IO context for cryptographic hashing.
export
hashPairStub : Vect 32 Bits8 -> Vect 32 Bits8 -> Vect 32 Bits8
hashPairStub h1 h2 = zipWith xor h1 h2

||| IO version: Combine two 32-byte hashes using BLAKE3 via FFI.
||| Returns Left on buffer allocation failure (z/out-of-memory).
export
hashPairBlake3 : HasIO io => Vect 32 Bits8 -> Vect 32 Bits8
              -> io (Either OchranceError (Vect 32 Bits8))
hashPairBlake3 h1 h2 = blake3 (toList h1 ++ toList h2)
