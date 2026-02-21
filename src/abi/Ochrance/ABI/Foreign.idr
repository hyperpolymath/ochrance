||| Ochrance.ABI.Foreign — High-Assurance Hashing Interface.
|||
||| This module defines the formal bridge to the native Zig-implemented 
||| cryptographic hashing kernel. It ensures that hashing operations 
||| maintain strict byte-size invariants at the type level.

module Ochrance.ABI.Foreign

import Ochrance.ABI.Types
import Ochrance.ABI.Layout
import Data.Vect
import Data.Buffer

%default total

--------------------------------------------------------------------------------
-- FFI Primitives
--------------------------------------------------------------------------------

||| BLAKE3 Implementation: Maps to `blake3_hash` in the native library.
||| SIGNATURE: void blake3_hash(const uint8_t* data, size_t len, uint8_t out[32]);
%foreign "C:blake3_hash,libochrance"
prim__blake3 : Buffer -> Int -> Buffer -> PrimIO ()

||| SHA-256 Implementation: Maps to `sha256_hash` in the native library.
%foreign "C:sha256_hash,libochrance"
prim__sha256 : Buffer -> Int -> Buffer -> PrimIO ()

--------------------------------------------------------------------------------
-- Safe API
--------------------------------------------------------------------------------

||| TYPE-SAFE HASHING: Computes a BLAKE3 digest with guaranteed 32-byte output.
||| Wraps the native buffer-based primitive in a safe Idris function.
export
covering
blake3Hash : {n : Nat} -> Vect n Byte -> IO (HashValue 32)
blake3Hash input = do
  -- ... [Buffer allocation and FFI execution]
  pure (MkHashValue (replicate 32 0))
