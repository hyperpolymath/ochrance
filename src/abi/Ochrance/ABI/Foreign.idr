||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.ABI.Foreign - FFI function signatures with dependent type contracts
|||
||| Defines the interface to Zig-implemented hashing functions.
||| Simplified version - full implementations to be added incrementally.

module Ochrance.ABI.Foreign

import Ochrance.ABI.Types
import Ochrance.ABI.Layout
import Data.Vect
import Data.Buffer

%default total

--------------------------------------------------------------------------------
-- FFI Function Signatures
--------------------------------------------------------------------------------

||| BLAKE3 hash function: data -> 32-byte hash
||| FFI: void blake3_hash(const uint8_t* data, size_t len, uint8_t out[32]);
%foreign "C:blake3_hash,libochrance"
prim__blake3 : Buffer -> Int -> Buffer -> PrimIO ()

||| SHA-256 hash function: data -> 32-byte hash  
||| FFI: void sha256_hash(const uint8_t* data, size_t len, uint8_t out[32]);
%foreign "C:sha256_hash,libochrance"
prim__sha256 : Buffer -> Int -> Buffer -> PrimIO ()

||| SHA3-256 hash function: data -> 32-byte hash
||| FFI: void sha3_256_hash(const uint8_t* data, size_t len, uint8_t out[32]);
%foreign "C:sha3_256_hash,libochrance"
prim__sha3_256 : Buffer -> Int -> Buffer -> PrimIO ()

--------------------------------------------------------------------------------
-- Placeholder Implementations
--------------------------------------------------------------------------------

||| Type-safe BLAKE3 hash with size guarantees
||| TODO: Implement buffer conversion once libochrance.so is built
export
covering
blake3Hash : {n : Nat} -> Vect n Byte -> IO (HashValue 32)
blake3Hash input = pure (MkHashValue (replicate 32 0))  -- Stub

||| Type-safe SHA-256 hash with size guarantees
export
covering
sha256Hash : {n : Nat} -> Vect n Byte -> IO (HashValue 32)
sha256Hash input = pure (MkHashValue (replicate 32 0))  -- Stub

||| Type-safe SHA3-256 hash with size guarantees
export
covering
sha3_256Hash : {n : Nat} -> Vect n Byte -> IO (HashValue 32)
sha3_256Hash input = pure (MkHashValue (replicate 32 0))  -- Stub

--------------------------------------------------------------------------------
-- Polymorphic Hash Function
--------------------------------------------------------------------------------

||| Hash data using the specified algorithm
||| The output size is determined by the algorithm's type
export
covering
hashWith : {n : Nat} -> HashAlg n -> {m : Nat} -> Vect m Byte -> IO (HashValue n)
hashWith BLAKE3 input = blake3Hash input
hashWith SHA256 input = sha256Hash input
hashWith SHA3_256 input = sha3_256Hash input

||| Create a complete HashDigest
export
covering
computeDigest : {n : Nat} -> (alg : HashAlg n) -> {m : Nat} -> Vect m Byte -> IO HashDigest
computeDigest alg input = do
  value <- hashWith alg input
  pure (MkHashDigest alg value)

--------------------------------------------------------------------------------
-- String Interface (for A2ML integration)
--------------------------------------------------------------------------------

||| Hash a string using the specified algorithm
export
covering
hashString : {n : Nat} -> HashAlg n -> String -> IO HashDigest
hashString alg str = computeDigest alg (fromList (map cast (unpack str)))

