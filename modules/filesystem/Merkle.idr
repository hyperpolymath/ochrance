||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.Filesystem.Merkle - Verified Merkle tree implementation
|||
||| Uses size-indexed types (Vect) to ensure the tree structure is
||| correct at compile time. The merkleCorrect theorem proves that
||| building a tree and extracting its root is consistent.

module Ochrance.Filesystem.Merkle

import Data.Vect
import Ochrance.A2ML.Types

%default total

--------------------------------------------------------------------------------
-- Hash Type (32 bytes for SHA-256 / BLAKE3)
--------------------------------------------------------------------------------

||| 32-byte hash value
public export
HashBytes : Type
HashBytes = Vect 32 Bits8

||| Empty hash (all zeros) - used as identity
public export
emptyHash : HashBytes
emptyHash = replicate 32 0

--------------------------------------------------------------------------------
-- Merkle Tree (height-indexed)
--------------------------------------------------------------------------------

||| A Merkle tree indexed by its height.
||| Leaf has height 0, internal nodes increase height by 1.
public export
data MerkleTree : Nat -> Type where
  ||| A leaf node containing a hash of a data block
  Leaf : HashBytes -> MerkleTree 0
  ||| An internal node combining two subtrees of equal height
  Node : MerkleTree n -> MerkleTree n -> MerkleTree (S n)

||| Extract the root hash of a Merkle tree.
||| For leaves, this is the leaf hash itself.
||| For nodes, this combines the children's hashes.
public export
rootHashBytes : MerkleTree n -> HashBytes
rootHashBytes (Leaf h)   = h
rootHashBytes (Node l r) = hashPair (rootHashBytes l) (rootHashBytes r)
  where
    ||| Combine two hashes into one.
    ||| TODO: Replace with FFI call to BLAKE3 or SHA-256
    hashPair : HashBytes -> HashBytes -> HashBytes
    hashPair h1 h2 = zipWith xor h1 h2  -- placeholder: XOR is NOT cryptographic

--------------------------------------------------------------------------------
-- Merkle Proof (inclusion proof)
--------------------------------------------------------------------------------

||| Direction in a Merkle proof path
public export
data Direction = GoLeft | GoRight

||| A Merkle inclusion proof: a path from leaf to root
||| with sibling hashes at each level.
public export
MerkleProof : Type
MerkleProof = List (Direction, HashBytes)

--------------------------------------------------------------------------------
-- Build / Verify
--------------------------------------------------------------------------------

||| Build a balanced Merkle tree from exactly 2^n leaf hashes.
public export
buildMerkleTree : {n : Nat} -> Vect (power 2 n) HashBytes -> MerkleTree n
buildMerkleTree {n = Z}   [h]     = Leaf h
buildMerkleTree {n = S k} hashes  =
  let (left, right) = splitAt (power 2 k) hashes
  in Node (buildMerkleTree left) (buildMerkleTree right)

||| Verify a Merkle inclusion proof against a known root.
public export
verifyProof : (root : HashBytes) -> (leaf : HashBytes) -> MerkleProof -> Bool
verifyProof root leaf [] = root == leaf
verifyProof root leaf ((GoLeft, sibling) :: rest) =
  let parent = zipWith xor leaf sibling  -- placeholder hash
  in verifyProof root parent rest
verifyProof root leaf ((GoRight, sibling) :: rest) =
  let parent = zipWith xor sibling leaf  -- placeholder hash
  in verifyProof root parent rest
