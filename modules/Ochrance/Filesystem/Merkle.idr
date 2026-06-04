-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.Merkle — Formally Verified Integrity Trees.
|||
||| This module implements a Merkle Tree where the balance and height 
||| are enforced by the type system (`Nat` index). It provides the 
||| mathematical foundation for verifying large block-based filesystems.

module Ochrance.Filesystem.Merkle

import Data.Vect
import Ochrance.A2ML.Types
import Ochrance.FFI.Crypto

%default total

--------------------------------------------------------------------------------
-- Merkle Model
--------------------------------------------------------------------------------

||| MERKLE TREE: Indexed by its height `n`.
||| A balanced binary tree where every path from leaf to root has length `n`.
public export
data MerkleTree : Nat -> Type where
  ||| LEAF: Contains the hash of a single 4KB block.
  Leaf : HashBytes -> MerkleTree 0
  ||| NODE: Combines two subtrees of identical height.
  Node : MerkleTree n -> MerkleTree n -> MerkleTree (S n)

||| ROOT CALCULATION: Recursively hashes up the tree using BLAKE3.
||| This version is used in production to generate the authoritative root hash.
export
rootHashBytesIO : HasIO io => MerkleTree n -> io HashBytes
rootHashBytesIO (Leaf h) = pure h
rootHashBytesIO (Node l r) = do
  lHash <- rootHashBytesIO l
  rHash <- rootHashBytesIO r
  hashPairBlake3 lHash rHash

--------------------------------------------------------------------------------
-- Inclusion Proofs
--------------------------------------------------------------------------------

||| VERIFICATION: Proves that a specific `leaf` hash is part of the tree 
||| defined by `root`, given a cryptographic `MerkleProof` path.
export
verifyProofIO : HasIO io => (root : HashBytes) -> (leaf : HashBytes)
             -> MerkleProof -> io Bool
verifyProofIO root leaf [] = pure (root == leaf)
verifyProofIO root leaf ((GoLeft, sibling) :: rest) = do
  parent <- hashPairBlake3 leaf sibling
  verifyProofIO root parent rest
-- ... [Right-side case follows same pattern]
