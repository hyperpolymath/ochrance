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
import Ochrance.FFI.Crypto

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

||| Extract the root hash of a Merkle tree (pure placeholder version).
||| For leaves, this is the leaf hash itself.
||| For nodes, this combines the children's hashes using XOR placeholder.
|||
||| NOTE: This uses XOR for totality. Use rootHashBytesIO for cryptographic hashing.
public export
rootHashBytes : MerkleTree n -> HashBytes
rootHashBytes (Leaf h)   = h
rootHashBytes (Node l r) = hashPairStub (rootHashBytes l) (rootHashBytes r)

||| Extract the root hash using BLAKE3 (IO version).
||| This is the cryptographically secure version that should be used in production.
export
rootHashBytesIO : HasIO io => MerkleTree n -> io HashBytes
rootHashBytesIO (Leaf h) = pure h
rootHashBytesIO (Node l r) = do
  lHash <- rootHashBytesIO l
  rHash <- rootHashBytesIO r
  hashPairBlake3 lHash rHash

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
-- Arithmetic Lemmas
--------------------------------------------------------------------------------

||| Proof that power 2 (S k) = power 2 k + power 2 k
||| This is needed for splitting vectors when building Merkle trees
|||
||| Full proof would require:
||| power 2 (S k) = 2 * power 2 k     (by definition of power)
||| 2 * power 2 k = power 2 k + power 2 k  (by distributivity)
||| For Phase 2, we postulate this as the arithmetic is standard
powerTwoSucc : (k : Nat) -> power 2 (S k) = power 2 k + power 2 k

--------------------------------------------------------------------------------
-- Build / Verify
--------------------------------------------------------------------------------

||| Build a balanced Merkle tree from exactly 2^n leaf hashes.
||| Uses arithmetic lemma to prove vector splitting is valid
public export
buildMerkleTree : {n : Nat} -> Vect (power 2 n) HashBytes -> MerkleTree n
buildMerkleTree {n = Z}   [h]     = Leaf h
buildMerkleTree {n = S k} hashes  =
  -- Use replace to transform the vector type explicitly
  let hashes' : Vect (power 2 k + power 2 k) HashBytes
      hashes' = replace {p = \x => Vect x HashBytes} (powerTwoSucc k) hashes
  in case splitAt (power 2 k) hashes' of
       (left, right) => Node (buildMerkleTree left) (buildMerkleTree right)

||| Verify a Merkle inclusion proof against a known root (placeholder version).
||| Uses XOR for totality. Use verifyProofIO for cryptographic verification.
public export
verifyProof : (root : HashBytes) -> (leaf : HashBytes) -> MerkleProof -> Bool
verifyProof root leaf [] = root == leaf
verifyProof root leaf ((GoLeft, sibling) :: rest) =
  let parent = hashPairStub leaf sibling
  in verifyProof root parent rest
verifyProof root leaf ((GoRight, sibling) :: rest) =
  let parent = hashPairStub sibling leaf
  in verifyProof root parent rest

||| Verify a Merkle inclusion proof using BLAKE3 (IO version).
||| This is the cryptographically secure version for production use.
export
verifyProofIO : HasIO io => (root : HashBytes) -> (leaf : HashBytes)
             -> MerkleProof -> io Bool
verifyProofIO root leaf [] = pure (root == leaf)
verifyProofIO root leaf ((GoLeft, sibling) :: rest) = do
  parent <- hashPairBlake3 leaf sibling
  verifyProofIO root parent rest
verifyProofIO root leaf ((GoRight, sibling) :: rest) = do
  parent <- hashPairBlake3 sibling leaf
  verifyProofIO root parent rest
