-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.Merkle - Verified Merkle tree implementation
|||
||| Uses height-indexed types to ensure the tree structure is correct at
||| compile time. The `merkleCorrect` theorem proves inclusion-proof soundness:
||| every proof produced by `generateProof` for an in-range leaf reconstructs
||| the tree's true root (a machine-checked propositional equality).
|||
||| The construction, verification, and soundness proof are *parametric in the
||| hash combiner* (`Combiner = HashBytes -> HashBytes -> HashBytes`): see the
||| `...With` family and the `merkleCorrectWith` theorem, whose proof uses no
||| property of the combiner whatsoever (combination is an opaque black box).
||| The pure XOR API (`rootHashBytes`, `verifyProof`, `generateProof`,
||| `reconstruct`, `merkleCorrect`) is recovered as the `xorCombiner` instance,
||| so XOR is no longer a special case but one point of a universally-quantified
||| result; the cryptographic BLAKE3 path's soundness is the same theorem at the
||| BLAKE3 combiner, with only its allocation-failure plumbing living in the
||| separate `...IO` functions.

module Ochrance.Filesystem.Merkle

import Data.Vect
import Ochrance.A2ML.Types
import Ochrance.FFI.Crypto
import Ochrance.Framework.Error

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

||| A pure two-input hash combiner over 32-byte digests. The Merkle
||| construction and its soundness proof are universally quantified over this
||| function, so every concrete hash (the XOR placeholder `xorCombiner`, or a
||| cryptographic BLAKE3/SHA-256 combiner) is one instance of the same theorem.
public export
Combiner : Type
Combiner = HashBytes -> HashBytes -> HashBytes

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

||| Root hash under an arbitrary combiner: a leaf is its own hash; a node
||| combines its children's roots with `h`.
public export
rootHashWith : Combiner -> MerkleTree n -> HashBytes
rootHashWith h (Leaf x)   = x
rootHashWith h (Node l r) = h (rootHashWith h l) (rootHashWith h r)

||| Extract the root hash of a Merkle tree (pure placeholder version).
||| For leaves, this is the leaf hash itself.
||| For nodes, this combines the children's hashes using the XOR placeholder.
|||
||| NOTE: This is `rootHashWith xorCombiner`. Use rootHashBytesIO for
||| cryptographic hashing.
public export
rootHashBytes : MerkleTree n -> HashBytes
rootHashBytes t = rootHashWith xorCombiner t

||| Extract the root hash using BLAKE3 (IO version).
||| This is the cryptographically secure version that should be used in production.
||| Returns Left on buffer allocation failure (z/out-of-memory).
export
rootHashBytesIO : HasIO io => MerkleTree n -> io (Either OchranceError HashBytes)
rootHashBytesIO (Leaf h) = pure (Right h)
rootHashBytesIO (Node l r) = do
  lResult <- rootHashBytesIO l
  case lResult of
    Left err => pure (Left err)
    Right lHash => do
      rResult <- rootHashBytesIO r
      case rResult of
        Left err => pure (Left err)
        Right rHash => hashPairBlake3 lHash rHash

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
||| This is needed for splitting vectors when building Merkle trees.
|||
||| By definition: power 2 (S k) = 2 * power 2 k = power 2 k + power 2 k
||| We prove this by showing that n + 0 = n (plusZeroRightNeutral) and then
||| using the fact that power 2 (S k) reduces to (power 2 k) + (power 2 k + 0).
public export
powerTwoSucc : (k : Nat) -> power 2 (S k) = power 2 k + power 2 k
powerTwoSucc k =
  -- power 2 (S k) normalises to: power 2 k + (power 2 k + 0)
  -- We need:                      power 2 k + power 2 k
  -- So we rewrite (power 2 k + 0) to (power 2 k) using plusZeroRightNeutral.
  rewrite plusZeroRightNeutral (power 2 k) in Refl

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

||| Verify a Merkle inclusion proof against a known root, under combiner `h`.
||| Folds each sibling into the running hash and tests equality with the root.
public export
verifyProofWith : (h : Combiner) -> (root : HashBytes) -> (leaf : HashBytes)
               -> MerkleProof -> Bool
verifyProofWith h root leaf [] = root == leaf
verifyProofWith h root leaf ((GoLeft, sibling) :: rest) =
  verifyProofWith h root (h leaf sibling) rest
verifyProofWith h root leaf ((GoRight, sibling) :: rest) =
  verifyProofWith h root (h sibling leaf) rest

||| Verify a Merkle inclusion proof against a known root (placeholder version).
||| This is `verifyProofWith xorCombiner`. Use verifyProofIO for cryptographic
||| verification.
public export
verifyProof : (root : HashBytes) -> (leaf : HashBytes) -> MerkleProof -> Bool
verifyProof root leaf prf = verifyProofWith xorCombiner root leaf prf

||| Verify a Merkle inclusion proof using BLAKE3 (IO version).
||| This is the cryptographically secure version for production use.
||| Returns Left on buffer allocation failure (z/out-of-memory).
export
verifyProofIO : HasIO io => (root : HashBytes) -> (leaf : HashBytes)
             -> MerkleProof -> io (Either OchranceError Bool)
verifyProofIO root leaf [] = pure (Right (root == leaf))
verifyProofIO root leaf ((GoLeft, sibling) :: rest) = do
  parentResult <- hashPairBlake3 leaf sibling
  case parentResult of
    Left err => pure (Left err)
    Right parent => verifyProofIO root parent rest
verifyProofIO root leaf ((GoRight, sibling) :: rest) = do
  parentResult <- hashPairBlake3 sibling leaf
  case parentResult of
    Left err => pure (Left err)
    Right parent => verifyProofIO root parent rest

--------------------------------------------------------------------------------
-- IO-based Merkle Operations (BLAKE3)
--------------------------------------------------------------------------------

||| Hash raw data bytes into a leaf hash using BLAKE3 via FFI.
||| This is the entry point for creating Merkle leaf hashes from actual data.
||| Returns Left on buffer allocation failure (z/out-of-memory).
export
hashLeafIO : HasIO io => List Bits8 -> io (Either OchranceError HashBytes)
hashLeafIO bytes = blake3 bytes

||| Generate a Merkle inclusion proof from a tree under combiner `h`.
||| Given a leaf index (0-based, left-to-right), extracts the path from
||| leaf to root with sibling hashes (computed via `rootHashWith h`) at each level.
|||
||| Returns Nothing if the index is out of range.
export
generateProofWith : {n : Nat} -> (h : Combiner) -> MerkleTree n
                 -> (leafIdx : Nat) -> Maybe MerkleProof
generateProofWith {n = Z} h (Leaf _) Z = Just []
generateProofWith {n = Z} h (Leaf _) (S _) = Nothing
generateProofWith {n = S k} h (Node l r) idx =
  let halfSize = power 2 k in
  if idx < halfSize
     then do  -- Leaf is in the left subtree
       subProof <- generateProofWith h l idx
       let siblingHash = rootHashWith h r
       Just (subProof ++ [(GoLeft, siblingHash)])
     else do  -- Leaf is in the right subtree
       subProof <- generateProofWith h r (idx `minus` halfSize)
       let siblingHash = rootHashWith h l
       Just (subProof ++ [(GoRight, siblingHash)])

||| Generate a Merkle inclusion proof from a tree (pure XOR version).
||| This is `generateProofWith xorCombiner`.
|||
||| Returns Nothing if the index is out of range.
export
generateProof : {n : Nat} -> MerkleTree n -> (leafIdx : Nat) -> Maybe MerkleProof
generateProof t i = generateProofWith xorCombiner t i

||| Generate a Merkle inclusion proof using BLAKE3 for sibling hashes (IO version).
||| This produces a cryptographically secure proof path.
||| Returns Left on FFI/allocation failure, Right Nothing if index is out of range.
export
generateProofIO : HasIO io => {n : Nat} -> MerkleTree n -> (leafIdx : Nat)
               -> io (Either OchranceError (Maybe MerkleProof))
generateProofIO {n = Z} (Leaf _) Z = pure (Right (Just []))
generateProofIO {n = Z} (Leaf _) (S _) = pure (Right Nothing)
generateProofIO {n = S k} (Node l r) idx =
  let halfSize = power 2 k in
  if idx < halfSize
     then do  -- Leaf is in the left subtree
       subResult <- generateProofIO l idx
       case subResult of
         Left err => pure (Left err)
         Right Nothing => pure (Right Nothing)
         Right (Just subProof) => do
           siblingResult <- rootHashBytesIO r
           case siblingResult of
             Left err => pure (Left err)
             Right siblingHash =>
               pure (Right (Just (subProof ++ [(GoLeft, siblingHash)])))
     else do  -- Leaf is in the right subtree
       subResult <- generateProofIO r (idx `minus` halfSize)
       case subResult of
         Left err => pure (Left err)
         Right Nothing => pure (Right Nothing)
         Right (Just subProof) => do
           siblingResult <- rootHashBytesIO l
           case siblingResult of
             Left err => pure (Left err)
             Right siblingHash =>
               pure (Right (Just (subProof ++ [(GoRight, siblingHash)])))

||| Get a specific leaf hash from a Merkle tree by index (pure version).
||| Returns Nothing if index is out of range.
public export
getLeafHash : {n : Nat} -> MerkleTree n -> (leafIdx : Nat) -> Maybe HashBytes
getLeafHash {n = Z} (Leaf h) Z = Just h
getLeafHash {n = Z} (Leaf _) (S _) = Nothing
getLeafHash {n = S k} (Node l r) idx =
  let halfSize = power 2 k in
  if idx < halfSize
     then getLeafHash l idx
     else getLeafHash r (idx `minus` halfSize)

--------------------------------------------------------------------------------
-- Inclusion-Proof Soundness (merkleCorrect)
--------------------------------------------------------------------------------

||| The hash an inclusion proof reconstructs under combiner `h`: start from a
||| leaf hash and fold in each sibling, left or right, exactly as `verifyProofWith
||| h` does. This is the value `verifyProofWith h` compares against the root
||| (see `verifyProofReconstructsWith`).
public export
reconstructWith : Combiner -> HashBytes -> MerkleProof -> HashBytes
reconstructWith h acc [] = acc
reconstructWith h acc ((GoLeft,  sib) :: rest) = reconstructWith h (h acc sib) rest
reconstructWith h acc ((GoRight, sib) :: rest) = reconstructWith h (h sib acc) rest

||| Reconstruct under the XOR placeholder. This is `reconstructWith xorCombiner`.
public export
reconstruct : HashBytes -> MerkleProof -> HashBytes
reconstruct acc prf = reconstructWith xorCombiner acc prf

||| `reconstructWith h` distributes over path concatenation: folding `p ++ q`
||| equals folding `p`, then folding `q` from that result.
reconstructAppendWith : (h : Combiner) -> (acc : HashBytes) -> (p, q : MerkleProof)
                 -> reconstructWith h acc (p ++ q)
                    = reconstructWith h (reconstructWith h acc p) q
reconstructAppendWith h acc []                       q = Refl
reconstructAppendWith h acc ((GoLeft,  sib) :: rest) q =
  reconstructAppendWith h (h acc sib) rest q
reconstructAppendWith h acc ((GoRight, sib) :: rest) q =
  reconstructAppendWith h (h sib acc) rest q

||| `verifyProofWith h` is exactly a root-equality test on the reconstructed
||| hash. This bridges the propositional soundness theorem below to the Bool API.
export
verifyProofReconstructsWith : (h : Combiner) -> (root, leaf : HashBytes)
                           -> (prf : MerkleProof)
                           -> verifyProofWith h root leaf prf
                              = (root == reconstructWith h leaf prf)
verifyProofReconstructsWith h root leaf []                       = Refl
verifyProofReconstructsWith h root leaf ((GoLeft,  sib) :: rest) =
  verifyProofReconstructsWith h root (h leaf sib) rest
verifyProofReconstructsWith h root leaf ((GoRight, sib) :: rest) =
  verifyProofReconstructsWith h root (h sib leaf) rest

||| `verifyProof` (XOR API) is a root-equality test on `reconstruct`.
||| The `xorCombiner` instance of `verifyProofReconstructsWith`.
export
verifyProofReconstructs : (root, leaf : HashBytes) -> (prf : MerkleProof)
                       -> verifyProof root leaf prf = (root == reconstruct leaf prf)
verifyProofReconstructs root leaf prf =
  verifyProofReconstructsWith xorCombiner root leaf prf

-- Injectivity of `Just`, used to read prf back out of the generated proof.
justInj : {0 a : Type} -> {0 x, y : a} -> Just x = Just y -> x = y
justInj Refl = Refl

||| SOUNDNESS, parametric in the combiner `h`: every inclusion proof produced by
||| `generateProofWith h` for an in-range leaf reconstructs (under `reconstructWith
||| h`) the tree's true root (`rootHashWith h`), as a propositional equality on
||| the 32-byte digest.
|||
||| The proof uses no property of `h` at all — combination is treated as an
||| opaque black box — which is precisely why it specialises to *every* hash, the
||| XOR placeholder and a cryptographic BLAKE3 combiner alike.
export
merkleCorrectWith : (h : Combiner) -> {n : Nat} -> (t : MerkleTree n) -> (i : Nat)
             -> (leaf : HashBytes) -> (prf : MerkleProof)
             -> getLeafHash t i = Just leaf
             -> generateProofWith h t i = Just prf
             -> reconstructWith h leaf prf = rootHashWith h t
merkleCorrectWith h (Leaf x) Z leaf prf gl gp =
  rewrite justInj (sym gl) in rewrite justInj (sym gp) in Refl
merkleCorrectWith h (Leaf x) (S j) leaf prf gl gp = absurd gl
merkleCorrectWith h {n = S k} (Node l r) i leaf prf gl gp with (i < power 2 k) proof pb
  merkleCorrectWith h {n = S k} (Node l r) i leaf prf gl gp | True with (generateProofWith h l i) proof ps
    merkleCorrectWith h {n = S k} (Node l r) i leaf prf gl gp | True | Just sub =
      let prfIs : (prf = sub ++ [(GoLeft, rootHashWith h r)])
          prfIs = sym (justInj gp)
          ih : (reconstructWith h leaf sub = rootHashWith h l)
          ih = merkleCorrectWith h l i leaf sub gl ps
      in rewrite prfIs in
         rewrite reconstructAppendWith h leaf sub [(GoLeft, rootHashWith h r)] in
         rewrite ih in Refl
    merkleCorrectWith h {n = S k} (Node l r) i leaf prf gl gp | True | Nothing =
      absurd gp
  merkleCorrectWith h {n = S k} (Node l r) i leaf prf gl gp | False with (generateProofWith h r (i `minus` power 2 k)) proof ps
    merkleCorrectWith h {n = S k} (Node l r) i leaf prf gl gp | False | Just sub =
      let prfIs : (prf = sub ++ [(GoRight, rootHashWith h l)])
          prfIs = sym (justInj gp)
          ih : (reconstructWith h leaf sub = rootHashWith h r)
          ih = merkleCorrectWith h r (i `minus` power 2 k) leaf sub gl ps
      in rewrite prfIs in
         rewrite reconstructAppendWith h leaf sub [(GoRight, rootHashWith h l)] in
         rewrite ih in Refl
    merkleCorrectWith h {n = S k} (Node l r) i leaf prf gl gp | False | Nothing =
      absurd gp

||| SOUNDNESS (merkleCorrect) for the XOR placeholder API: the `xorCombiner`
||| instance of `merkleCorrectWith`. Every inclusion proof produced by
||| `generateProof` for an in-range leaf reconstructs the tree's true root.
|||
||| Stated as a propositional equality on the 32-byte digest — the strongest
||| honest form. `verifyProof` then accepts the proof, because it is exactly
||| `root == reconstruct leaf prf` (`verifyProofReconstructs`) and here the
||| reconstruction equals the root. (The residual `root == root` Bool step holds
||| for any lawful `Eq`; discharging it for the primitive `Bits8` equality would
||| need an unsafe primitive-reflexivity axiom, which is why the propositional
||| statement is the right one.)
export
merkleCorrect : {n : Nat} -> (t : MerkleTree n) -> (i : Nat)
             -> (leaf : HashBytes) -> (prf : MerkleProof)
             -> getLeafHash t i = Just leaf
             -> generateProof t i = Just prf
             -> reconstruct leaf prf = rootHashBytes t
merkleCorrect t i leaf prf gl gp = merkleCorrectWith xorCombiner t i leaf prf gl gp
