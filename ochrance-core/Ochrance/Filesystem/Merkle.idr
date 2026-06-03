||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.Merkle - Verified Merkle tree implementation
|||
||| Uses height-indexed types to ensure the tree structure is correct at
||| compile time. The `merkleCorrect` theorem proves inclusion-proof soundness:
||| every proof produced by `generateProof` for an in-range leaf reconstructs
||| the tree's true root (a machine-checked propositional equality).

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

||| Generate a Merkle inclusion proof from a tree (pure XOR version).
||| Given a leaf index (0-based, left-to-right), extracts the path from
||| leaf to root with sibling hashes at each level.
|||
||| Returns Nothing if the index is out of range.
export
generateProof : {n : Nat} -> MerkleTree n -> (leafIdx : Nat) -> Maybe MerkleProof
generateProof {n = Z} (Leaf _) Z = Just []
generateProof {n = Z} (Leaf _) (S _) = Nothing
generateProof {n = S k} (Node l r) idx =
  let halfSize = power 2 k in
  if idx < halfSize
     then do  -- Leaf is in the left subtree
       subProof <- generateProof l idx
       let siblingHash = rootHashBytes r
       Just (subProof ++ [(GoLeft, siblingHash)])
     else do  -- Leaf is in the right subtree
       subProof <- generateProof r (idx `minus` halfSize)
       let siblingHash = rootHashBytes l
       Just (subProof ++ [(GoRight, siblingHash)])

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
export
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

||| The hash an inclusion proof reconstructs: start from a leaf hash and fold in
||| each sibling, left or right, exactly as `verifyProof` does. This is the value
||| `verifyProof` compares against the root (see `verifyProofReconstructs`).
public export
reconstruct : HashBytes -> MerkleProof -> HashBytes
reconstruct acc [] = acc
reconstruct acc ((GoLeft,  sib) :: rest) = reconstruct (hashPairStub acc sib) rest
reconstruct acc ((GoRight, sib) :: rest) = reconstruct (hashPairStub sib acc) rest

||| `reconstruct` distributes over path concatenation: folding `p ++ q` equals
||| folding `p`, then folding `q` from that result.
reconstructAppend : (acc : HashBytes) -> (p, q : MerkleProof)
                 -> reconstruct acc (p ++ q) = reconstruct (reconstruct acc p) q
reconstructAppend acc []                       q = Refl
reconstructAppend acc ((GoLeft,  sib) :: rest) q = reconstructAppend (hashPairStub acc sib) rest q
reconstructAppend acc ((GoRight, sib) :: rest) q = reconstructAppend (hashPairStub sib acc) rest q

||| `verifyProof` is exactly a root-equality test on the reconstructed hash.
||| This bridges the propositional soundness theorem below to the Bool API.
export
verifyProofReconstructs : (root, leaf : HashBytes) -> (prf : MerkleProof)
                       -> verifyProof root leaf prf = (root == reconstruct leaf prf)
verifyProofReconstructs root leaf []                       = Refl
verifyProofReconstructs root leaf ((GoLeft,  sib) :: rest) =
  verifyProofReconstructs root (hashPairStub leaf sib) rest
verifyProofReconstructs root leaf ((GoRight, sib) :: rest) =
  verifyProofReconstructs root (hashPairStub sib leaf) rest

-- Injectivity of `Just`, used to read prf back out of the generated proof.
justInj : {0 a : Type} -> {0 x, y : a} -> Just x = Just y -> x = y
justInj Refl = Refl

||| SOUNDNESS (merkleCorrect): every inclusion proof produced by `generateProof`
||| for an in-range leaf reconstructs the tree's true root.
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
merkleCorrect (Leaf h) Z leaf prf gl gp =
  rewrite justInj (sym gl) in rewrite justInj (sym gp) in Refl
merkleCorrect (Leaf h) (S j) leaf prf gl gp = absurd gl
merkleCorrect {n = S k} (Node l r) i leaf prf gl gp with (i < power 2 k) proof pb
  merkleCorrect {n = S k} (Node l r) i leaf prf gl gp | True with (generateProof l i) proof ps
    merkleCorrect {n = S k} (Node l r) i leaf prf gl gp | True | Just sub =
      let prfIs : (prf = sub ++ [(GoLeft, rootHashBytes r)])
          prfIs = sym (justInj gp)
          ih : (reconstruct leaf sub = rootHashBytes l)
          ih = merkleCorrect l i leaf sub gl ps
      in rewrite prfIs in
         rewrite reconstructAppend leaf sub [(GoLeft, rootHashBytes r)] in
         rewrite ih in Refl
    merkleCorrect {n = S k} (Node l r) i leaf prf gl gp | True | Nothing =
      absurd gp
  merkleCorrect {n = S k} (Node l r) i leaf prf gl gp | False with (generateProof r (i `minus` power 2 k)) proof ps
    merkleCorrect {n = S k} (Node l r) i leaf prf gl gp | False | Just sub =
      let prfIs : (prf = sub ++ [(GoRight, rootHashBytes l)])
          prfIs = sym (justInj gp)
          ih : (reconstruct leaf sub = rootHashBytes r)
          ih = merkleCorrect r (i `minus` power 2 k) leaf sub gl ps
      in rewrite prfIs in
         rewrite reconstructAppend leaf sub [(GoRight, rootHashBytes l)] in
         rewrite ih in Refl
    merkleCorrect {n = S k} (Node l r) i leaf prf gl gp | False | Nothing =
      absurd gp
