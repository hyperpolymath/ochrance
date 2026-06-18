||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.VerifyMerkle - wiring verification to the Merkle proofs.
|||
||| Root-based verification - trusting a single Merkle root instead of checking every
||| block hash - is sound and complete *exactly* because the root is a FAITHFUL
||| fingerprint of the leaf vector: two block-hash vectors build trees with equal
||| roots iff the vectors are equal. The forward (no-collision / binding) direction is
||| `merkleBindingTree` (Stage 1.4, discharged against `CollisionResistant h`); the
||| backward direction is congruence. This is the theorem that licenses a verifier to
||| trust a root, and it carries Stage 1.4's binding guarantee into the verification
||| use-case.
|||
||| Three verification modes are surfaced, generic -> granular:
|||   * `rootFaithful` / `rootVerifySound` - root-equivalence (one root check stands
|||     in for every block hash; backed by `merkleBindingTree`, Stage 1.4);
|||   * `inclusionVerifySound` - inclusion-proof verification (per-leaf `(leaf, proof)`
|||     reconstructs the root; backed by `merkleCorrect`, Stage 1.1);
|||   * `hashToBytes` - the live `Hash` <-> `HashBytes` bridge for snapshot-root
|||     verification, composing with `rootVerifySound`.
|||
||| NOTE (remaining): fully replacing the *live* A2ML-`Hash`-based verifier
||| (`verifyRefsHelper`, Stage 2.2) with a tree build needs the hex conversion
||| correctness (Stage 2.3, bounded by the `unpack`/`pack` + `Bits8` walls) and a
||| power-of-two leaf layout - a verify-path change, documented in docs/PROOFS.adoc,
||| not faked here.
module Ochrance.Filesystem.VerifyMerkle

import Data.Vect

import Ochrance.A2ML.Types
import Ochrance.Util.Hex
import Ochrance.Filesystem.Merkle
import Ochrance.Filesystem.MerkleBinding

%default total

||| The Merkle root is a faithful fingerprint of the block-hash vector: equal roots
||| iff equal leaves. Backward is congruence (same leaves => same root); forward is
||| binding (`merkleBindingTree` against the typed `CollisionResistant h`). Checking
||| one root is therefore *equivalent* to checking every block hash - the
||| justification for root-based verification.
export
rootFaithful : (h : Combiner) -> CollisionResistant h -> {n : Nat} ->
               (xs, ys : Vect (power 2 n) HashBytes) ->
               ( (rootHashWith h (buildMerkleTree {n} xs) = rootHashWith h (buildMerkleTree {n} ys)) -> xs = ys
               , (xs = ys) -> (rootHashWith h (buildMerkleTree {n} xs) = rootHashWith h (buildMerkleTree {n} ys))
               )
rootFaithful h cr xs ys =
  ( merkleBindingTree h cr xs ys
  , \eq => cong (\v => rootHashWith h (buildMerkleTree {n} v)) eq
  )

||| The verifier's security guarantee, as the forward direction in verification terms:
||| if the actual blocks build a tree whose root matches the expected (manifest) root,
||| the actual blocks ARE the expected blocks - no collision can substitute different
||| data under the same committed root. (First projection of `rootFaithful`.)
export
rootVerifySound : (h : Combiner) -> CollisionResistant h -> {n : Nat} ->
                  (actual, expected : Vect (power 2 n) HashBytes) ->
                  rootHashWith h (buildMerkleTree {n} actual)
                    = rootHashWith h (buildMerkleTree {n} expected) ->
                  actual = expected
rootVerifySound h cr actual expected = merkleBindingTree h cr actual expected

--------------------------------------------------------------------------------
-- Mode 3 (granular): inclusion-proof verification
--------------------------------------------------------------------------------

||| GRANULAR (inclusion-proof) verification soundness: for an in-range leaf, the
||| inclusion proof produced by `generateProof` reconstructs the tree's true root.
||| So a verifier presented with `(leaf, proof)` and checking it against the root
||| accepts exactly the genuine data at that position - this is `merkleCorrect`
||| (Stage 1.1) read as a verification guarantee. (The propositional `reconstruct =
||| root` is the wall-free core; the residual `root == root` Bool step in the
||| `verifyProof` API is the documented primitive-`Bits8` boundary.)
export
inclusionVerifySound : {n : Nat} -> (t : MerkleTree n) -> (i : Nat) ->
  (leaf : HashBytes) -> (prf : MerkleProof) ->
  getLeafHash t i = Just leaf -> generateProof t i = Just prf ->
  reconstruct leaf prf = rootHashBytes t
inclusionVerifySound t i leaf prf gl gp = merkleCorrect t i leaf prf gl gp

--------------------------------------------------------------------------------
-- Mode 2 (live bridge): A2ML Hash <-> Merkle HashBytes
--------------------------------------------------------------------------------

||| The bridge between the `Hash`-typed manifest/snapshot world (hex string + algo)
||| and the `HashBytes`-typed Merkle proofs: decode a hash's 32 raw bytes. Partial -
||| `Nothing` on malformed/short hex. Snapshot-root verification composes this with
||| `rootVerifySound`: convert the leaves and the committed `FSSnapshot.rootHash`,
||| build the tree, compare roots, and binding gives "same root ⇒ same blocks". The
||| conversion's *correctness* is the hex boundary (Stage 2.3, `parsePairsRoundtrip`
||| modulo the per-byte `Bits8` + `unpack`/`pack` walls), so it is surfaced as this
||| explicit decode rather than asserted.
public export
hashToBytes : Hash -> Maybe HashBytes
hashToBytes h = hexStringToVect 32 h.value
