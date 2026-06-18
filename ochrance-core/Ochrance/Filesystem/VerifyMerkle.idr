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
||| NOTE (remaining bridge): this connects the proven Merkle layer (`HashBytes`
||| leaves) to the verification use-case. Connecting it the rest of the way to the
||| *live* A2ML-`Hash`-based verifier (`verifyRefsHelper`, Stage 2.2) additionally
||| needs the hex `Hash` <-> `HashBytes` conversion (Stage 2.3, bounded by the
||| `unpack`/`pack` + `Bits8` walls) and a power-of-two leaf layout - a verify-path
||| change, documented in docs/PROOFS.adoc, not faked here.
module Ochrance.Filesystem.VerifyMerkle

import Data.Vect

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
