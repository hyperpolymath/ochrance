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
||| Verification modes are surfaced, generic -> granular:
|||   * `rootFaithful` / `rootVerifySound` - root-equivalence (one root check stands
|||     in for every block hash; backed by `merkleBindingTree`, Stage 1.4);
|||   * `inclusionVerifySound` - inclusion-proof verification (per-leaf `(leaf, proof)`
|||     reconstructs the root; backed by `merkleCorrect`, Stage 1.1);
|||   * `hashToBytes` - the live `Hash` <-> `HashBytes` bridge;
|||   * `merkleRootVerifyHashSound` - LIVE root-verification soundness at the A2ML
|||     `Hash` level: two block-hash vectors that decode and yield equal roots are
|||     equal. This is the theorem a redesigned root-comparing `verifyRefsHelper` rests
|||     on; it carries `rootVerifySound` up across the decoder bridge.
|||
||| NOTE (remaining): the *soundness* of replacing the live `Hash`-based verifier with
||| a tree build is now proven (`merkleRootVerifyHashSound`), modulo two named
||| boundaries - `CollisionResistant h` (1.4) and `DecodeInjective dec` (the hex wall,
||| 2.3). What is left is purely *plumbing*: padding an arbitrary-length block list to
||| a power-of-two leaf `Vect` and swapping the verify path over - engineering, not a
||| proof, and tracked in docs/PROOFS.adoc.
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

--------------------------------------------------------------------------------
-- Mode 2 (live): root-verification soundness at the A2ML `Hash` level
--------------------------------------------------------------------------------
--
-- These are stated for an arbitrary decoder `dec : Hash -> Maybe HashBytes` (exactly
-- as `rootVerifySound` is stated for an arbitrary `h : Combiner`); the intended
-- instance is `dec := hashToBytes`, supplied at the call site by a redesigned
-- verifier. Keeping `dec` an explicit parameter also avoids it being auto-bound as
-- an unbound implicit in the signatures.

||| Decode every hash in a vector (structural, so it reduces definitionally - unlike
||| the `Functor` `map`, which the per-element proofs below need to compute through).
public export
decAll : (Hash -> Maybe HashBytes) -> Vect k Hash -> Vect k (Maybe HashBytes)
decAll dec []        = []
decAll dec (x :: xs) = dec x :: decAll dec xs

||| A decoder is INJECTIVE - distinct hashes give distinct bytes. True for well-formed
||| hashes, but bottoms out in the primitive hex/`String` wall (Stage 2.3), so it is
||| taken as a named hypothesis: the live-verification counterpart of
||| `CollisionResistant` (named, never faked).
public export
DecodeInjective : (Hash -> Maybe HashBytes) -> Type
DecodeInjective dec = (a, b : Hash) -> dec a = dec b -> a = b

||| Lift decode-injectivity over a vector: two `Hash` vectors whose decodings agree
||| are equal.
export
mapDecodeInjective : (dec : Hash -> Maybe HashBytes) -> DecodeInjective dec ->
  {k : Nat} -> (xs, ys : Vect k Hash) -> decAll dec xs = decAll dec ys -> xs = ys
mapDecodeInjective dec inj []        []        _  = Refl
mapDecodeInjective dec inj (x :: xs) (y :: ys) eq =
  rewrite inj x y (cong Data.Vect.head eq) in
  cong (\zs => y :: zs) (mapDecodeInjective dec inj xs ys (cong Data.Vect.tail eq))

||| LIVE root-verification soundness (Mode 2, full): two block-hash vectors that
||| decode successfully and yield equal Merkle roots are EQUAL - carrying
||| `rootVerifySound` (HashBytes level) up to the live A2ML `Hash` level across the
||| decoder bridge (instantiate `dec := hashToBytes`). This is what a redesigned
||| `verifyRefsHelper` (one root comparison instead of per-ref hash checks) would rest
||| on. Honest hypotheses: `CollisionResistant h` (Stage 1.4) and `DecodeInjective dec`
||| (the hex boundary, Stage 2.3). The `decAll dec xs = map Just xb` premises say "xs
||| decodes to the leaf bytes xb"; the power-of-two layout is the leaf length index.
export
merkleRootVerifyHashSound :
  (h : Combiner) -> CollisionResistant h ->
  (dec : Hash -> Maybe HashBytes) -> DecodeInjective dec -> {n : Nat} ->
  (xs, ys : Vect (power 2 n) Hash) -> (xb, yb : Vect (power 2 n) HashBytes) ->
  decAll dec xs = map Just xb -> decAll dec ys = map Just yb ->
  rootHashWith h (buildMerkleTree {n} xb) = rootHashWith h (buildMerkleTree {n} yb) ->
  xs = ys
merkleRootVerifyHashSound h cr dec inj xs ys xb yb dx dy roots =
  let bytesEq : (xb = yb)
      bytesEq = rootVerifySound h cr xb yb roots
      decsEq  : (decAll dec xs = decAll dec ys)
      decsEq  = trans dx (trans (cong (map Just) bytesEq) (sym dy))
  in mapDecodeInjective dec inj xs ys decsEq
