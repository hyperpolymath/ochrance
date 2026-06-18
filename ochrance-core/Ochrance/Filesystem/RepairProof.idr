||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.RepairProof - Stage 3: pure repair correctness.
|||
||| FSState's verifiable content is its hash map (`BlockIndex -> Maybe Hash`); there
||| is no separate block data. So `repairBlockPure` - which installs a hash at one
||| index - is the genuine repair semantics (not a no-op against which a theorem
||| would be vacuous), and these lemmas pin it down:
|||
|||   * `repairBlockSets`       - the repaired index now holds exactly the new hash;
|||   * `repairBlockPreserves`  - every other index is untouched;
|||   * `repairBlockNumBlocks`  - the block count is unchanged;
|||   * `repairBlockIdempotent` - repairing the same (index, hash) twice equals once.
|||
||| All axiom-free: the index test is `Nat` `==`, whose reflexivity is *structural*
||| (`eqNatReflTrue`) - unlike the primitive `Hash`/`String` `==`. No `believe_me`,
||| no `postulate`.
module Ochrance.Filesystem.RepairProof

import Decidable.Equality

import Ochrance.A2ML.Types
import Ochrance.Filesystem.Types
import Ochrance.Filesystem.Repair

%default total

||| `Nat` equality is reflexively `True` - provable because `Eq Nat` is structural
||| (the wall-free counterpart of the primitive `String`/`Bits8` reflexivity that
||| the `Hash` comparison would need).
public export
eqNatReflTrue : (n : Nat) -> (n == n) = True
eqNatReflTrue Z     = Refl
eqNatReflTrue (S k) = eqNatReflTrue k

||| Repair installs the requested hash at the repaired index.
export
repairBlockSets : (s : FSState) -> (i : BlockIndex) -> (h : Hash) ->
                  blockHash (repairBlockPure s i h) i = Just h
repairBlockSets s i h = rewrite eqNatReflTrue i in Refl

||| Repair leaves every other index untouched.
export
repairBlockPreserves : (s : FSState) -> (i : BlockIndex) -> (h : Hash) ->
                       (j : BlockIndex) -> (j == i) = False ->
                       blockHash (repairBlockPure s i h) j = blockHash s j
repairBlockPreserves s i h j neq = rewrite neq in Refl

||| Repair preserves the block count.
export
repairBlockNumBlocks : (s : FSState) -> (i : BlockIndex) -> (h : Hash) ->
                       numBlocks (repairBlockPure s i h) = numBlocks s
repairBlockNumBlocks s i h = Refl

||| `Nat` inequality reflects as `== = False` (structural counterpart of the
||| primitive-equality wall). Used to drive `repairBlockPreserves` on the `j /= i`
||| branch of idempotence.
neqNatFalse : (m, n : Nat) -> Not (m = n) -> (m == n) = False
neqNatFalse Z     Z     ctra = absurd (ctra Refl)
neqNatFalse Z     (S _) _    = Refl
neqNatFalse (S _) Z     _    = Refl
neqNatFalse (S k) (S l) ctra = neqNatFalse k l (\p => ctra (cong S p))

||| Repair idempotence: repairing the same index with the same hash twice agrees
||| with doing it once, at every index (extensional equality of the hash maps).
||| Decided on `j` vs `i`: at `i`, both sides read `Just h` (`repairBlockSets`);
||| elsewhere, both read the original (`repairBlockPreserves`). Uses `decEq` rather
||| than a `with` on `j == i` (whose reduct `equalNat` collapses the two `if`s
||| asymmetrically).
export
repairBlockIdempotent : (s : FSState) -> (i : BlockIndex) -> (h : Hash) ->
  (j : BlockIndex) ->
  blockHash (repairBlockPure (repairBlockPure s i h) i h) j
    = blockHash (repairBlockPure s i h) j
repairBlockIdempotent s i h j = case decEq j i of
  Yes prf  => rewrite prf in
              trans (repairBlockSets (repairBlockPure s i h) i h)
                    (sym (repairBlockSets s i h))
  No  ctra => repairBlockPreserves (repairBlockPure s i h) i h j (neqNatFalse j i ctra)
