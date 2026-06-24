||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.MerkleAssumption - Stage 4 (honest): the collision-resistance
||| assumption, isolated.
|||
||| The binding theorem `merkleBinding` (Stage 1.4) is discharged against the
||| hypothesis `CollisionResistant h` - full pairwise injectivity of the combiner. The
||| roadmap once aimed to *discharge* that hypothesis for the concrete cryptographic
||| combiner in "Stage 4". That is IMPOSSIBLE to do honestly, and this module records
||| why rather than faking it:
|||
|||   * A combiner has type `HashBytes -> HashBytes -> HashBytes` - it maps 64 bytes to
|||     32 (it compresses). By the pigeonhole principle two distinct input pairs must
|||     share an output - a collision - so `CollisionResistant h` is FALSE for every
|||     total combiner. No term of that type exists; constructing one would need
|||     `believe_me`/`postulate`, which this codebase forbids.
|||   * Real collision resistance is a *computational* statement ("no efficient
|||     adversary finds a collision"), not expressible as a pure type-theoretic
|||     proposition. So `CollisionResistant h` is the irreducible cryptographic trust
|||     root: it stays an explicit hypothesis, threaded through every binding-based
|||     result and supplied by the caller at the verification boundary.
|||
||| What we CAN prove - and do here - is that the hypothesis is not vacuous: it has
||| teeth. `constNotCollisionResistant` exhibits a concrete (degenerate) combiner that
||| provably FAILS collision resistance, so the binding theorem genuinely depends on
||| being handed a good combiner. This is the honest content of "Stage 4".
module Ochrance.Filesystem.MerkleAssumption

import Data.Vect

import Ochrance.Filesystem.Merkle
import Ochrance.Filesystem.MerkleBinding

%default total

||| Two distinguished digests, written as explicit conses so that `head` reduces to a
||| literal `Bits8` (a bare `replicate 32 _` leaves `head` stuck).
public export
zerosH : HashBytes
zerosH = 0 :: replicate 31 0

public export
onesH : HashBytes
onesH = 1 :: replicate 31 0

||| `True = False` is uninhabited (distinct constructors).
trueNotFalse : Not (True = False)
trueNotFalse Refl impossible

||| The two digests differ: their first bytes are `0` and `1`. Taking heads turns the
||| assumed `zerosH = onesH` into `(0 : Bits8) = 1`, and `== 0` turns that into the
||| impossible `True = False`.
zerosNotOnes : Not (MerkleAssumption.zerosH = MerkleAssumption.onesH)
zerosNotOnes prf = trueNotFalse (cong (\b => b == 0) (cong Data.Vect.head prf))

||| A degenerate combiner that ignores its inputs (always returns `emptyHash`).
public export
constCombiner : Combiner
constCombiner _ _ = emptyHash

||| THE ASSUMPTION HAS TEETH: a combiner that ignores its inputs provably is NOT
||| collision-resistant - the distinct inputs `zerosH` / `onesH` collide under it. So
||| `merkleBinding`'s hypothesis is a genuine constraint, not vacuously satisfiable; a
||| real (compressing) combiner can only ever *assume* it, never prove it (see the
||| module header). This is the honest content of "Stage 4": isolate the assumption,
||| and show it bites.
export
constNotCollisionResistant : Not (CollisionResistant MerkleAssumption.constCombiner)
constNotCollisionResistant cr =
  zerosNotOnes (fst (cr zerosH zerosH onesH zerosH Refl))
