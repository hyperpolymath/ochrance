||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.RepairVerify - Stage 3.2: whole-manifest repair => verify.
|||
||| The end-to-end guarantee that ties Stage 2.2 (verifier soundness) to Stage 3.1
||| (pure repair): after repairing every ref of a manifest, the verifier ACCEPTS
||| that manifest - `verifyRefsHelper (repairRefsPure s refs) refs = Right ()`.
|||
||| `repairRefsPure` folds the 3.1 single-block primitive `repairBlockPure` over the
||| refs, installing each ref's hash at its parsed index. The proof factors as:
|||
|||   * `verifyRefsComplete` (Lemma A) - COMPLETENESS, the exact converse of 2.2's
|||     `verifyRefsSound`: if every ref matches the state (`All (RefMatches fs)`), the
|||     verifier accepts. Drives the four guards forward from the witness equalities.
|||   * `repairRefsConsistent` (Lemma B) - repair ESTABLISHES that match witness for
|||     every ref, via `repairBlockSets` (the head's index now holds its hash) and
|||     `repairRefsPurePreserves` (no later repair clobbers it - this is where
|||     distinctness is consumed).
|||
||| Two boundaries are named explicitly, never faked (the house style of
||| `CollisionResistant` / `HexByteRoundtrip`):
|||   * `hashRefl : (h : Hash) -> (h == h) = True` - reflexivity of the PRIMITIVE
|||     `Hash`/`String` `==` (the same wall as `merkleCorrect`'s residual `root==root`
|||     step; `Nat` reflexivity, by contrast, is structural - `eqNatReflTrue`);
|||   * `GoodRefs` - the precondition that ref names parse to DISTINCT in-range
|||     indices. Without distinctness a later repair would overwrite an earlier ref's
|||     block, so the theorem would be false; it is a genuine hypothesis, not a dodge.
|||
||| No `believe_me`, no `assert_total`, no `postulate`.
module Ochrance.Filesystem.RepairVerify

import Data.List.Quantifiers

import Ochrance.A2ML.Types
import Ochrance.Framework.Error
import Ochrance.Filesystem.Types
import Ochrance.Filesystem.Verify
import Ochrance.Filesystem.VerifyProof
import Ochrance.Filesystem.Repair
import Ochrance.Filesystem.RepairProof

%default total

--------------------------------------------------------------------------------
-- Pure whole-manifest repair
--------------------------------------------------------------------------------

||| Repair every ref of a manifest: install each ref's hash at its parsed block
||| index (refs whose name does not parse are skipped - `GoodRefs` rules that out).
||| Folds the Stage 3.1 primitive `repairBlockPure`.
public export
repairRefsPure : FSState -> List Ref -> FSState
repairRefsPure s [] = s
repairRefsPure s (ref :: refs) =
  case parseBlockIdx ref.name of
    Nothing  => repairRefsPure s refs
    Just idx => repairRefsPure (repairBlockPure s idx ref.hash) refs

--------------------------------------------------------------------------------
-- Precondition: distinct, in-range, parseable ref names
--------------------------------------------------------------------------------

||| "This ref's name does NOT parse to index `i`." The atom of distinctness.
public export
NotParsedAs : BlockIndex -> Ref -> Type
NotParsedAs i ref = Not (parseBlockIdx ref.name = Just i)

||| Every ref name parses to a distinct, in-range block index. Indexed by the block
||| count (a `Nat`) rather than the whole state, since repair preserves the count -
||| so the same `GoodRefs` serves the original and every repaired state.
public export
data GoodRefs : Nat -> List Ref -> Type where
  GoodNil  : GoodRefs nb []
  GoodCons : {0 ref : Ref} -> {0 refs : List Ref} ->
             (idx : BlockIndex) ->
             parseBlockIdx ref.name = Just idx ->
             (idx >= nb) = False ->
             All (NotParsedAs idx) refs ->
             GoodRefs nb refs ->
             GoodRefs nb (ref :: refs)

--------------------------------------------------------------------------------
-- Lemma A: completeness (converse of Stage 2.2)
--------------------------------------------------------------------------------

||| COMPLETENESS: if every ref matches the filesystem state, the verifier accepts.
||| The exact converse of `verifyRefsSound` - drives the four guards forward from
||| each ref's `RefMatches` witness (name-parse, range, block-present, hash-equal).
export
verifyRefsComplete : (fs : FSState) -> (refs : List Ref) ->
  All (RefMatches fs) refs -> verifyRefsHelper fs refs = Right ()
verifyRefsComplete fs []          []                                       = Refl
verifyRefsComplete fs (ref :: rs) ((idx ** (pi, rng, (h ** (bh, heq)))) :: rest) =
  rewrite pi  in
  rewrite rng in
  rewrite bh  in
  rewrite heq in
  verifyRefsComplete fs rs rest

--------------------------------------------------------------------------------
-- Repair structural lemmas
--------------------------------------------------------------------------------

||| Whole-manifest repair preserves the block count (folds 3.1's
||| `repairBlockNumBlocks`).
export
repairRefsPureNumBlocks : (s : FSState) -> (refs : List Ref) ->
  numBlocks (repairRefsPure s refs) = numBlocks s
repairRefsPureNumBlocks s []          = Refl
repairRefsPureNumBlocks s (ref :: rs) with (parseBlockIdx ref.name)
  repairRefsPureNumBlocks s (ref :: rs) | Nothing  = repairRefsPureNumBlocks s rs
  repairRefsPureNumBlocks s (ref :: rs) | Just idx =
    repairRefsPureNumBlocks (repairBlockPure s idx ref.hash) rs

||| Repairing a list whose parsed indices all differ from `i` leaves block `i`
||| untouched - the no-clobber lemma. Folds 3.1's `repairBlockPreserves`; the per-
||| step inequality comes from `NotParsedAs` via `neqNatFalse`.
export
repairRefsPurePreserves : (s : FSState) -> (refs : List Ref) -> (i : BlockIndex) ->
  All (NotParsedAs i) refs ->
  blockHash (repairRefsPure s refs) i = blockHash s i
repairRefsPurePreserves s []          i _                    = Refl
repairRefsPurePreserves s (ref :: rs) i (notHere :: notRest) with (parseBlockIdx ref.name)
  repairRefsPurePreserves s (ref :: rs) i (notHere :: notRest) | Nothing  =
    repairRefsPurePreserves s rs i notRest
  repairRefsPurePreserves s (ref :: rs) i (notHere :: notRest) | Just j =
    -- `with` has refined `notHere : Not (Just j = Just i)`
    let iNotJ : Not (i = j)
        iNotJ = \p => notHere (cong Just (sym p))
    in trans (repairRefsPurePreserves (repairBlockPure s j ref.hash) rs i notRest)
             (repairBlockPreserves s j ref.hash i (neqNatFalse i j iNotJ))

||| Unfold one step of `repairRefsPure` once the head ref's index is known.
export
repairRefsUnfold : (s : FSState) -> (ref : Ref) -> (refs : List Ref) ->
  (idx : BlockIndex) -> parseBlockIdx ref.name = Just idx ->
  repairRefsPure s (ref :: refs) = repairRefsPure (repairBlockPure s idx ref.hash) refs
repairRefsUnfold s ref refs idx pi = rewrite pi in Refl

--------------------------------------------------------------------------------
-- Lemma B: repair establishes the match witness
--------------------------------------------------------------------------------

||| Repair ESTABLISHES consistency: after `repairRefsPure`, every ref matches the
||| repaired state. The head's hash is installed (`repairBlockSets`) and protected
||| from later repairs (`repairRefsPurePreserves`, fed the head's distinctness); the
||| tail is the induction hypothesis (the repaired state's count equals the
||| original's, definitionally, so the same `GoodRefs` carries through). The hash
||| equality of each ref against itself is the named `hashRefl` boundary.
export
repairRefsConsistent : (s : FSState) -> (refs : List Ref) ->
  GoodRefs (numBlocks s) refs ->
  (hashRefl : (h : Hash) -> (h == h) = True) ->
  All (RefMatches (repairRefsPure s refs)) refs
repairRefsConsistent s []          GoodNil                          hashRefl = []
repairRefsConsistent s (ref :: rs) (GoodCons idx pi rng notI good') hashRefl =
  let s'     : FSState
      s'     = repairBlockPure s idx ref.hash
      bhF    : blockHash (repairRefsPure s' rs) idx = Just ref.hash
      bhF    = trans (repairRefsPurePreserves s' rs idx notI)
                     (repairBlockSets s idx ref.hash)
      rngF   : (idx >= numBlocks (repairRefsPure s' rs)) = False
      rngF   = rewrite repairRefsPureNumBlocks s' rs in rng
      headW  : RefMatches (repairRefsPure s' rs) ref
      headW  = (idx ** (pi, rngF, (ref.hash ** (bhF, hashRefl ref.hash))))
      tailW  : All (RefMatches (repairRefsPure s' rs)) rs
      tailW  = repairRefsConsistent s' rs good' hashRefl
  in rewrite repairRefsUnfold s ref rs idx pi in (headW :: tailW)

--------------------------------------------------------------------------------
-- Stage 3.2: whole-manifest repair => verify
--------------------------------------------------------------------------------

||| STAGE 3.2: repairing a manifest's refs makes the verifier accept it. Composes
||| Lemma B (repair => match witnesses) with Lemma A (witnesses => acceptance).
||| Honest hypotheses: `GoodRefs` (distinct in-range parseable names) and `hashRefl`
||| (primitive `Hash` `==` reflexivity).
export
repairThenVerify : (s : FSState) -> (refs : List Ref) ->
  GoodRefs (numBlocks s) refs ->
  (hashRefl : (h : Hash) -> (h == h) = True) ->
  verifyRefsHelper (repairRefsPure s refs) refs = Right ()
repairThenVerify s refs good hashRefl =
  verifyRefsComplete (repairRefsPure s refs) refs
    (repairRefsConsistent s refs good hashRefl)
