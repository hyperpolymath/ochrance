||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.VerifyProof - Stage 2.2: soundness of ref-checking.
|||
||| The pure verifier `verifyRefsHelper` (the core of the FSState
||| `VerifiedSubsystem.verify`) accepts a manifest's refs only if every ref names an
||| in-range block whose pre-computed hash equals the ref's hash. This module proves
||| the direction trust depends on: *acceptance is sound* - if `verifyRefsHelper`
||| returns `Right ()`, then every ref genuinely matches the filesystem state
||| (`All (RefMatches fs)`). Proved by inverting the verifier's four guards
||| (name-parse, range, block-present, hash-equal) per ref - a failure in any one
||| would have short-circuited to `Left`.
|||
||| Equality facts are stated at the wall-free Bool level (`h == ref.hash = True`),
||| since `Hash`/`String` equality bottoms out in primitives. No `believe_me`,
||| no `postulate`.
|||
||| SCOPE: this is soundness of the *current* hash-comparison verifier. Wiring it to
||| `merkleCorrect` (checking against a Merkle root rather than per-ref hashes) is a
||| separate architectural step - the verify path does not build a tree today.
module Ochrance.Filesystem.VerifyProof

import Data.List
import Data.List.Quantifiers

import Ochrance.A2ML.Types
import Ochrance.Framework.Error
import Ochrance.Filesystem.Types
import Ochrance.Filesystem.Verify

%default total

||| What it means for one ref to match the filesystem: its name parses to a block
||| index, in range, whose stored hash equals the ref's hash (Bool form).
public export
RefMatches : FSState -> Ref -> Type
RefMatches fs ref =
  ( idx : BlockIndex
  ** ( parseBlockIdx ref.name = Just idx
     , (idx >= fs.numBlocks) = False
     , ( h : Hash ** (fs.blockHash idx = Just h, (h == ref.hash) = True) )
     ) )

||| SOUNDNESS (Stage 2.2): if the pure verifier accepts all refs, every ref matches
||| the filesystem state. Inverts the four guards per ref (each guard's failure path
||| is a `Left`, contradicting the `Right ()` hypothesis).
export
verifyRefsSound : (fs : FSState) -> (refs : List Ref) ->
                  verifyRefsHelper fs refs = Right () -> All (RefMatches fs) refs
verifyRefsSound fs []            _   = []
verifyRefsSound fs (ref :: refs) prf with (parseBlockIdx ref.name) proof pbi
  verifyRefsSound fs (ref :: refs) prf | Nothing  = absurd prf
  verifyRefsSound fs (ref :: refs) prf | Just idx with (idx >= fs.numBlocks) proof geq
    verifyRefsSound fs (ref :: refs) prf | Just idx | True  = absurd prf
    verifyRefsSound fs (ref :: refs) prf | Just idx | False with (fs.blockHash idx) proof bh
      verifyRefsSound fs (ref :: refs) prf | Just idx | False | Nothing  = absurd prf
      verifyRefsSound fs (ref :: refs) prf | Just idx | False | Just h with (h == ref.hash) proof heq
        verifyRefsSound fs (ref :: refs) prf | Just idx | False | Just h | False = absurd prf
        verifyRefsSound fs (ref :: refs) prf | Just idx | False | Just h | True  =
          (idx ** (pbi, geq, (h ** (bh, heq)))) :: verifyRefsSound fs refs prf
