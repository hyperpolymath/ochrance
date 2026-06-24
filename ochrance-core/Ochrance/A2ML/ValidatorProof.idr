||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.A2ML.ValidatorProof - Stage 2.1: soundness of validateManifest.
|||
||| A `ValidManifest` is meant to be a *type-level witness* of validity. This
||| module proves it actually is one: every manifest `validateManifest` accepts
||| satisfies the three structural invariants the validator checks - supported
||| version, non-empty subsystem, and well-formed-hex ref hashes.
|||
||| The invariants are stated at the *decision (Bool) level* - `isVersionSupported
||| v = True`, `(sub == "") = False`, `isValidHexString h = True` - rather than
||| inverted into propositional `String` facts (`v = "0.1.0"`, `Not (sub = "")`):
||| `String` equality is a primitive with no equational theory, so the Bool form
||| is the strongest *honest* statement (cf. the documented String/primitive wall
||| in docs/PROOFS.adoc). No `believe_me`, no `postulate`.
module Ochrance.A2ML.ValidatorProof

import Data.List
import Data.List.Quantifiers

import Ochrance.A2ML.Types
import Ochrance.A2ML.Validator
import Ochrance.Framework.Error

%default total

||| The per-ref invariant a ValidManifest carries: the ref's hash value passed the
||| well-formed-hex check. Wall-free Bool form.
public export
RefValid : Ref -> Type
RefValid ref = isValidHexString ref.hash.value = True

||| Soundness of the `traverse_ validateRef` phase: if validating every ref
||| succeeded, every ref satisfies `RefValid`. Inverts the Either-applicative fold
||| one element at a time (a `Left` anywhere would have short-circuited the whole).
||| From a successful single-ref validation, recover the wall-free hex invariant.
validRefSound : (ref : Ref) -> validateRef ref = Right () ->
                isValidHexString ref.hash.value = True
validRefSound ref vr with (isValidHexString ref.hash.value)
  validRefSound ref vr | True  = Refl
  validRefSound ref vr | False = absurd vr

||| Soundness of the `traverse_ validateRef` phase: if validating every ref
||| succeeded, every ref satisfies `RefValid`. `traverse_` for `Either` desugars
||| through `<*>` (`Right () *> y` is `map id y`, equal to `y` only by the functor
||| identity *law*, not definitionally), so we case on the head outcome and the
||| tail fold directly: every remaining step (`map g (Left e) = Left e`,
||| `Left e <*> y = Left e`, `Right f <*> Right x = Right (f x)`) is definitional.
traverseRefsSound : (refs : List Ref) ->
                    the (Either ValidationError ()) (traverse_ Validator.validateRef refs) = Right () ->
                    All RefValid refs
traverseRefsSound []            _   = []
traverseRefsSound (ref :: rest) prf with (validateRef ref) proof vrEq
  traverseRefsSound (ref :: rest) prf | Left  e  = absurd prf
  traverseRefsSound (ref :: rest) prf | Right () with (traverse_ validateRef rest) proof trEq
    traverseRefsSound (ref :: rest) prf | Right () | Left  e  = absurd prf
    traverseRefsSound (ref :: rest) prf | Right () | Right () =
      validRefSound ref vrEq :: traverseRefsSound rest trEq

||| SOUNDNESS (Stage 2.1): a manifest accepted by `validateManifest` satisfies the
||| three structural invariants the validator enforces - supported version,
||| non-empty subsystem, and well-formed ref hashes. `ValidManifest` is therefore a
||| genuine validity witness, not a mere wrapper.
|||
||| Proved by inverting the Either-monad validation pipeline: each guard that could
||| have produced a `Left` is shown to have taken its `Right` branch, since the
||| whole returned `Right`.
export
validateManifestSound : (m : Manifest) -> (vm : ValidManifest) ->
  validateManifest m = Right vm ->
  ( isVersionSupported m.manifestData.version = True
  , (m.manifestData.subsystem == "") = False
  , All RefValid m.refs )
validateManifestSound m vm prf with (isVersionSupported m.manifestData.version)
  validateManifestSound m vm prf | False = absurd prf
  validateManifestSound m vm prf | True with (m.manifestData.subsystem == "")
    validateManifestSound m vm prf | True | True  = absurd prf
    validateManifestSound m vm prf | True | False with (traverse_ validateRef m.refs) proof trEq
      validateManifestSound m vm prf | True | False | Left  e  = absurd prf
      validateManifestSound m vm prf | True | False | Right () =
        (Refl, Refl, traverseRefsSound m.refs trEq)
