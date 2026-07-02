-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.A2ML.ValidatorProof - Stage 2.1: soundness of validateManifest.
|||
||| A `ValidManifest` is meant to be a *type-level witness* of validity. This
||| module proves it actually is one: every manifest `validateManifest` accepts
||| satisfies the four invariants the validator checks - supported version,
||| non-empty subsystem, well-formed-hex ref hashes, and the clock-free policy
||| constraints (require_sig, max_age shape).
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
import Data.Maybe

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

--------------------------------------------------------------------------------
-- Hex digest check: strengthened decision soundness
--------------------------------------------------------------------------------

||| Split a proof that a Boolean conjunction is True into proofs of both
||| conjuncts.
andSound : (a, b : Bool) -> (a && b) = True -> (a = True, b = True)
andSound True  True  Refl = (Refl, Refl)
andSound True  False prf  = absurd prf
andSound False _     prf  = absurd prf

||| The strengthened hex check is sound: acceptance forces BOTH the exact
||| digest length (64 hex chars = 32 bytes) AND hex-digit-only content
||| (Bool level, cf. the String wall note above). In particular the empty
||| string and '.'-padded values the old check accepted are now impossible
||| inside a ValidManifest.
export
hexAcceptSound : (s : String) -> isValidHexString s = True ->
  ( (length (unpack s) == Validator.digestHexLength) = True
  , Prelude.all Prelude.Types.isHexDigit (unpack s) = True )
hexAcceptSound s prf = andSound _ _ prf

||| Rejection: a string of the wrong length never passes the hex check,
||| regardless of its content.
export
hexWrongLengthRejected : (s : String) ->
  (length (unpack s) == Validator.digestHexLength) = False ->
  isValidHexString s = False
hexWrongLengthRejected s wrongLen with (length (unpack s) == Validator.digestHexLength)
  hexWrongLengthRejected s wrongLen | False = Refl
  hexWrongLengthRejected s wrongLen | True  = absurd wrongLen

||| Known-answer check: the empty string is rejected (the old check
||| accepted it).
export
hexEmptyRejected : isValidHexString "" = False
hexEmptyRejected = Refl

--------------------------------------------------------------------------------
-- Timestamp parser: known-answer checks
--------------------------------------------------------------------------------

||| Known-answer check: the Unix epoch itself parses to 0 seconds.
export
parseTimestampEpochKAT : parseTimestamp "1970-01-01T00:00:00Z" = Just 0
parseTimestampEpochKAT = Refl

||| Known-answer check: a representative modern date (cross-checked against
||| an independent epoch converter), across a leap year (2024) and a
||| century non-leap rule neighbourhood.
export
parseTimestampModernKAT : parseTimestamp "2026-02-07T00:00:00Z" = Just 1770422400
parseTimestampModernKAT = Refl

||| Known-answer check: malformed timestamps are rejected.
export
parseTimestampGarbageKAT : parseTimestamp "not-a-timestamp" = Nothing
parseTimestampGarbageKAT = Refl

||| Known-answer check: out-of-range fields are rejected (month 13).
export
parseTimestampBadMonthKAT : parseTimestamp "2026-13-01T00:00:00Z" = Nothing
parseTimestampBadMonthKAT = Refl

--------------------------------------------------------------------------------
-- Freshness (max_age) enforcement
--------------------------------------------------------------------------------

||| SECURITY (max_age): a manifest older than max_age seconds is REJECTED
||| by the freshness check.
export
checkFreshnessRejectsStale : (now, issued : Integer) -> (maxAge : Nat) ->
  ((now - issued) > cast maxAge) = True ->
  Not (checkFreshness now issued maxAge = Right ())
checkFreshnessRejectsStale now issued maxAge stale prf with ((now - issued) > cast maxAge)
  checkFreshnessRejectsStale now issued maxAge stale prf | True  = absurd prf
  checkFreshnessRejectsStale now issued maxAge stale prf | False = absurd stale

||| Completeness partner: a manifest within max_age passes the freshness check.
export
checkFreshnessAcceptsFresh : (now, issued : Integer) -> (maxAge : Nat) ->
  ((now - issued) > cast maxAge) = False ->
  checkFreshness now issued maxAge = Right ()
checkFreshnessAcceptsFresh now issued maxAge fresh with ((now - issued) > cast maxAge)
  checkFreshnessAcceptsFresh now issued maxAge fresh | False = Refl
  checkFreshnessAcceptsFresh now issued maxAge fresh | True  = absurd fresh

||| SECURITY (max_age, end to end): `validatePolicyAt` rejects a stale
||| manifest. Stated on the destructured manifest so the policy pipeline
||| reduces definitionally; the timestamp parse and the staleness of the
||| clock reading are supplied as hypotheses.
export
staleManifestRejected :
  (now : Integer) -> (v, sub, ts : String) -> (refs : List Ref) ->
  (att : Maybe Attestation) -> (mode : VerificationMode) -> (maxAge : Nat) ->
  (issued : Integer) ->
  parseTimestamp ts = Just issued ->
  ((now - issued) > cast maxAge) = True ->
  Not (validatePolicyAt now
        (MkManifest (MkManifestData v sub (Just ts)) refs att
                    (Just (MkPolicy mode (Just maxAge) False)))
       = Right ())
staleManifestRejected now v sub ts refs Nothing mode maxAge issued parseEq stale =
  -- Rewriting the goal along parseEq turns both stuck `parseTimestamp ts`
  -- case scrutinees (in checkMaxAgeShape and validatePolicyAt) into
  -- `Just issued`, after which the whole policy pipeline reduces to
  -- `checkFreshness now issued maxAge`.
  rewrite parseEq in checkFreshnessRejectsStale now issued maxAge stale
staleManifestRejected now v sub ts refs (Just a) mode maxAge issued parseEq stale =
  rewrite parseEq in checkFreshnessRejectsStale now issued maxAge stale

--------------------------------------------------------------------------------
-- Policy enforcement in the pure validator
--------------------------------------------------------------------------------

||| SECURITY (require_sig): a manifest whose policy sets require_sig but
||| which carries NO attestation is REJECTED by the pure validator - the
||| policy gate is wired into `validateManifest`, not dead code. Stated on
||| the destructured manifest so `validatePolicy` reduces definitionally.
export
requireSigNoAttestationRejected :
  (md : ManifestData) -> (refs : List Ref) ->
  (mode : VerificationMode) -> (maxAge : Maybe Nat) ->
  (vm : ValidManifest) ->
  Not (validateManifest
        (MkManifest md refs Nothing (Just (MkPolicy mode maxAge True)))
       = Right vm)
requireSigNoAttestationRejected md refs mode maxAge vm prf
  with (isVersionSupported md.version)
  requireSigNoAttestationRejected md refs mode maxAge vm prf | False = absurd prf
  requireSigNoAttestationRejected md refs mode maxAge vm prf | True
    with (md.subsystem == "")
    requireSigNoAttestationRejected md refs mode maxAge vm prf | True | True = absurd prf
    requireSigNoAttestationRejected md refs mode maxAge vm prf | True | False
      with (the (Either ValidationError ()) (traverse_ validateRef refs))
      requireSigNoAttestationRejected md refs mode maxAge vm prf | True | False | Left e   = absurd prf
      requireSigNoAttestationRejected md refs mode maxAge vm prf | True | False | Right () = absurd prf

--------------------------------------------------------------------------------
-- Soundness of validateManifest
--------------------------------------------------------------------------------

||| SOUNDNESS (Stage 2.1): a manifest accepted by `validateManifest` satisfies the
||| four invariants the validator enforces - supported version, non-empty
||| subsystem, well-formed ref hashes, and the clock-free policy constraints.
||| `ValidManifest` is therefore a genuine validity witness, not a mere wrapper.
|||
||| Proved by inverting the Either-monad validation pipeline: each guard that could
||| have produced a `Left` is shown to have taken its `Right` branch, since the
||| whole returned `Right`.
export
validateManifestSound : (m : Manifest) -> (vm : ValidManifest) ->
  validateManifest m = Right vm ->
  ( isVersionSupported m.manifestData.version = True
  , (m.manifestData.subsystem == "") = False
  , All RefValid m.refs
  , validatePolicy m = Right () )
validateManifestSound m vm prf with (isVersionSupported m.manifestData.version)
  validateManifestSound m vm prf | False = absurd prf
  validateManifestSound m vm prf | True with (m.manifestData.subsystem == "")
    validateManifestSound m vm prf | True | True  = absurd prf
    validateManifestSound m vm prf | True | False with (traverse_ validateRef m.refs) proof trEq
      validateManifestSound m vm prf | True | False | Left  e  = absurd prf
      validateManifestSound m vm prf | True | False | Right () with (validatePolicy m)
        validateManifestSound m vm prf | True | False | Right () | Left  e  = absurd prf
        validateManifestSound m vm prf | True | False | Right () | Right () =
          (Refl, Refl, traverseRefsSound m.refs trEq, Refl)
