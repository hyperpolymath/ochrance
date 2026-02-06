||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.Framework.Proof - Proof witnesses for verification results
|||
||| These types serve as compile-time evidence that verification has
||| occurred. A VerificationProof can only be constructed by the
||| verify function, ensuring no forgery.

module Ochrance.Framework.Proof

import Ochrance.A2ML.Types

%default total

--------------------------------------------------------------------------------
-- Verification Proof Witnesses
--------------------------------------------------------------------------------

||| Evidence that a subsystem state matches a manifest.
||| The state type parameter ties the proof to a specific subsystem.
public export
data VerificationProof : (state : Type) -> Type where
  ||| Lax verification passed (manifest parsed but hashes not checked)
  LaxProof : (manifest : ValidManifest) -> VerificationProof state
  ||| Checked verification passed (all hashes match)
  CheckedProof : (manifest : ValidManifest)
              -> (rootHash : Hash)
              -> VerificationProof state
  ||| Attested verification passed (hashes match + signature valid)
  AttestedProof : (manifest : ValidManifest)
               -> (rootHash : Hash)
               -> (signature : String)
               -> VerificationProof state

||| Extract the verification mode from a proof
public export
proofMode : VerificationProof state -> VerificationMode
proofMode (LaxProof _)          = Lax
proofMode (CheckedProof _ _)    = Checked
proofMode (AttestedProof _ _ _) = Attested

||| Extract the manifest from a proof
public export
proofManifest : VerificationProof state -> ValidManifest
proofManifest (LaxProof m)          = m
proofManifest (CheckedProof m _)    = m
proofManifest (AttestedProof m _ _) = m

--------------------------------------------------------------------------------
-- Repair Witnesses
--------------------------------------------------------------------------------

||| Evidence that a repair operation was performed.
||| Linear: consuming this proof consumes the old state reference.
public export
data RepairProof : (state : Type) -> Type where
  ||| Repair succeeded, new state matches manifest
  RepairSucceeded : (blocksRepaired : Nat)
                 -> (manifest : ValidManifest)
                 -> RepairProof state
  ||| No repair needed, state already matches
  NoRepairNeeded : (manifest : ValidManifest) -> RepairProof state
