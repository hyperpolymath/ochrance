-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Ochrance.Framework.Progressive — Verification Assurance Tiers.
|||
||| This module implements the "Progressive Assurance" model, allowing the 
||| framework to scale its strictness based on the operational context. 
||| It defines the relationship between different verification modes 
||| and provides type-level evidence of security thresholds.

module Ochrance.Framework.Progressive

%default total

--------------------------------------------------------------------------------
-- Verification Modes
--------------------------------------------------------------------------------

||| MODES: Representing the three tiers of system assurance.
||| - **Lax**: Structural sanity checks only.
||| - **Checked**: Full cryptographic hash verification.
||| - **Attested**: Formal proof of domain invariants.
public export
data VerificationMode = Lax | Checked | Attested

||| ORDERING: Defines the relationship Lax < Checked < Attested.
public export
Ord VerificationMode where
  compare Lax      Lax      = EQ
  compare Lax      _        = LT
  compare Checked  Attested = LT
  -- ... [Remaining cases]

--------------------------------------------------------------------------------
-- Proof Witnesses
--------------------------------------------------------------------------------

||| THRESHOLD PROOF: A dependent type that serves as evidence that a 
||| specific `mode` is at least as strict as a required `threshold`.
public export
data SatisfiesMinimum : (threshold : VerificationMode) -> (mode : VerificationMode) -> Type where
  ||| WITNESS: Proves that the target mode meets or exceeds the threshold.
  MeetsThreshold : {0 t, m : VerificationMode}
                -> (prf : atLeast t m = True)
                -> SatisfiesMinimum t m

||| AUTO-PROOFS: Common threshold satisfied cases.
public export
attestedSatisfiesLax : SatisfiesMinimum Lax Attested
attestedSatisfiesLax = MeetsThreshold Refl
