||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.Framework.Interface - The VerifiedSubsystem interface
|||
||| Any subsystem (filesystem, memory, network) that wants Ochránce
||| verification must implement this interface.

module Ochrance.Framework.Interface

import Ochrance.A2ML.Types
import Ochrance.Framework.Error
import Ochrance.Framework.Proof

%default total

--------------------------------------------------------------------------------
-- VerifiedSubsystem Interface
--------------------------------------------------------------------------------

||| Interface that verified subsystems must implement.
||| Each subsystem provides its own state type and verification logic.
public export
interface VerifiedSubsystem (state : Type) where
  ||| Name of this subsystem (e.g. "filesystem", "memory")
  subsystemName : String

  ||| Generate a manifest from the current state
  generateManifest : state -> Either OchranceError Manifest

  ||| Verify state against a validated manifest
  verify : state -> ValidManifest -> Either OchranceError (VerificationProof state)

  ||| Attempt to repair state to match manifest (consumes old state)
  repair : (1 _ : state) -> ValidManifest -> IO (Either OchranceError state)
