||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.Filesystem.Verify - Filesystem verification logic
|||
||| Implements the VerifiedSubsystem interface for filesystem state.
||| Compares filesystem block hashes against an A2ML manifest.

module Ochrance.Filesystem.Verify

import Ochrance.A2ML.Types
import Ochrance.Framework.Interface
import Ochrance.Framework.Proof
import Ochrance.Framework.Error
import Ochrance.Filesystem.Types
import Ochrance.Filesystem.Merkle

%default total

-- TODO: Implement VerifiedSubsystem for FSState
-- This requires:
-- 1. generateManifest: hash all blocks, build Merkle tree, produce A2ML
-- 2. verify: compare manifest refs against current block hashes
-- 3. repair: re-read corrupted blocks from backup/redundancy
