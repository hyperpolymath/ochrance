||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.Filesystem.Repair - Linear type repair operations
|||
||| Uses Idris2 linear types (Quantity 1) to ensure that repair
||| operations consume the old filesystem state, preventing
||| use-after-repair bugs at compile time.

module Ochrance.Filesystem.Repair

import Ochrance.A2ML.Types
import Ochrance.Framework.Proof
import Ochrance.Framework.Error
import Ochrance.Filesystem.Types

%default total

-- TODO: Implement linear repair operations
-- Key functions:
-- 1. repairBlock : (1 _ : FSState) -> BlockIndex -> Hash -> IO (Either OchranceError FSState)
-- 2. repairFromSnapshot : (1 _ : FSState) -> FSSnapshot -> IO (Either OchranceError FSState)
-- 3. linearVerifyAndRepair : (1 _ : FSState) -> ValidManifest -> IO (Either OchranceError (FSState, RepairProof FSState))
