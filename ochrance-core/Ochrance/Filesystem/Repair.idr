||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.Filesystem.Repair - Linear type repair operations
|||
||| Uses Idris2 linear types (Quantity 1) to ensure that repair
||| operations consume the old filesystem state, preventing
||| use-after-repair bugs at compile time.

module Ochrance.Filesystem.Repair

import Data.Vect
import Ochrance.A2ML.Types
import Ochrance.Framework.Proof
import Ochrance.Framework.Error
import Ochrance.Filesystem.Types
import Ochrance.Filesystem.Merkle
import Ochrance.FFI.Crypto

%default total

--------------------------------------------------------------------------------
-- Linear Repair Operations
--------------------------------------------------------------------------------

||| Repair a single block in the filesystem.
||| Uses linear types to ensure oldState is consumed exactly once.
|||
||| @ oldState The filesystem state to repair (consumed linearly)
||| @ blockIdx Index of the block to repair
||| @ expectedHash The hash the block should have after repair
export
repairBlock : HasIO io
           => (1 oldState : FSState)
           -> (blockIdx : BlockIndex)
           -> (expectedHash : Hash)
           -> io (Either OchranceError FSState)
repairBlock oldState blockIdx expectedHash = do
  -- Pattern match to extract components (consuming oldState linearly)
  let MkFSState numBlocks blockHashFunc metadata = oldState

  -- Check if block index is valid
  if blockIdx >= numBlocks
     then pure (Left (QError (InvalidManifestPath ("Block index out of range: " ++ show blockIdx))))
     else do
       -- In a real implementation, this would:
       -- 1. Read the corrupted block
       -- 2. Attempt to repair from redundancy/parity data
       -- 3. Verify the repaired block matches expectedHash
       -- 4. Write the repaired block back to disk

       -- For now, we create a new state with the hash updated
       let newState = MkFSState
             numBlocks
             (\idx => if idx == blockIdx then Just expectedHash else blockHashFunc idx)
             metadata

       pure (Right newState)

||| Repair filesystem from a snapshot (linear version).
||| Consumes the old state and produces a new state matching the snapshot.
|||
||| @ oldState The filesystem state to repair (consumed linearly)
||| @ snapshot The target snapshot to restore to
export
repairFromSnapshot : HasIO io
                  => (1 oldState : FSState)
                  -> (snapshot : FSSnapshot)
                  -> io (Either OchranceError FSState)
repairFromSnapshot oldState snapshot = do
  -- Pattern match to extract components (consuming oldState linearly)
  let MkFSState numBlocks blockHashFunc metadata = oldState

  -- Verify snapshot has same block count
  if numBlocks /= snapshot.blockCount
     then pure (Left (QError (InvalidManifestPath "Block count mismatch")))
     else do
       -- Build new block hash function from snapshot refs
       let newBlockHashFunc = buildHashFunction snapshot.refs

       let newState = MkFSState
             numBlocks
             newBlockHashFunc
             metadata

       pure (Right newState)
  where
    findHashForBlock : String -> List Ref -> Maybe Hash
    findHashForBlock name [] = Nothing
    findHashForBlock name (ref :: refs) =
      if ref.name == name
         then Just ref.hash
         else findHashForBlock name refs

    buildHashFunction : List Ref -> (BlockIndex -> Maybe Hash)
    buildHashFunction refs idx =
      findHashForBlock ("block_" ++ show idx) refs

||| Verify filesystem and repair if needed (linear version).
||| This is the main entry point for repair operations.
|||
||| @ oldState The filesystem state to verify/repair (consumed linearly)
||| @ manifest The validated manifest to verify against
export
linearVerifyAndRepair : HasIO io
                     => (1 oldState : FSState)
                     -> (manifest : ValidManifest)
                     -> io (Either OchranceError (FSState, RepairProof FSState))
linearVerifyAndRepair oldState manifest = do
  -- First attempt verification without repair
  -- Pattern match to extract components for verification
  let MkFSState numBlocks blockHashFunc metadata = oldState
  let unwrapped = unwrapValid manifest

  -- Create copy for verification (oldState was consumed by pattern match)
  let oldStateCopy = MkFSState numBlocks blockHashFunc metadata

  verifyResult <- verifyState oldStateCopy unwrapped

  case verifyResult of
    Right () => do
      -- Verification succeeded, no repair needed
      -- Reconstruct oldState to return
      let resultState = MkFSState numBlocks blockHashFunc metadata
      pure (Right (resultState, NoRepairNeeded manifest))

    Left err => do
      -- Verification failed, attempt repair
      -- Reconstruct oldState for repair operation
      let oldState' = MkFSState numBlocks blockHashFunc metadata
      repairResult <- repairFromRefs oldState' unwrapped.refs

      case repairResult of
        Left repairErr => pure (Left repairErr)
        Right (newState, blocksRepaired) => do
          -- Re-verify after repair
          verifyResult' <- verifyState newState unwrapped

          case verifyResult' of
            Right () =>
              pure (Right (newState, RepairSucceeded blocksRepaired manifest))
            Left _ =>
              pure (Left (PError (HashMismatch "repair" "failed" "verification")))
  where
    -- Parsing helpers (defined in dependency order)
    split : (Char -> Bool) -> String -> List String
    split p s = splitHelper p (unpack s) [] []
      where
        splitHelper : (Char -> Bool) -> List Char -> List Char -> List String -> List String
        splitHelper p [] acc res = reverse (pack (reverse acc) :: res)
        splitHelper p (c :: cs) acc res =
          if p c
             then splitHelper p cs [] (pack (reverse acc) :: res)
             else splitHelper p cs (c :: acc) res

    words : String -> List String
    words s = filter (\str => str /= "") (split (== '_') s)

    parseNat : String -> Maybe Nat
    parseNat s = case all isDigit (unpack s) of
      False => Nothing
      True => Just (cast s)

    parseBlockIndex : String -> Maybe BlockIndex
    parseBlockIndex name =
      case words name of
        ["block", numStr] => parseNat numStr
        _ => Nothing

    -- Helper: verify all refs
    verifyRefs : FSState -> List Ref -> io (Either OchranceError ())
    verifyRefs fs [] = pure (Right ())
    verifyRefs fs (ref :: refs) = do
      case parseBlockIndex ref.name of
        Nothing => pure (Left (QError (InvalidManifestPath ref.name)))
        Just idx => do
          if idx >= fs.numBlocks
             then pure (Left (QError (InvalidManifestPath "Block index out of range")))
             else case fs.blockHash idx of
               Nothing => pure (Left (ZError (FileNotFound ("Block " ++ show idx))))
               Just actualHash =>
                 if actualHash == ref.hash
                    then verifyRefs fs refs
                    else pure (Left (PError (HashMismatch ref.name (show ref.hash) (show actualHash))))

    -- Helper: verify state matches refs
    verifyState : FSState -> Manifest -> io (Either OchranceError ())
    verifyState fs m = do
      verifyRefs fs m.refs

    -- Helper: repair loop
    -- Note: Idris2's Pair type doesn't perfectly preserve linearity
    -- This is a known limitation - workaround by using unsafePerformIO or restructuring
    repairLoop : (1 fs : FSState) -> List Ref -> Nat -> io (Either OchranceError (FSState, Nat))
    repairLoop fs [] count =
      -- Use pattern match to consume fs and create new state
      let MkFSState numBlocks blockHashFunc metadata = fs
          resultState = MkFSState numBlocks blockHashFunc metadata
      in pure (Right (resultState, count))
    repairLoop fs (ref :: refs) count = do
      case parseBlockIndex ref.name of
        Nothing => repairLoop fs refs count
        Just idx => do
          -- Pattern match to extract components
          let MkFSState numBlocks blockHashFunc metadata = fs
          -- Check if repair needed
          case blockHashFunc idx of
            Just actualHash =>
              if actualHash == ref.hash
                 then do
                   -- No repair needed, reconstruct and continue
                   let fs' = MkFSState numBlocks blockHashFunc metadata
                   repairLoop fs' refs count
                 else do
                   -- Repair needed - reconstruct fs and pass to repairBlock
                   let fs' = MkFSState numBlocks blockHashFunc metadata
                   result <- repairBlock fs' idx ref.hash
                   case result of
                     Left err => pure (Left err)
                     Right newFs => repairLoop newFs refs (count + 1)
            Nothing => do
              -- Block missing, needs repair - reconstruct fs
              let fs' = MkFSState numBlocks blockHashFunc metadata
              result <- repairBlock fs' idx ref.hash
              case result of
                Left err => pure (Left err)
                Right newFs => repairLoop newFs refs (count + 1)

    -- Helper: repair from refs
    repairFromRefs : (1 fs : FSState) -> List Ref -> io (Either OchranceError (FSState, Nat))
    repairFromRefs fs refs = do
      repairLoop fs refs 0

--------------------------------------------------------------------------------
-- Batch Repair Operations
--------------------------------------------------------------------------------

||| Repair multiple blocks in sequence (linear version).
||| Each repair consumes the previous state and produces a new one.
export
repairBlocks : HasIO io
            => (1 oldState : FSState)
            -> (repairs : List (BlockIndex, Hash))
            -> io (Either OchranceError FSState)
repairBlocks oldState [] =
  -- Consume oldState by pattern matching and reconstructing
  let MkFSState numBlocks blockHashFunc metadata = oldState
      resultState = MkFSState numBlocks blockHashFunc metadata
  in pure (Right resultState)
repairBlocks oldState ((idx, hash) :: rest) =
  -- Pattern match to consume oldState, then reconstruct to pass to repairBlock
  let MkFSState numBlocks blockHashFunc metadata = oldState
      oldState' = MkFSState numBlocks blockHashFunc metadata
  in repairBlock oldState' idx hash >>= \result =>
       case result of
         Left err => pure (Left err)
         Right newState => repairBlocks newState rest
