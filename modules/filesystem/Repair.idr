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

||| Repair a single block in the filesystem (linear version).
||| The old FSState is consumed (Quantity 1) and a new one is returned.
||| This prevents use-after-repair bugs at compile time.
|||
||| @ oldState The filesystem state to repair (will be consumed)
||| @ blockIdx Index of the block to repair
||| @ expectedHash The hash the block should have after repair
export
repairBlock : HasIO io
           => (1 oldState : FSState)
           -> (blockIdx : BlockIndex)
           -> (expectedHash : Hash)
           -> io (Either OchranceError FSState)
repairBlock oldState blockIdx expectedHash = do
  -- Check if block index is valid
  if blockIdx >= oldState.numBlocks
     then pure (Left (QError (InvalidManifestPath ("Block index out of range: " ++ show blockIdx))))
     else do
       -- In a real implementation, this would:
       -- 1. Read the corrupted block
       -- 2. Attempt to repair from redundancy/parity data
       -- 3. Verify the repaired block matches expectedHash
       -- 4. Write the repaired block back to disk

       -- For now, we create a new state with the hash updated
       let newState = MkFSState
             oldState.numBlocks
             (\idx => if idx == blockIdx then Just expectedHash else oldState.blockHash idx)
             oldState.metadata

       pure (Right newState)

||| Repair filesystem from a snapshot (linear version).
||| Consumes the old state and produces a new state matching the snapshot.
|||
||| @ oldState The filesystem state to repair (will be consumed)
||| @ snapshot The target snapshot to restore to
export
repairFromSnapshot : HasIO io
                  => (1 oldState : FSState)
                  -> (snapshot : FSSnapshot)
                  -> io (Either OchranceError FSState)
repairFromSnapshot oldState snapshot = do
  -- Verify snapshot has same block count
  if oldState.numBlocks /= snapshot.blockCount
     then pure (Left (QError (InvalidManifestPath "Block count mismatch")))
     else do
       -- Build new block hash function from snapshot refs
       let newBlockHashFunc = buildHashFunction snapshot.refs

       let newState = MkFSState
             oldState.numBlocks
             newBlockHashFunc
             oldState.metadata

       pure (Right newState)
  where
    buildHashFunction : List Ref -> (BlockIndex -> Maybe Hash)
    buildHashFunction refs idx =
      findHashForBlock ("block_" ++ show idx) refs

    findHashForBlock : String -> List Ref -> Maybe Hash
    findHashForBlock name [] = Nothing
    findHashForBlock name (ref :: refs) =
      if ref.name == name
         then Just ref.hash
         else findHashForBlock name refs

||| Verify filesystem and repair if needed (linear version).
||| This is the main entry point for repair operations.
|||
||| @ oldState The filesystem state to verify/repair (will be consumed)
||| @ manifest The validated manifest to verify against
export
linearVerifyAndRepair : HasIO io
                     => (1 oldState : FSState)
                     -> (manifest : ValidManifest)
                     -> io (Either OchranceError (FSState, RepairProof FSState))
linearVerifyAndRepair oldState manifest = do
  -- First attempt verification without repair
  let unwrapped = unwrapValid manifest
  verifyResult <- verifyState oldState unwrapped

  case verifyResult of
    Right () => do
      -- Verification succeeded, no repair needed
      pure (Right (oldState, NoRepairNeeded manifest))

    Left err => do
      -- Verification failed, attempt repair
      repairResult <- repairFromRefs oldState unwrapped.refs

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
    -- Helper: verify state matches refs
    verifyState : FSState -> Manifest -> io (Either OchranceError ())
    verifyState fs m = do
      verifyRefs fs m.refs

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

    -- Helper: repair from refs
    repairFromRefs : FSState -> List Ref -> io (Either OchranceError (FSState, Nat))
    repairFromRefs fs refs = do
      repairLoop fs refs 0

    repairLoop : FSState -> List Ref -> Nat -> io (Either OchranceError (FSState, Nat))
    repairLoop fs [] count = pure (Right (fs, count))
    repairLoop fs (ref :: refs) count = do
      case parseBlockIndex ref.name of
        Nothing => repairLoop fs refs count
        Just idx => do
          -- Check if repair needed
          case fs.blockHash idx of
            Just actualHash =>
              if actualHash == ref.hash
                 then repairLoop fs refs count
                 else do
                   -- Repair needed
                   result <- repairBlock fs idx ref.hash
                   case result of
                     Left err => pure (Left err)
                     Right newFs => repairLoop newFs refs (count + 1)
            Nothing => do
              -- Block missing, needs repair
              result <- repairBlock fs idx ref.hash
              case result of
                Left err => pure (Left err)
                Right newFs => repairLoop newFs refs (count + 1)

    parseBlockIndex : String -> Maybe BlockIndex
    parseBlockIndex name =
      case words name of
        ["block", numStr] => parseNat numStr
        _ => Nothing

    parseNat : String -> Maybe Nat
    parseNat s = case all isDigit (unpack s) of
      False => Nothing
      True => Just (cast s)

    words : String -> List String
    words s = filter (not . null) (split (== '_') s)

    split : (Char -> Bool) -> String -> List String
    split p s = splitHelper p (unpack s) [] []
      where
        splitHelper : (Char -> Bool) -> List Char -> List Char -> List String -> List String
        splitHelper p [] acc res = reverse (pack (reverse acc) :: res)
        splitHelper p (c :: cs) acc res =
          if p c
             then splitHelper p cs [] (pack (reverse acc) :: res)
             else splitHelper p cs (c :: acc) res

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
repairBlocks oldState [] = pure (Right oldState)
repairBlocks oldState ((idx, hash) :: rest) = do
  result <- repairBlock oldState idx hash
  case result of
    Left err => pure (Left err)
    Right newState => repairBlocks newState rest
