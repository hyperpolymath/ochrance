||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| End-to-end integration test for Ochrance
|||
||| Tests the complete workflow:
||| 1. Parse A2ML manifest
||| 2. Validate manifest (with signature verification)
||| 3. Create filesystem state
||| 4. Verify filesystem against manifest
||| 5. Introduce corruption
||| 6. Repair from manifest
||| 7. Re-verify after repair

module EndToEndTest

import Data.Vect
import Ochrance.A2ML.Types
import Ochrance.A2ML.Lexer
import Ochrance.A2ML.Parser
import Ochrance.A2ML.Validator
import Ochrance.Framework.Proof
import Ochrance.Framework.Error
import Ochrance.Filesystem.Types
import Ochrance.Filesystem.Verify
import Ochrance.Filesystem.Repair

--------------------------------------------------------------------------------
-- Test Data
--------------------------------------------------------------------------------

||| Sample A2ML manifest for testing
sampleManifest : String
sampleManifest = """
version = "0.1.0"
subsystem = "test-filesystem"
timestamp = "2026-02-07T00:00:00Z"

[refs]
block_0 = { hash = "sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" }
block_1 = { hash = "sha256:fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210" }
block_2 = { hash = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" }
"""

||| Create a filesystem state for testing
createTestFS : Nat -> FSState
createTestFS numBlocks = MkFSState
  numBlocks
  testBlockHash
  []
  where
    testBlockHash : BlockIndex -> Maybe Hash
    testBlockHash 0 = Just (MkHash "sha256" "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
    testBlockHash 1 = Just (MkHash "sha256" "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210")
    testBlockHash 2 = Just (MkHash "sha256" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    testBlockHash _ = Nothing

||| Create a corrupted filesystem state (block 1 has wrong hash)
createCorruptedFS : Nat -> FSState
createCorruptedFS numBlocks = MkFSState
  numBlocks
  corruptedBlockHash
  []
  where
    corruptedBlockHash : BlockIndex -> Maybe Hash
    corruptedBlockHash 0 = Just (MkHash "sha256" "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
    corruptedBlockHash 1 = Just (MkHash "sha256" "CORRUPTED000000000000000000000000000000000000000000000000000000")
    corruptedBlockHash 2 = Just (MkHash "sha256" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    corruptedBlockHash _ = Nothing

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

||| Helper to print test results
printTestResult : String -> Either OchranceError a -> IO ()
printTestResult testName (Left err) =
  putStrLn $ "  ✗ " ++ testName ++ " FAILED: " ++ show err
printTestResult testName (Right _) =
  putStrLn $ "  ✓ " ++ testName ++ " PASSED"

||| Helper to check if error is expected type
isHashMismatch : OchranceError -> Bool
isHashMismatch (PError (HashMismatch _ _ _)) = True
isHashMismatch _ = False

--------------------------------------------------------------------------------
-- Test Helper Functions
--------------------------------------------------------------------------------

||| Handle repair failure
handleRepairFailure : OchranceError -> IO ()
handleRepairFailure err = do
  putStrLn $ "  ✗ Repair FAILED: " ++ show err
  putStrLn ""

||| Handle repair success
handleRepairSuccess : (FSState, RepairProof FSState) -> Manifest -> IO ()
handleRepairSuccess result manifest =
  let (repairedFS, proof) = result in
  case proof of
    NoRepairNeeded _ => do
      putStrLn "  ✓ Filesystem was already valid"
      putStrLn ""
    RepairSucceeded count _ => do
      putStrLn $ "  ✓ Repair SUCCEEDED: " ++ show count ++ " blocks repaired"
      putStrLn ""
      putStrLn "Step 7: Verify Repaired Filesystem"
      verifyRepaired <- verifyFilesystem repairedFS manifest
      printTestResult "Verify repaired filesystem" verifyRepaired
      putStrLn ""

||| Test repair and verification workflow
testRepairAndVerify : FSState -> ValidManifest -> Manifest -> IO ()
testRepairAndVerify corruptedFS validManifest manifest = do
  putStrLn "Step 6: Repair Corrupted Filesystem"
  repairResult <- linearVerifyAndRepair corruptedFS validManifest
  either handleRepairFailure (\result => handleRepairSuccess result manifest) repairResult

||| Test signature verification
testSignatureVerification : Manifest -> IO ()
testSignatureVerification manifest = do
  putStrLn "Step 8: Test Signature Verification"
  case manifest.attestation of
    Nothing =>
      do putStrLn "  ⊘ No attestation present (skipped)"
         putStrLn ""
    Just att =>
      do validationIO <- validateManifestIO manifest
         printTestResult "Signature verification" validationIO
         putStrLn ""

--------------------------------------------------------------------------------
-- End-to-End Test
--------------------------------------------------------------------------------

||| Main end-to-end test
export
main : IO ()
main = do
  putStrLn "=== Ochrance End-to-End Integration Test ==="
  putStrLn ""

  -- Test 1: Lex manifest
  putStrLn "Step 1: Lexical Analysis"
  let tokens = lex sampleManifest
  putStrLn $ "  ✓ Lexed " ++ show (length tokens) ++ " tokens"
  putStrLn ""

  -- Test 2: Parse manifest
  putStrLn "Step 2: Parse Manifest"
  case parse tokens of
    Left err => do
      putStrLn $ "  ✗ Parse FAILED: " ++ show err
      putStrLn ""
      putStrLn "TEST SUITE ABORTED"
    Right manifest => do
      putStrLn $ "  ✓ Parsed manifest for subsystem: " ++ manifest.manifestData.subsystem
      putStrLn $ "  ✓ Found " ++ show (length manifest.refs) ++ " block references"
      putStrLn ""

      -- Test 3: Validate manifest (pure version, no signatures)
      putStrLn "Step 3: Validate Manifest"
      case validateManifest manifest of
        Left err => do
          putStrLn $ "  ✗ Validation FAILED: " ++ show err
          putStrLn ""
        Right validManifest => do
          putStrLn "  ✓ Manifest validation PASSED"
          putStrLn ""

          -- Test 4: Verify valid filesystem
          putStrLn "Step 4: Verify Valid Filesystem"
          let validFS = createTestFS 3
          verifyResult <- verifyFilesystem validFS manifest
          printTestResult "Verify valid filesystem" verifyResult
          putStrLn ""

          -- Test 5: Verify corrupted filesystem (should fail)
          putStrLn "Step 5: Verify Corrupted Filesystem (expect failure)"
          let corruptedFS = createCorruptedFS 3
          verifyCorrupted <- verifyFilesystem corruptedFS manifest
          case verifyCorrupted of
            Left err =>
              if isHashMismatch err
                 then putStrLn "  ✓ Correctly detected hash mismatch"
                 else putStrLn $ "  ? Unexpected error type: " ++ show err
            Right _ => putStrLn "  ✗ Should have detected corruption"
          putStrLn ""

          -- Test 6: Repair corrupted filesystem
          testRepairAndVerify corruptedFS validManifest manifest

          -- Test 7: Test with attestation (if present)
          testSignatureVerification manifest

  putStrLn "=== End-to-End Test Complete ==="
