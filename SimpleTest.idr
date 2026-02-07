||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Simple End-to-End Test for Ochrance
|||
||| Demonstrates basic workflow: parse, validate, verify

module SimpleTest

import Data.Vect
import Ochrance.A2ML.Types
import Ochrance.A2ML.Lexer
import Ochrance.A2ML.Parser
import Ochrance.A2ML.Validator
import Ochrance.Framework.Error
import Ochrance.Framework.Proof
import Ochrance.Filesystem.Types
import Ochrance.Filesystem.Verify

--------------------------------------------------------------------------------
-- Test Data
--------------------------------------------------------------------------------

sampleManifest : String
sampleManifest = "version = \"0.1.0\"\nsubsystem = \"test-filesystem\"\n\n[refs]\nblock_0 = { hash = \"sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\" }\n"

createTestFS : FSState
createTestFS = MkFSState 1 testHash testMetadata
  where
    testHash : BlockIndex -> Maybe Hash
    testHash 0 = Just (MkHash SHA256 "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
    testHash _ = Nothing

    testMetadata : ManifestData
    testMetadata = MkManifestData "0.1.0" "test-filesystem" Nothing

--------------------------------------------------------------------------------
-- Main Test
--------------------------------------------------------------------------------

export
main : IO ()
main = do
  putStrLn "=== Ochrance Simple End-to-End Test ==="
  putStrLn ""

  putStrLn "Step 1: Lex"
  case lex sampleManifest of
    Left lexErr => putStrLn "  ✗ Lex FAILED"
    Right tokens => do
      putStrLn $ "  ✓ Lexed " ++ show (length tokens) ++ " tokens"

      putStrLn ""
      putStrLn "Step 2: Parse"
      case parse tokens of
        Left err => putStrLn "  ✗ Parse FAILED"
        Right manifest => do
          putStrLn "  ✓ Parse PASSED"

          putStrLn ""
          putStrLn "Step 3: Validate"
          case validateManifest manifest of
            Left err => putStrLn "  ✗ Validate FAILED"
            Right _ => do
              putStrLn "  ✓ Validate PASSED"

              putStrLn ""
              putStrLn "Step 4: Verify Filesystem"
              case validateManifest manifest of
                Left err => putStrLn "  ✗ Re-validate FAILED"
                Right validManifest => do
                  result <- verify createTestFS validManifest
                  case result of
                    Left err => putStrLn "  ✗ Verify FAILED"
                    Right _ => putStrLn "  ✓ Verify PASSED"

  putStrLn ""
  putStrLn "=== Test Complete ==="
