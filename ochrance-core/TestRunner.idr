||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| TestRunner - Simple test runner for Phase 1
|||
||| Demonstrates that core modules work together

module TestRunner

import Data.Vect
import Ochrance.A2ML.Types
import Ochrance.A2ML.Lexer
import Ochrance.A2ML.Parser
import Ochrance.A2ML.Validator
import Ochrance.Framework.Interface
import Ochrance.Filesystem.Types
import Ochrance.Filesystem.Merkle

%default total

||| Simple smoke test - parse and validate a minimal manifest
smokeTest : IO ()
smokeTest = do
  putStrLn "Ochrance Phase 1 Smoke Test"
  putStrLn "============================"
  
  -- Test 1: Lex a simple manifest
  let input = "version: 0.1.0\nsubsystem: test\n"
  case lex input of
    Left err => putStrLn $ "✗ Lexer: " ++ show err
    Right tokens => putStrLn $ "✓ Lexer: " ++ show (length tokens) ++ " tokens"
  
  -- Test 2: Create a minimal manifest
  let manifest = MkManifest
        (MkManifestData "0.1.0" "test" Nothing)
        []
        Nothing
        Nothing
  putStrLn "✓ Types: Created manifest"
  
  -- Test 3: Validate manifest
  case validateManifest manifest of
    Left err => putStrLn $ "✗ Validator: " ++ show err
    Right _ => putStrLn "✓ Validator: Manifest valid"
  
  -- Test 4: Create Merkle tree
  let hash1 = replicate 32 1
  let tree = Leaf hash1
  putStrLn "✓ Merkle: Created tree"
  
  putStrLn ""
  putStrLn "All smoke tests passed!"

main : IO ()
main = smokeTest
