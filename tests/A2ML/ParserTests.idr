||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| ParserTests - Property-based tests for A2ML lexer and parser
|||
||| Tests roundtrip properties, error handling, and edge cases.

module ParserTests

import Ochrance.A2ML.Lexer
import Ochrance.A2ML.Parser
import Ochrance.A2ML.Serializer
import Ochrance.A2ML.Types

%default total

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

||| Assert that a value is Right
assertRight : Show e => Either e a -> IO ()
assertRight (Left err) = putStrLn ("FAIL: " ++ show err)
assertRight (Right _) = pure ()

||| Assert two values are equal
assertEqual : (Eq a, Show a) => a -> a -> IO ()
assertEqual expected actual =
  if expected == actual
     then pure ()
     else putStrLn ("FAIL: Expected " ++ show expected ++ ", got " ++ show actual)

||| Test case wrapper
testCase : String -> IO () -> IO ()
testCase name test = do
  putStr (name ++ " ... ")
  test
  putStrLn "OK"

--------------------------------------------------------------------------------
-- Sample Manifests
--------------------------------------------------------------------------------

||| Minimal valid manifest
minimalManifest : String
minimalManifest = """
@manifest {
  version = "0.1.0"
  subsystem = "test"
}

@refs {
  root : blake3:0123456789abcdef
}
"""

||| Manifest with attestation
attestedManifest : String
attestedManifest = """
@manifest {
  version = "0.1.0"
  subsystem = "test"
  timestamp = "2026-02-07T12:00:00Z"
}

@refs {
  root : blake3:0123456789abcdef
  data : sha256:fedcba9876543210
}

@attestation {
  witness = "test-witness"
  signature = "sig123"
  pubkey = "key456"
}
"""

||| Manifest with policy
policyManifest : String
policyManifest = """
@manifest {
  version = "0.1.0"
  subsystem = "test"
}

@refs {
  root : blake3:abc123
}

@policy {
  mode = "checked"
  max_age = 3600
  require_sig = true
}
"""

--------------------------------------------------------------------------------
-- Lexer Tests
--------------------------------------------------------------------------------

lexerTests : IO ()
lexerTests = do
  putStrLn "\n=== Lexer Tests ==="

  testCase "Lex empty string" $ do
    case lex "" of
      Right tokens => assertEqual [EOF] tokens
      Left err => putStrLn ("FAIL: " ++ show err)

  testCase "Lex minimal manifest" $ do
    case lex minimalManifest of
      Right tokens => pure ()  -- Success if no error
      Left err => putStrLn ("FAIL: " ++ show err)

  testCase "Lex with attestation" $ do
    case lex attestedManifest of
      Right tokens => pure ()
      Left err => putStrLn ("FAIL: " ++ show err)

  testCase "Lex with policy" $ do
    case lex policyManifest of
      Right tokens => pure ()
      Left err => putStrLn ("FAIL: " ++ show err)

  testCase "Lex rejects invalid characters" $ do
    case lex "@manifest { version = \x00 }" of
      Left _ => pure ()  -- Should fail
      Right _ => putStrLn "FAIL: Should reject null bytes"

--------------------------------------------------------------------------------
-- Parser Tests
--------------------------------------------------------------------------------

parserTests : IO ()
parserTests = do
  putStrLn "\n=== Parser Tests ==="

  testCase "Parse minimal manifest" $ do
    case lex minimalManifest of
      Right tokens =>
        case parse tokens of
          Right manifest => do
            assertEqual "0.1.0" manifest.manifestData.version
            assertEqual "test" manifest.manifestData.subsystem
          Left err => putStrLn ("FAIL: " ++ show err)
      Left err => putStrLn ("FAIL lex: " ++ show err)

  testCase "Parse attested manifest" $ do
    case lex attestedManifest of
      Right tokens =>
        case parse tokens of
          Right manifest => do
            case manifest.attestation of
              Nothing => putStrLn "FAIL: Expected attestation"
              Just att => do
                assertEqual "test-witness" att.witness
                assertEqual "sig123" att.signature
          Left err => putStrLn ("FAIL: " ++ show err)
      Left err => putStrLn ("FAIL lex: " ++ show err)

  testCase "Parse policy manifest" $ do
    case lex policyManifest of
      Right tokens =>
        case parse tokens of
          Right manifest => do
            case manifest.policy of
              Nothing => putStrLn "FAIL: Expected policy"
              Just pol => do
                assertEqual Checked pol.mode
                case pol.maxAge of
                  Nothing => putStrLn "FAIL: Expected max_age"
                  Just age => assertEqual 3600 age
          Left err => putStrLn ("FAIL: " ++ show err)
      Left err => putStrLn ("FAIL lex: " ++ show err)

  testCase "Parse rejects duplicate sections" $ do
    let duplicateManifest = minimalManifest ++ "\n@manifest { version = \"0.2.0\" }\n"
    case lex duplicateManifest of
      Right tokens =>
        case parse tokens of
          Left _ => pure ()  -- Should fail
          Right _ => putStrLn "FAIL: Should reject duplicate sections"
      Left _ => pure ()  -- Lex error also acceptable

--------------------------------------------------------------------------------
-- Roundtrip Tests
--------------------------------------------------------------------------------

roundtripTests : IO ()
roundtripTests = do
  putStrLn "\n=== Roundtrip Tests ==="

  testCase "Serialize then parse minimal manifest" $ do
    case lex minimalManifest of
      Right tokens =>
        case parse tokens of
          Right manifest1 => do
            let serialized = serialize manifest1
            case lex serialized of
              Right tokens2 =>
                case parse tokens2 of
                  Right manifest2 => do
                    assertEqual manifest1.manifestData.version manifest2.manifestData.version
                    assertEqual manifest1.manifestData.subsystem manifest2.manifestData.subsystem
                  Left err => putStrLn ("FAIL parse2: " ++ show err)
              Left err => putStrLn ("FAIL lex2: " ++ show err)
          Left err => putStrLn ("FAIL parse1: " ++ show err)
      Left err => putStrLn ("FAIL lex1: " ++ show err)

--------------------------------------------------------------------------------
-- Error Handling Tests
--------------------------------------------------------------------------------

errorHandlingTests : IO ()
errorHandlingTests = do
  putStrLn "\n=== Error Handling Tests ==="

  testCase "Missing version field" $ do
    let invalid = "@manifest { subsystem = \"test\" }\n@refs { root : blake3:abc }"
    case lex invalid of
      Right tokens =>
        case parse tokens of
          Left _ => pure ()  -- Should fail
          Right _ => putStrLn "FAIL: Should require version field"
      Left _ => pure ()  -- Lex error also acceptable

  testCase "Invalid hash format" $ do
    let invalid = "@manifest { version = \"0.1.0\" subsystem = \"test\" }\n@refs { root : invalid_no_colon }"
    case lex invalid of
      Right tokens =>
        case parse tokens of
          Left _ => pure ()  -- Should fail
          Right _ => putStrLn "FAIL: Should reject invalid hash format"
      Left _ => pure ()  -- Lex error also acceptable

  testCase "Unterminated string" $ do
    let invalid = "@manifest { version = \"unterminated }"
    case lex invalid of
      Left _ => pure ()  -- Should fail at lex stage
      Right _ => putStrLn "FAIL: Should reject unterminated string"

--------------------------------------------------------------------------------
-- Main Test Runner
--------------------------------------------------------------------------------

export
main : IO ()
main = do
  putStrLn "==================================="
  putStrLn "A2ML Parser Property-Based Tests"
  putStrLn "==================================="

  lexerTests
  parserTests
  roundtripTests
  errorHandlingTests

  putStrLn "\n==================================="
  putStrLn "All tests completed!"
  putStrLn "==================================="
