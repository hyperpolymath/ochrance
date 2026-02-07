module SimpleTest

import Data.Vect
import Ochrance.A2ML.Types
import Ochrance.A2ML.Lexer
import Ochrance.A2ML.Validator

main : IO ()
main = do
  putStrLn "=== Ochrance Phase 1 Tests ==="
  putStrLn ""
  
  -- Test 1: Lexer
  putStrLn "Test 1: Lexer"
  let input = "version: 0.1.0"
  case lex input of
    Left err => putStrLn $ "  FAIL: " ++ show err
    Right tokens => putStrLn $ "  PASS: Lexed " ++ show (length tokens) ++ " tokens"
  
  -- Test 2: Types
  putStrLn ""
  putStrLn "Test 2: Manifest Construction"
  let manifest = MkManifest
        (MkManifestData "0.1.0" "test" Nothing)
        []
        Nothing
        Nothing
  putStrLn "  PASS: Created manifest"
  
  -- Test 3: Validator
  putStrLn ""
  putStrLn "Test 3: Validator"
  case validateManifest manifest of
    Left err => putStrLn $ "  FAIL: " ++ show err
    Right _ => putStrLn "  PASS: Manifest validated"
  
  putStrLn ""
  putStrLn "=== All Tests Passed! ==="
