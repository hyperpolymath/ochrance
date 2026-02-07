module HexTest

import Data.Vect
import Ochrance.Util.Hex

main : IO ()
main = do
  putStrLn "=== Hex Parsing Tests ==="
  putStrLn ""
  
  -- Test 1: Simple hex string
  putStrLn "Test 1: Parse '00ff'"
  case hexStringToBytes "00ff" of
    Nothing => putStrLn "  ✗ FAIL: Could not parse"
    Just bytes => putStrLn $ "  ✓ PASS: " ++ show bytes
  
  -- Test 2: Hex to vect (32 bytes)
  putStrLn ""
  putStrLn "Test 2: Parse 32-byte hex (64 chars)"
  let hexStr = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
  case hexStringToVect 32 hexStr of
    Nothing => putStrLn "  ✗ FAIL: Could not parse"
    Just v => putStrLn $ "  ✓ PASS: Parsed 32 bytes"
  
  -- Test 3: Hex to vect (64 bytes for signature)
  putStrLn ""
  putStrLn "Test 3: Parse 64-byte hex (128 chars)"
  let sig = "0123456789abcdef" ++ "0123456789abcdef" ++
            "0123456789abcdef" ++ "0123456789abcdef" ++
            "0123456789abcdef" ++ "0123456789abcdef" ++
            "0123456789abcdef" ++ "0123456789abcdef"
  case hexStringToVect 64 sig of
    Nothing => putStrLn "  ✗ FAIL: Could not parse"
    Just v => putStrLn $ "  ✓ PASS: Parsed 64 bytes"
  
  -- Test 4: Roundtrip (bytes -> hex -> bytes)
  putStrLn ""
  putStrLn "Test 4: Roundtrip encoding"
  let bytes : Vect 4 Bits8 = [0xDE, 0xAD, 0xBE, 0xEF]
  let hex = vectToHex bytes
  putStrLn $ "  Original: " ++ show bytes
  putStrLn $ "  Hex: " ++ hex
  case hexStringToVect 4 hex of
    Nothing => putStrLn "  ✗ FAIL: Could not parse back"
    Just parsed => if toList parsed == toList bytes
                      then putStrLn "  ✓ PASS: Roundtrip successful"
                      else putStrLn "  ✗ FAIL: Roundtrip mismatch"
  
  putStrLn ""
  putStrLn "=== All Hex Tests Complete ==="
