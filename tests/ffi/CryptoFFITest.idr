||| SPDX-License-Identifier: MPL-2.0
|||
||| CryptoFFITest — end-to-end runtime proof that Ochránce's *production*
||| verification path computes REAL BLAKE3 through libochrance.so.
|||
||| The pure proof corpus is combiner-generic and instantiates the XOR *spec*
||| combiner `xorCombiner`; this executable instead drives the production IO path
||| (`blake3` / `sha256` / `sha3_256` / `hashPairBlake3` / `rootHashBytesIO`),
||| every call of which crosses the FFI boundary into libochrance.so via
||| `%foreign "C:...,libochrance"` (see Ochrance.FFI.Crypto). It checks published
||| known-answer vectors and shows the production Merkle root is the cryptographic
||| BLAKE3 fold, not the XOR spec root — i.e. the real combiner is wired in.
|||
||| Requires libochrance.so on the loader path. Use tests/ffi/run_ffi_test.sh, or:
|||   (cd ffi/zig && zig build)
|||   idris2 --install ochrance.ipkg && idris2 --build tests/ffi/ffi-test.ipkg
|||   LD_LIBRARY_PATH=ffi/zig/zig-out/lib tests/ffi/build/exec/crypto-ffi-test
module CryptoFFITest

import Data.Vect
import Data.List
import System
import Ochrance.FFI.Crypto
import Ochrance.Filesystem.Merkle
import Ochrance.Framework.Error
import Ochrance.Util.Hex

||| Known-answer vectors (lowercase hex) — identical to the in-module Zig tests
||| (ffi/zig/src/main.zig) and the C link test (ffi/zig/test/link_test.c).
katBlake3Empty : String
katBlake3Empty = "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262"

katBlake3Abc : String
katBlake3Abc = "6437b3ac38465133ffb63b75273a8db548c558465d79db03fd359c6cd5bd9d85"

katSha256Abc : String
katSha256Abc = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

katSha3Abc : String
katSha3Abc = "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"

||| "abc" as raw ASCII bytes (avoids any Char->Bits8 cast ambiguity).
abcBytes : List Bits8
abcBytes = [97, 98, 99]

check : String -> Bool -> IO Bool
check label ok = do
  putStrLn ("  " ++ (if ok then "PASS" else "FAIL") ++ ": " ++ label)
  pure ok

||| Run a hashing FFI action and compare the digest to an expected hex string.
runKat : String -> IO (Either OchranceError (Vect 32 Bits8)) -> String -> IO Bool
runKat label act expected = do
  r <- act
  case r of
    Left err => check (label ++ " (FFI error: " ++ show err ++ ")") False
    Right d  =>
      let got = vectToHex d in
      if got == expected
         then check label True
         else do putStrLn ("    expected " ++ expected)
                 putStrLn ("    got      " ++ got)
                 check label False

||| Drive the production Merkle root path and contrast it with the XOR spec root.
||| If the combiner were still the XOR stub, `rootHashBytesIO` would equal
||| `rootHashBytes` (the pure XOR root); BLAKE3 makes them differ.
merkleChecks : IO (List Bool)
merkleChecks = do
  ea <- blake3 [97]   -- leaf "a"
  eb <- blake3 [98]   -- leaf "b"
  case (ea, eb) of
    (Right a, Right b) => do
      let tree = Node (Leaf a) (Leaf b)
      eRoot <- rootHashBytesIO tree
      eExp  <- hashPairBlake3 a b
      case (eRoot, eExp) of
        (Right root, Right expect) => do
          c1 <- check "rootHashBytesIO = BLAKE3(leafA ++ leafB)" (root == expect)
          c2 <- check "production root /= XOR spec root (real crypto, not stub)"
                      (not (root == rootHashBytes tree))
          pure [c1, c2]
        _ => do _ <- check "Merkle IO root (FFI error)" False
                pure [False]
    _ => do _ <- check "leaf hashing (FFI error)" False
            pure [False]

main : IO ()
main = do
  putStrLn "=== Ochrance Crypto FFI runtime test (Idris -> libochrance.so) ==="
  k1 <- runKat "BLAKE3(\"\")"     (blake3 [])       katBlake3Empty
  k2 <- runKat "BLAKE3(\"abc\")"   (blake3 abcBytes) katBlake3Abc
  k3 <- runKat "SHA-256(\"abc\")"  (sha256 abcBytes) katSha256Abc
  k4 <- runKat "SHA3-256(\"abc\")" (sha3_256 abcBytes) katSha3Abc
  ms <- merkleChecks
  let results = [k1, k2, k3, k4] ++ ms
  let passed = length (filter id results)
  let tot = length results
  putStrLn ""
  putStrLn (show passed ++ "/" ++ show tot ++ " checks passed")
  if passed == tot
     then putStrLn "=== Crypto FFI runtime test PASSED ==="
     else do putStrLn "=== Crypto FFI runtime test FAILED ==="
             exitFailure
