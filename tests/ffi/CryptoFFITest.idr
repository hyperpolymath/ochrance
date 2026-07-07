-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
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
import Ochrance.A2ML.Types
import Ochrance.A2ML.Validator

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

--------------------------------------------------------------------------------
-- Positive-path attestation: sign the canonical manifest bytes with the FFI
-- signer, then prove validateManifestIO accepts the manifest and rejects a
-- tampering of ANY signed field (timestamp / witness / ref digest) — i.e. the
-- canonical serialization actually binds those fields.
--------------------------------------------------------------------------------

signingSeed : Vect 32 Bits8
signingSeed = Data.Vect.replicate 32 0x42

||| A well-formed 64-hex toy digest built from one repeated character.
hex64 : Char -> String
hex64 c = pack (Data.List.replicate 64 c)

||| Manifest under test, parameterized over every field the tamper checks
||| flip. Policy requires a signature; maxAge is unset so validation is
||| clock-free.
testManifest : (sigHex : String) -> (pubkeyHex : String)
            -> (timestamp : String) -> (witness : String)
            -> (digest : String) -> Manifest
testManifest sigHex pkHex ts wit digest =
  MkManifest (MkManifestData "0.1.0" "filesystem" (Just ts))
             [MkRef "boot.img" (MkHash BLAKE3 digest)]
             (Just (MkAttestation wit sigHex pkHex))
             (Just (MkPolicy Attested Nothing True))

expectValid : Manifest -> IO Bool
expectValid m = do
  r <- validateManifestIO m
  case r of
    Right _ => pure True
    Left e  => do putStrLn ("    rejected: " ++ show e); pure False

expectSigFail : Manifest -> IO Bool
expectSigFail m = do
  r <- validateManifestIO m
  case r of
    Left SignatureVerificationFailed => pure True
    Left e  => do putStrLn ("    wrong error: " ++ show e); pure False
    Right _ => do putStrLn "    unexpectedly accepted"; pure False

signingChecks : IO (List Bool)
signingChecks = do
  ePk <- ed25519PublicKeyFromSeed signingSeed
  case ePk of
    Right (Just pk) => do
      let pkHex = vectToHex pk
      let ts = "2026-07-07T00:00:00Z"
      let wit = "witness-1"
      let digest = hex64 'a'
      -- The signature field is excluded from the signed bytes, so signing
      -- over a placeholder-signature manifest is the convention.
      let m0 = testManifest "" pkHex ts wit digest
      eHash <- blake3 (serializeForSigning m0)
      case eHash of
        Left _ => do _ <- check "blake3(signing bytes) (FFI error)" False
                     pure [False]
        Right h => do
          eSig <- ed25519Sign signingSeed (toList h)
          case eSig of
            Right (Just sig) => do
              let sigHex = vectToHex sig
              let m = testManifest sigHex pkHex ts wit digest
              c0 <- check "signature field excluded from signing bytes (convention)"
                          (serializeForSigning m == serializeForSigning m0)
              v  <- expectValid m
              c1 <- check "signed manifest accepted end-to-end (validateManifestIO)" v
              t1 <- expectSigFail (testManifest sigHex pkHex "2027-01-01T00:00:00Z" wit digest)
              c2 <- check "tampered timestamp rejected" t1
              t2 <- expectSigFail (testManifest sigHex pkHex ts "witness-EVIL" digest)
              c3 <- check "tampered witness rejected" t2
              t3 <- expectSigFail (testManifest sigHex pkHex ts wit (hex64 'b'))
              c4 <- check "tampered ref digest rejected" t3
              -- Boundary-shift regression: length prefixes keep distinct
              -- manifests from serializing to identical bytes.
              let mA = MkManifest (MkManifestData "ab" "c" Nothing) [] Nothing Nothing
              let mB = MkManifest (MkManifestData "a" "bc" Nothing) [] Nothing Nothing
              c5 <- check "field boundaries delimited (ab|c /= a|bc)"
                          (not (serializeForSigning mA == serializeForSigning mB))
              pure [c0, c1, c2, c3, c4, c5]
            _ => do _ <- check "ed25519Sign (FFI error)" False
                    pure [False]
    _ => do _ <- check "ed25519PublicKeyFromSeed (FFI error)" False
            pure [False]

main : IO ()
main = do
  putStrLn "=== Ochrance Crypto FFI runtime test (Idris -> libochrance.so) ==="
  k1 <- runKat "BLAKE3(\"\")"     (blake3 [])       katBlake3Empty
  k2 <- runKat "BLAKE3(\"abc\")"   (blake3 abcBytes) katBlake3Abc
  k3 <- runKat "SHA-256(\"abc\")"  (sha256 abcBytes) katSha256Abc
  k4 <- runKat "SHA3-256(\"abc\")" (sha3_256 abcBytes) katSha3Abc
  ms <- merkleChecks
  ss <- signingChecks
  let results = [k1, k2, k3, k4] ++ ms ++ ss
  let passed = length (filter id results)
  let tot = length results
  putStrLn ""
  putStrLn (show passed ++ "/" ++ show tot ++ " checks passed")
  if passed == tot
     then putStrLn "=== Crypto FFI runtime test PASSED ==="
     else do putStrLn "=== Crypto FFI runtime test FAILED ==="
             exitFailure
