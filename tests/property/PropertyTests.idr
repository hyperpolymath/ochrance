-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| SPDX-License-Identifier: MPL-2.0
|||
||| PropertyTests - Systematic property-based tests for Ochrance
|||
||| Covers:
|||   1. Hex encoding roundtrip: hexDecode(hexEncode(bytes)) == bytes
|||   2. Merkle tree: insert -> verify inclusion -> succeed
|||   3. Repair idempotence: repair(repair(state)) == repair(state)
|||   4. A2ML validation: well-formed manifests always validate
|||
||| Author: Jonathan D.A. Jewell

module PropertyTests

import System
import Data.Vect
import Data.List
import Data.Bits
import Ochrance.Util.Hex
import Ochrance.FFI.Crypto
import Ochrance.A2ML.Types
import Ochrance.A2ML.Validator
import Ochrance.Framework.Error
import Ochrance.Framework.Proof
import Ochrance.Filesystem.Types
import Ochrance.Filesystem.Merkle
import Ochrance.Filesystem.Repair

%default total

--------------------------------------------------------------------------------
-- Test Infrastructure
--------------------------------------------------------------------------------

||| Test result tracking
record TestStats where
  constructor MkStats
  passed : Nat
  failed : Nat
  count  : Nat

initStats : TestStats
initStats = MkStats 0 0 0

addPass : TestStats -> TestStats
addPass s = { passed := S s.passed, count := S s.count } s

addFail : TestStats -> TestStats
addFail s = { failed := S s.failed, count := S s.count } s

||| Run a test case and return updated stats
testCase : String -> IO Bool -> TestStats -> IO TestStats
testCase name test stats = do
  result <- test
  if result
     then do putStrLn ("  PASS: " ++ name)
             pure (addPass stats)
     else do putStrLn ("  FAIL: " ++ name)
             pure (addFail stats)

--------------------------------------------------------------------------------
-- Sample byte vectors for systematic testing
--------------------------------------------------------------------------------

||| All-zero bytes
allZeros4 : List Bits8
allZeros4 = [0, 0, 0, 0]

||| All-0xFF bytes
allOnes4 : List Bits8
allOnes4 = [0xFF, 0xFF, 0xFF, 0xFF]

||| Ascending byte sequence
ascending8 : List Bits8
ascending8 = [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77]

||| Mixed nibble boundaries
nibbleBoundary : List Bits8
nibbleBoundary = [0x0F, 0xF0, 0x09, 0x90, 0xAB, 0xBA, 0xCD, 0xDC]

||| Single byte edge cases
singleBytes : List (List Bits8)
singleBytes = [[0x00], [0x01], [0x0F], [0x10], [0x7F], [0x80], [0xFE], [0xFF]]

||| Empty byte list
emptyBytes : List Bits8
emptyBytes = []

||| All 16 hex digits as individual bytes
hexDigitBytes : List Bits8
hexDigitBytes = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

--------------------------------------------------------------------------------
-- Property 1: Hex encoding roundtrip
-- hexDecode(hexEncode(bytes)) == bytes
--------------------------------------------------------------------------------

||| Check hex roundtrip for a list of bytes
hexRoundtrip : List Bits8 -> Bool
hexRoundtrip bytes =
  let encoded = bytesToHex bytes
  in case hexStringToBytes encoded of
       Nothing => False
       Just decoded => decoded == bytes

||| Check hex roundtrip for a Vect of bytes
hexRoundtripVect : {n : Nat} -> Vect n Bits8 -> Bool
hexRoundtripVect v =
  let encoded = vectToHex v
  in case hexStringToVect n encoded of
       Nothing => False
       Just decoded => toList decoded == toList v

||| Verify that hex encoding produces only valid hex characters
hexEncodingValid : List Bits8 -> Bool
hexEncodingValid bytes =
  let encoded = bytesToHex bytes
  in all isHexChar (unpack encoded)
  where
    isHexChar : Char -> Bool
    isHexChar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

||| Verify that hex encoding produces exactly 2*len characters
hexEncodingLength : List Bits8 -> Bool
hexEncodingLength bytes =
  let encoded = bytesToHex bytes
  in length (unpack encoded) == 2 * length bytes

hexTests : IO TestStats
hexTests = do
  putStrLn "\n=== Property 1: Hex Encoding Roundtrip ==="
  stats <- pure initStats

  -- Roundtrip tests across all sample vectors
  stats <- testCase "Roundtrip empty bytes" (pure (hexRoundtrip emptyBytes)) stats
  stats <- testCase "Roundtrip all zeros" (pure (hexRoundtrip allZeros4)) stats
  stats <- testCase "Roundtrip all 0xFF" (pure (hexRoundtrip allOnes4)) stats
  stats <- testCase "Roundtrip ascending" (pure (hexRoundtrip ascending8)) stats
  stats <- testCase "Roundtrip nibble boundary" (pure (hexRoundtrip nibbleBoundary)) stats
  stats <- testCase "Roundtrip hex digits" (pure (hexRoundtrip hexDigitBytes)) stats

  -- Individual byte roundtrips (edge cases)
  stats <- testCase "Roundtrip byte 0x00" (pure (hexRoundtrip [0x00])) stats
  stats <- testCase "Roundtrip byte 0xFF" (pure (hexRoundtrip [0xFF])) stats
  stats <- testCase "Roundtrip byte 0x0F" (pure (hexRoundtrip [0x0F])) stats
  stats <- testCase "Roundtrip byte 0xF0" (pure (hexRoundtrip [0xF0])) stats

  -- Vect roundtrip (4 bytes)
  stats <- testCase "Roundtrip Vect [0xDE, 0xAD, 0xBE, 0xEF]"
    (pure (hexRoundtripVect {n=4} [0xDE, 0xAD, 0xBE, 0xEF])) stats

  -- Vect roundtrip (32 bytes = hash size)
  stats <- testCase "Roundtrip Vect 32 zeros"
    (pure (hexRoundtripVect {n=32} (replicate 32 0))) stats
  stats <- testCase "Roundtrip Vect 32 0xFF"
    (pure (hexRoundtripVect {n=32} (replicate 32 0xFF))) stats

  -- Encoding validity
  stats <- testCase "Encoding valid chars (ascending)" (pure (hexEncodingValid ascending8)) stats
  stats <- testCase "Encoding valid chars (nibble)" (pure (hexEncodingValid nibbleBoundary)) stats

  -- Length property
  stats <- testCase "Encoding length (empty)" (pure (hexEncodingLength emptyBytes)) stats
  stats <- testCase "Encoding length (4 bytes)" (pure (hexEncodingLength allZeros4)) stats
  stats <- testCase "Encoding length (8 bytes)" (pure (hexEncodingLength ascending8)) stats

  -- Decode rejects invalid input
  stats <- testCase "Decode rejects odd length"
    (pure (case hexStringToBytes "abc" of Nothing => True; Just _ => False)) stats
  stats <- testCase "Decode rejects non-hex 'gg'"
    (pure (case hexStringToBytes "gg" of Nothing => True; Just _ => False)) stats
  stats <- testCase "Decode rejects spaces"
    (pure (case hexStringToBytes "ab cd" of Nothing => True; Just _ => False)) stats

  -- Case sensitivity: uppercase hex should also decode
  stats <- testCase "Decode uppercase hex 'ABCD'"
    (pure (case hexStringToBytes "ABCD" of Nothing => False; Just _ => True)) stats
  stats <- testCase "Decode mixed case 'aB0F'"
    (pure (case hexStringToBytes "aB0F" of Nothing => False; Just _ => True)) stats

  pure stats

--------------------------------------------------------------------------------
-- Property 2: Merkle tree - build and verify
--------------------------------------------------------------------------------

||| Build a 2-leaf Merkle tree and verify root is non-trivial
merkle2LeafNonTrivial : Bool
merkle2LeafNonTrivial =
  let h1 : HashBytes = replicate 32 1
      h2 : HashBytes = replicate 32 2
      tree = Node (Leaf h1) (Leaf h2)
      root = rootHashBytes tree
  in root /= h1 && root /= h2

||| Merkle tree: leaf of a single hash returns that hash
merkleSingleLeaf : Bool
merkleSingleLeaf =
  let h : HashBytes = replicate 32 42
      tree = Leaf h
  in rootHashBytes tree == h

||| Merkle tree: buildMerkleTree with 1 hash returns Leaf
merkleBuild1 : Bool
merkleBuild1 =
  let h : Vect 1 HashBytes = [replicate 32 7]
      tree = buildMerkleTree {n=0} h
  in rootHashBytes tree == replicate 32 7

||| Merkle tree: buildMerkleTree with 2 hashes has correct structure
merkleBuild2 : Bool
merkleBuild2 =
  let h1 : HashBytes = replicate 32 1
      h2 : HashBytes = replicate 32 2
      tree = buildMerkleTree {n=1} [h1, h2]
      expectedRoot = xorCombiner h1 h2
  in rootHashBytes tree == expectedRoot

||| Merkle proof: simple inclusion proof verification (single leaf)
merkleProofSingleLeaf : Bool
merkleProofSingleLeaf =
  let h : HashBytes = replicate 32 5
  in verifyProof h h []

||| Merkle proof: 2-leaf tree, prove left leaf inclusion
merkleProofLeftLeaf : Bool
merkleProofLeftLeaf =
  let h1 : HashBytes = replicate 32 1
      h2 : HashBytes = replicate 32 2
      root = xorCombiner h1 h2
      prf : MerkleProof = [(GoLeft, h2)]
  in verifyProof root h1 prf

||| Merkle proof: 2-leaf tree, prove right leaf inclusion
merkleProofRightLeaf : Bool
merkleProofRightLeaf =
  let h1 : HashBytes = replicate 32 1
      h2 : HashBytes = replicate 32 2
      root = xorCombiner h1 h2
      prf : MerkleProof = [(GoRight, h1)]
  in verifyProof root h2 prf

||| Merkle proof: wrong sibling should fail
merkleProofWrongSibling : Bool
merkleProofWrongSibling =
  let h1 : HashBytes = replicate 32 1
      h2 : HashBytes = replicate 32 2
      h3 : HashBytes = replicate 32 3
      root = xorCombiner h1 h2
      prf : MerkleProof = [(GoLeft, h3)]  -- h3 != h2
  in not (verifyProof root h1 prf)

merkleTests : IO TestStats
merkleTests = do
  putStrLn "\n=== Property 2: Merkle Tree Properties ==="
  stats <- pure initStats

  stats <- testCase "Single leaf returns its hash" (pure merkleSingleLeaf) stats
  stats <- testCase "2-leaf tree root != either leaf" (pure merkle2LeafNonTrivial) stats
  stats <- testCase "buildMerkleTree 1 hash" (pure merkleBuild1) stats
  stats <- testCase "buildMerkleTree 2 hashes" (pure merkleBuild2) stats
  stats <- testCase "Proof: single leaf (empty proof)" (pure merkleProofSingleLeaf) stats
  stats <- testCase "Proof: left leaf inclusion" (pure merkleProofLeftLeaf) stats
  stats <- testCase "Proof: right leaf inclusion" (pure merkleProofRightLeaf) stats
  stats <- testCase "Proof: wrong sibling fails" (pure merkleProofWrongSibling) stats

  -- 4-leaf tree property
  stats <- testCase "4-leaf tree buildMerkleTree consistency" (pure merkle4Leaf) stats

  pure stats
  where
    merkle4Leaf : Bool
    merkle4Leaf =
      let h1 : HashBytes = replicate 32 1
          h2 : HashBytes = replicate 32 2
          h3 : HashBytes = replicate 32 3
          h4 : HashBytes = replicate 32 4
          tree = buildMerkleTree {n=2} [h1, h2, h3, h4]
          expectedLeft = xorCombiner h1 h2
          expectedRight = xorCombiner h3 h4
          expectedRoot = xorCombiner expectedLeft expectedRight
      in rootHashBytes tree == expectedRoot

--------------------------------------------------------------------------------
-- Property 3: Repair idempotence
-- repair(repair(state)) == repair(state)
--------------------------------------------------------------------------------

||| Create a test filesystem state with known hashes
mkTestFS : Nat -> (BlockIndex -> Maybe Hash) -> FSState
mkTestFS n hashFn = MkFSState n hashFn (MkManifestData "0.1.0" "test" Nothing)

||| Check if two filesystem states have the same block hashes
||| for all blocks in range [0, numBlocks)
fsStatesEqual : FSState -> FSState -> Bool
fsStatesEqual fs1 fs2 =
  fs1.numBlocks == fs2.numBlocks &&
  all (\idx => fs1.blockHash idx == fs2.blockHash idx) [0 .. (fs1.numBlocks `minus` 1)]

repairTests : IO TestStats
repairTests = do
  putStrLn "\n=== Property 3: Repair Idempotence ==="
  stats <- pure initStats

  -- Repair idempotence: repairing an already-correct state is a no-op
  let expectedHash = MkHash BLAKE3 "abcd1234"
  let correctFS = mkTestFS 2 (\idx => if idx == 0 then Just expectedHash else Nothing)

  result1 <- repairBlock correctFS 0 expectedHash
  let check1 = case result1 of
                  Left _ => False
                  Right fs1 => fs1.blockHash 0 == Just expectedHash
  stats <- testCase "Repair already-correct block is idempotent" (pure check1) stats

  -- Double repair: repair(repair(state)) == repair(state)
  let wrongFS = mkTestFS 3 (\_ => Just (MkHash BLAKE3 "wrong"))
  let target0 = MkHash BLAKE3 "correct0"
  let target1 = MkHash BLAKE3 "correct1"

  result2 <- repairBlock wrongFS 0 target0
  check2 <- case result2 of
    Left _ => pure False
    Right fs1 => do
      result3 <- repairBlock fs1 0 target0
      case result3 of
        Left _ => pure False
        Right fs2 => pure (fs2.blockHash 0 == Just target0)
  stats <- testCase "Double repair block 0" (pure check2) stats

  -- Batch repair idempotence
  let repairs = [(0, target0), (1, target1)]
  result4 <- repairBlocks (mkTestFS 3 (\_ => Just (MkHash BLAKE3 "wrong"))) repairs
  check4 <- case result4 of
    Left _ => pure False
    Right fs1 => do
      result5 <- repairBlocks fs1 repairs
      case result5 of
        Left _ => pure False
        Right fs2 => pure (fsStatesEqual fs1 fs2)
  stats <- testCase "Batch repair then re-repair is idempotent" (pure check4) stats

  -- Repair out-of-range index returns error
  let smallFS = mkTestFS 2 (\_ => Nothing)
  result6 <- repairBlock smallFS 999 (MkHash BLAKE3 "x")
  let check6 = case result6 of
                  Left (QError _) => True
                  _ => False
  stats <- testCase "Repair out-of-range index returns QError" (pure check6) stats

  -- Snapshot repair idempotence
  let snap = MkFSSnapshot (MkHash BLAKE3 "root") 2
        [MkRef "block_0" (MkHash BLAKE3 "snap0"), MkRef "block_1" (MkHash BLAKE3 "snap1")]
  result7 <- repairFromSnapshot (mkTestFS 2 (\_ => Nothing)) snap
  check7 <- case result7 of
    Left _ => pure False
    Right fs1 => do
      result8 <- repairFromSnapshot fs1 snap
      case result8 of
        Left _ => pure False
        Right fs2 => pure (fsStatesEqual fs1 fs2)
  stats <- testCase "Snapshot repair then re-snapshot-repair is idempotent" (pure check7) stats

  -- Block count mismatch returns error
  let wrongSnap = MkFSSnapshot (MkHash BLAKE3 "root") 5 []
  result9 <- repairFromSnapshot (mkTestFS 2 (\_ => Nothing)) wrongSnap
  let check9 = case result9 of
                  Left (QError _) => True
                  _ => False
  stats <- testCase "Snapshot block count mismatch returns QError" (pure check9) stats

  pure stats

--------------------------------------------------------------------------------
-- Property 4: A2ML Manifest Validation
--------------------------------------------------------------------------------

||| Well-formed manifest with valid hex hashes
mkValidManifestSample : Manifest
mkValidManifestSample = MkManifest
  (MkManifestData "0.1.0" "test-filesystem" Nothing)
  [MkRef "block_0" (MkHash BLAKE3 "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")]
  Nothing
  Nothing

||| Manifest with empty subsystem (should fail)
mkEmptySubsystem : Manifest
mkEmptySubsystem = MkManifest
  (MkManifestData "0.1.0" "" Nothing)
  []
  Nothing
  Nothing

||| Manifest with invalid version (should fail)
mkBadVersion : Manifest
mkBadVersion = MkManifest
  (MkManifestData "99.99.99" "test" Nothing)
  []
  Nothing
  Nothing

||| Manifest with invalid hex in hash (should fail)
mkBadHash : Manifest
mkBadHash = MkManifest
  (MkManifestData "0.1.0" "test" Nothing)
  [MkRef "block_0" (MkHash BLAKE3 "not_valid_hex!")]
  Nothing
  Nothing

||| Manifest with policy requiring sig but no attestation (should fail policy)
mkPolicyViolation : Manifest
mkPolicyViolation = MkManifest
  (MkManifestData "0.1.0" "test" Nothing)
  [MkRef "block_0" (MkHash BLAKE3 "abcdef")]
  Nothing
  (Just (MkPolicy Attested Nothing True))

validationTests : IO TestStats
validationTests = do
  putStrLn "\n=== Property 4: A2ML Manifest Validation ==="
  stats <- pure initStats

  -- Valid manifest passes validation
  let c1 = case validateManifest mkValidManifestSample of
              Right _ => True
              Left _ => False
  stats <- testCase "Well-formed manifest validates" (pure c1) stats

  -- Empty subsystem fails
  let c2 = case validateManifest mkEmptySubsystem of
              Left _ => True
              Right _ => False
  stats <- testCase "Empty subsystem rejects" (pure c2) stats

  -- Bad version fails
  let c3 = case validateManifest mkBadVersion of
              Left (UnsupportedVersion _) => True
              _ => False
  stats <- testCase "Unsupported version rejects" (pure c3) stats

  -- Bad hash value fails
  let c4 = case validateManifest mkBadHash of
              Left (InvalidHashValue _) => True
              _ => False
  stats <- testCase "Invalid hex hash rejects" (pure c4) stats

  -- Policy violation: require_sig without attestation
  let c5 = case validatePolicy mkPolicyViolation of
              Left (PolicyViolation _) => True
              _ => False
  stats <- testCase "Policy require_sig without attestation fails" (pure c5) stats

  -- Policy with max_age but no timestamp
  let mNoTs = MkManifest
        (MkManifestData "0.1.0" "test" Nothing)
        [] Nothing
        (Just (MkPolicy Lax (Just 3600) False))
  let c6 = case validatePolicy mNoTs of
              Left (PolicyViolation _) => True
              _ => False
  stats <- testCase "Policy max_age without timestamp fails" (pure c6) stats

  -- Manifest with zero refs still validates
  let c7 = case validateManifest (MkManifest (MkManifestData "0.1.0" "test" Nothing) [] Nothing Nothing) of
              Right _ => True
              Left _ => False
  stats <- testCase "Manifest with zero refs validates" (pure c7) stats

  -- Manifest with timestamp validates
  let mTs = MkManifest
        (MkManifestData "0.1.0" "test" (Just "2026-03-10T00:00:00Z"))
        [MkRef "block_0" (MkHash BLAKE3 "abcdef0123456789")]
        Nothing Nothing
  let c8 = case validateManifest mTs of
              Right _ => True
              Left _ => False
  stats <- testCase "Manifest with timestamp validates" (pure c8) stats

  -- Multiple refs all validated
  let mMulti = MkManifest
        (MkManifestData "0.1.0" "test" Nothing)
        [ MkRef "a" (MkHash SHA256 "abcdef")
        , MkRef "b" (MkHash SHA3_256 "012345")
        , MkRef "c" (MkHash BLAKE3 "fedcba")
        ]
        Nothing Nothing
  let c9 = case validateManifest mMulti of
              Right _ => True
              Left _ => False
  stats <- testCase "Multiple valid refs all pass" (pure c9) stats

  pure stats

--------------------------------------------------------------------------------
-- Main Test Runner
--------------------------------------------------------------------------------

covering
export
main : IO ()
main = do
  putStrLn "==========================================="
  putStrLn "Ochrance Property-Based Test Suite"
  putStrLn "==========================================="

  hexStats <- hexTests
  merkleStats <- merkleTests
  repairStats <- repairTests
  validStats <- validationTests

  let totalPassed = hexStats.passed + merkleStats.passed
                  + repairStats.passed + validStats.passed
  let totalFailed = hexStats.failed + merkleStats.failed
                  + repairStats.failed + validStats.failed
  let totalCount = hexStats.count + merkleStats.count
                 + repairStats.count + validStats.count

  putStrLn "\n==========================================="
  putStrLn ("Property Tests: " ++ show totalPassed ++ "/"
         ++ show totalCount ++ " passed, "
         ++ show totalFailed ++ " failed")
  putStrLn "==========================================="

  -- Gate: nonzero exit on any failure, so CI actually enforces this suite
  when (totalFailed > 0) exitFailure
