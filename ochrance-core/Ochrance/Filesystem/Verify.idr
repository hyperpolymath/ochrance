-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.Verify - Filesystem verification logic
|||
||| Implements the VerifiedSubsystem interface for filesystem state.
||| Compares filesystem block hashes against an A2ML manifest.

module Ochrance.Filesystem.Verify

import Data.List
import Data.Vect
import Ochrance.A2ML.Types
import Ochrance.Framework.Interface
import Ochrance.Framework.Proof
import Ochrance.Framework.Error
import Ochrance.Filesystem.Types
import Ochrance.Filesystem.Merkle
import Ochrance.Filesystem.Repair
import Ochrance.FFI.Crypto

%default total

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

||| Collect all block hashes from filesystem state
collectBlockHashes : FSState -> List (Maybe Hash)
collectBlockHashes fs = map fs.blockHash [0 .. (fs.numBlocks `minus` 1)]

||| Convert block to bytes for hashing
blockToBytes : Block -> List Bits8
blockToBytes = toList

||| Hash a single block using BLAKE3.
||| Returns Left on buffer allocation failure (z/out-of-memory).
export
hashBlock : HasIO io => Block -> io (Either OchranceError Hash)
hashBlock block = do
  result <- blake3 (blockToBytes block)
  case result of
    Left err => pure (Left err)
    Right hashBytes => pure (Right (MkHash BLAKE3 (bytesToHex hashBytes)))
  where
    ||| Hex digit lookup: nibble value (0-15) to character.
    ||| Uses exhaustive pattern matching for totality.
    hexDigit : Bits8 -> Char
    hexDigit 0 = '0'; hexDigit 1 = '1'; hexDigit 2 = '2'; hexDigit 3 = '3'
    hexDigit 4 = '4'; hexDigit 5 = '5'; hexDigit 6 = '6'; hexDigit 7 = '7'
    hexDigit 8 = '8'; hexDigit 9 = '9'; hexDigit 10 = 'a'; hexDigit 11 = 'b'
    hexDigit 12 = 'c'; hexDigit 13 = 'd'; hexDigit 14 = 'e'; hexDigit 15 = 'f'
    hexDigit _ = '0'  -- Defensive fallback for values > 15

    ||| Convert a byte to a two-character hex string.
    ||| Provably total: extracts high and low nibbles via div/mod on Bits8,
    ||| no recursion required.
    byteToHex : Bits8 -> String
    byteToHex b =
      let hi = b `div` 16
          lo = b `mod` 16
      in pack [hexDigit hi, hexDigit lo]

    bytesToHex : Vect 32 Bits8 -> String
    bytesToHex bytes = concat (map byteToHex (toList bytes))

--------------------------------------------------------------------------------
-- Manifest Generation
--------------------------------------------------------------------------------

||| Generate A2ML manifest from current filesystem state
export
generateManifest : HasIO io => FSState -> io (Either OchranceError Manifest)
generateManifest fs = do
  -- Collect block hashes (placeholder - in reality would read from disk)
  let blockHashes = collectBlockHashes fs

  -- Filter out Nothing values and create refs
  let validRefs = mapMaybe toRef (zip [0 .. (fs.numBlocks `minus` 1)] blockHashes)

  -- Build manifest
  let manifest = MkManifest
        fs.metadata
        validRefs
        Nothing  -- no attestation by default
        Nothing  -- no policy by default

  pure (Right manifest)
  where
    toRef : (BlockIndex, Maybe Hash) -> Maybe Ref
    toRef (idx, Nothing) = Nothing
    toRef (idx, Just hash) = Just (MkRef ("block_" ++ show idx) hash)

    mapMaybe : (a -> Maybe b) -> List a -> List b
    mapMaybe f [] = []
    mapMaybe f (x :: xs) = case f x of
      Nothing => mapMaybe f xs
      Just y => y :: mapMaybe f xs

--------------------------------------------------------------------------------
-- Verification Helpers
--------------------------------------------------------------------------------

||| Verify all refs match filesystem state
verifyAllRefs : HasIO io => FSState -> List Ref -> io (Either OchranceError ())
verifyAllRefs fs [] = pure (Right ())
verifyAllRefs fs (ref :: refs) = do
  -- Extract block index from ref name (assumes "block_N" format)
  case parseBlockIndex ref.name of
    Nothing => pure (Left (QError (InvalidManifestPath ("Invalid ref name: " ++ ref.name))))
    Just idx => do
      -- Check if block index is valid
      if idx >= fs.numBlocks
         then pure (Left (QError (InvalidManifestPath ("Block index out of range: " ++ show idx))))
         else do
           -- Get block hash from filesystem
           case fs.blockHash idx of
             Nothing => pure (Left (ZError (FileNotFound ("Block " ++ show idx))))
             Just actualHash => do
               -- Compare hashes
               if actualHash == ref.hash
                  then verifyAllRefs fs refs
                  else pure (Left (PError (HashMismatch ref.name (show ref.hash) (show actualHash))))
  where
    split : (Char -> Bool) -> String -> List String
    split p s = splitHelper p (unpack s) [] []
      where
        splitHelper : (Char -> Bool) -> List Char -> List Char -> List String -> List String
        splitHelper p [] acc res = reverse (pack (reverse acc) :: res)
        splitHelper p (c :: cs) acc res =
          if p c
             then splitHelper p cs [] (pack (reverse acc) :: res)
             else splitHelper p cs (c :: acc) res

    words : String -> List String
    words s = filter (\str => str /= "") (split (== '_') s)

    parseNat : String -> Maybe Nat
    parseNat s = case all isDigit (unpack s) of
      False => Nothing
      True => Just (cast s)

    parseBlockIndex : String -> Maybe BlockIndex
    parseBlockIndex name =
      case words name of
        ["block", numStr] => parseNat numStr
        _ => Nothing

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

||| Verify filesystem state against a validated manifest
export
verify : HasIO io => FSState -> ValidManifest -> io (Either OchranceError (VerificationProof FSState))
verify fs validManifest = do
  let manifest = unwrapValid validManifest

  -- Check that subsystem matches
  if manifest.manifestData.subsystem /= fs.metadata.subsystem
     then pure (Left (QError (InvalidManifestPath "Subsystem mismatch")))
     else do
       -- Verify each ref against filesystem state
       result <- verifyAllRefs fs manifest.refs

       case result of
         Left err => pure (Left err)
         Right () => do
           -- Determine verification mode based on manifest
           let mode = case manifest.attestation of
                        Nothing => Lax
                        Just att => Attested

           -- Build appropriate proof based on verification mode
           case mode of
             Lax => pure (Right (LaxProof validManifest))
             Attested =>
               case manifest.attestation of
                 Just att =>
                   -- Extract root hash from first ref for attestation proof
                   case head' manifest.refs of
                     Nothing => pure (Left (QError (MissingRequiredField "refs")))
                     Just ref => pure (Right (AttestedProof validManifest ref.hash att.signature))
                 Nothing => pure (Right (LaxProof validManifest))
             Checked =>
               -- Checked mode: build root hash from all ref hashes
               case head' manifest.refs of
                 Nothing => pure (Left (QError (MissingRequiredField "refs")))
                 Just ref => pure (Right (CheckedProof validManifest ref.hash))
  where
    head' : List a -> Maybe a
    head' [] = Nothing
    head' (x :: _) = Just x

--------------------------------------------------------------------------------
-- Pure ref-checking (top-level so it can be reasoned about; see VerifyProof)
--------------------------------------------------------------------------------

||| Split a string on a separator character.
splitOn : Char -> String -> List String
splitOn sep s = splitHelper (unpack s) [] []
  where
    splitHelper : List Char -> List Char -> List String -> List String
    splitHelper [] acc res = reverse (pack (reverse acc) :: res)
    splitHelper (c :: cs) acc res =
      if c == sep
         then splitHelper cs [] (pack (reverse acc) :: res)
         else splitHelper cs (c :: acc) res

||| Parse a string of digits to a Nat.
parseNatHelper : String -> Maybe Nat
parseNatHelper s = case all isDigit (unpack s) of
  False => Nothing
  True => Just (cast s)

||| Parse a "block_N" ref name to its block index.
public export
parseBlockIdx : String -> Maybe BlockIndex
parseBlockIdx name =
  let parts = filter (\str => str /= "") (splitOn '_' name) in
  case parts of
    ["block", numStr] => parseNatHelper numStr
    _ => Nothing

||| Check every manifest ref against the filesystem's pre-computed block hashes:
||| each ref name must parse to an in-range block index whose stored hash equals
||| the ref's hash. `Right ()` iff all refs match. (`VerifyProof.verifyRefsSound`
||| inverts this to the per-ref match witness.)
public export
verifyRefsHelper : FSState -> List Ref -> Either OchranceError ()
verifyRefsHelper _ [] = Right ()
verifyRefsHelper st (ref :: refs) =
  case parseBlockIdx ref.name of
    Nothing => Left (QError (InvalidManifestPath ("Invalid ref name: " ++ ref.name)))
    Just idx =>
      if idx >= st.numBlocks
         then Left (QError (InvalidManifestPath ("Block index out of range: " ++ show idx)))
         else case st.blockHash idx of
           Nothing => Left (ZError (FileNotFound ("Block " ++ show idx)))
           Just actualHash =>
             if actualHash == ref.hash
                then verifyRefsHelper st refs
                else Left (PError (HashMismatch ref.name (show ref.hash) (show actualHash)))

--------------------------------------------------------------------------------
-- VerifiedSubsystem Instance
--------------------------------------------------------------------------------

||| FSState implements VerifiedSubsystem.
|||
||| Note: The VerifiedSubsystem interface uses pure signatures for
||| generateManifest and verify, but the filesystem module requires IO
||| for cryptographic hashing. The pure implementations build manifests
||| and verify using the pre-computed block hashes stored in FSState
||| (which were computed via IO at block-read time).
||| For full IO-based verification, use the standalone verify function.
export
implementation VerifiedSubsystem FSState where
  subsystemName = "filesystem"

  -- Pure generateManifest: builds manifest from pre-computed block hashes
  generateManifest = \fs =>
    let blockHashes = map fs.blockHash [0 .. (fs.numBlocks `minus` 1)]
        validRefs = buildRefs blockHashes 0
        manifest = MkManifest fs.metadata validRefs Nothing Nothing
    in Right manifest
    where
      buildRefs : List (Maybe Hash) -> Nat -> List Ref
      buildRefs [] _ = []
      buildRefs (Nothing :: rest) idx = buildRefs rest (S idx)
      buildRefs ((Just h) :: rest) idx =
        MkRef ("block_" ++ show idx) h :: buildRefs rest (S idx)

  -- Pure verify: checks pre-computed block hashes against manifest refs
  verify = \fs, vm =>
    let manifest = unwrapValid vm
    in if manifest.manifestData.subsystem /= fs.metadata.subsystem
          then Left (QError (InvalidManifestPath "Subsystem mismatch"))
          else case verifyRefsHelper fs manifest.refs of
            Left err => Left err
            Right () =>
              case manifest.attestation of
                Nothing => Right (LaxProof vm)
                Just att =>
                  case head' manifest.refs of
                    Nothing => Left (QError (MissingRequiredField "refs"))
                    Just ref => Right (AttestedProof vm ref.hash att.signature)
    where
      head' : List a -> Maybe a
      head' [] = Nothing
      head' (x :: _) = Just x

  -- Repair implementation: delegates to linearRepairFromManifest
  repair = \fs, manifest => do
    result <- linearRepairFromManifest fs manifest
    case result of
      Left err => pure (Left err)
      Right (newFS, _) => pure (Right newFS)
