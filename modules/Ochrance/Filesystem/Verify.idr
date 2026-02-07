||| SPDX-License-Identifier: PMPL-1.0-or-later
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

||| Hash a single block using BLAKE3
export
hashBlock : HasIO io => Block -> io Hash
hashBlock block = do
  hashBytes <- blake3 (blockToBytes block)
  pure (MkHash BLAKE3 (bytesToHex hashBytes))
  where
    bytesToHex : Vect 32 Bits8 -> String
    bytesToHex bytes = concat (map byteToHex (toList bytes))

    byteToHex : Bits8 -> String
    byteToHex b = let n = cast {to=Nat} b
                  in padLeft 2 '0' (asHex n)

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

           -- Build appropriate proof
           case mode of
             Lax => pure (Right (LaxProof validManifest))
             Attested =>
               case manifest.attestation of
                 Just att =>
                   -- Extract root hash from first ref (placeholder)
                   case head' manifest.refs of
                     Nothing => pure (Left (QError (MissingRequiredField "refs")))
                     Just ref => pure (Right (AttestedProof validManifest ref.hash att.signature))
                 Nothing => pure (Right (LaxProof validManifest))
             _ => pure (Right (CheckedProof validManifest (MkHash BLAKE3 "placeholder")))
  where
    head' : List a -> Maybe a
    head' [] = Nothing
    head' (x :: _) = Just x

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
    parseBlockIndex : String -> Maybe BlockIndex
    parseBlockIndex name =
      case words name of
        ["block", numStr] => parseNat numStr
        _ => Nothing

    parseNat : String -> Maybe Nat
    parseNat s = case all isDigit (unpack s) of
      False => Nothing
      True => Just (cast s)

    words : String -> List String
    words s = filter (not . null) (split (== '_') s)

    split : (Char -> Bool) -> String -> List String
    split p s = splitHelper p (unpack s) [] []
      where
        splitHelper : (Char -> Bool) -> List Char -> List Char -> List String -> List String
        splitHelper p [] acc res = reverse (pack (reverse acc) :: res)
        splitHelper p (c :: cs) acc res =
          if p c
             then splitHelper p cs [] (pack (reverse acc) :: res)
             else splitHelper p cs (c :: acc) res

--------------------------------------------------------------------------------
-- VerifiedSubsystem Instance
--------------------------------------------------------------------------------

||| FSState implements VerifiedSubsystem
export
implementation VerifiedSubsystem FSState where
  subsystemName = "filesystem"
  generateManifest = generateManifest
  verify = verify
  repair = \fs, manifest => do
    result <- linearVerifyAndRepair fs manifest
    case result of
      Left err => pure (Left err)
      Right (newState, _proof) => pure (Right newState)
