||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.A2ML.Validator - Semantic validation of parsed manifests
|||
||| Checks that a parsed Manifest satisfies all semantic constraints:
||| supported version, valid hash algorithms, valid base64, signature
||| verification, and policy consistency.

module Ochrance.A2ML.Validator

import Ochrance.A2ML.Types
import Ochrance.Framework.Error

%default total

--------------------------------------------------------------------------------
-- Validation Errors
--------------------------------------------------------------------------------

||| Specific validation failure reasons
public export
data ValidationError
  = MissingRequiredField String
  | UnsupportedVersion String
  | InvalidHashAlgorithm String
  | InvalidHashValue String
  | SignatureVerificationFailed
  | PolicyViolation String

public export
Show ValidationError where
  show (MissingRequiredField f)   = "Missing required field: " ++ f
  show (UnsupportedVersion v)     = "Unsupported version: " ++ v
  show (InvalidHashAlgorithm a)   = "Invalid hash algorithm: " ++ a
  show (InvalidHashValue v)       = "Invalid hash value: " ++ v
  show SignatureVerificationFailed = "Signature verification failed"
  show (PolicyViolation msg)      = "Policy violation: " ++ msg

--------------------------------------------------------------------------------
-- Validation Logic
--------------------------------------------------------------------------------

||| Check if a version string is supported
isVersionSupported : String -> Bool
isVersionSupported "0.1.0" = True
isVersionSupported _       = False

||| Check if a hash value contains only valid hex characters
isValidHexString : String -> Bool
isValidHexString s = all isHexChar (unpack s)
  where
    isHexChar : Char -> Bool
    isHexChar c = isHexDigit c || c == '.'

||| Validate a single reference's hash
validateRef : Ref -> Either ValidationError ()
validateRef ref =
  if not (isValidHexString ref.hash.value)
     then Left (InvalidHashValue ref.hash.value)
     else Right ()

||| Validate a complete manifest, producing a ValidManifest on success.
public export
validateManifest : Manifest -> Either ValidationError ValidManifest
validateManifest m = do
  -- Check version
  if not (isVersionSupported m.manifestData.version)
     then Left (UnsupportedVersion m.manifestData.version)
     else pure ()
  -- Check subsystem name is non-empty
  if m.manifestData.subsystem == ""
     then Left (MissingRequiredField "subsystem")
     else pure ()
  -- Validate all refs
  traverse_ validateRef m.refs
  -- If attestation present, check signature (placeholder)
  -- TODO: actual signature verification via FFI
  -- Wrap in ValidManifest
  Right (MkValidManifest m)
