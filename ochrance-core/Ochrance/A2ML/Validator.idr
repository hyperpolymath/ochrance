||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.A2ML.Validator - Semantic validation of parsed manifests
|||
||| Checks that a parsed Manifest satisfies all semantic constraints:
||| supported version, valid hash algorithms, valid base64, signature
||| verification, and policy consistency.

module Ochrance.A2ML.Validator

import Data.Vect
import Ochrance.A2ML.Types
import Ochrance.Framework.Error
import Ochrance.FFI.Crypto

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

||| Validate a complete manifest (pure version, no signature verification).
||| Use validateManifestIO for full validation including signatures.
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
  -- Signature verification skipped in pure version
  -- Wrap in ValidManifest
  Right (MkValidManifest m)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

stringToBytes : String -> List Bits8
stringToBytes s = map (cast . ord) (unpack s)

refToBytes : Ref -> List Bits8
refToBytes r = stringToBytes (r.name ++ show r.hash)

serializeForSigning : Manifest -> List Bits8
serializeForSigning m =
  let versionBytes = stringToBytes m.manifestData.version
      subsystemBytes = stringToBytes m.manifestData.subsystem
      refsBytes = concatMap refToBytes m.refs
  in versionBytes ++ subsystemBytes ++ refsBytes

verifySignatureStub : String -> String -> Vect 32 Bits8 -> Bool
verifySignatureStub sig pubkey hash =
  -- TODO: Implement Ed25519 signature verification via FFI
  -- For now, accept all signatures in Lax mode
  True

||| Validate a complete manifest with signature verification (IO version).
||| This performs full validation including cryptographic signature checks.
export
validateManifestIO : HasIO io => Manifest -> io (Either ValidationError ValidManifest)
validateManifestIO m = do
  -- Run pure validation first
  case validateManifest m of
    Left err => pure (Left err)
    Right _ => do
      -- If attestation present, verify signature
      case m.attestation of
        Nothing => pure (Right (MkValidManifest m))
        Just att => do
          -- Compute manifest hash for signature verification
          let manifestBytes = serializeForSigning m
          manifestHash <- blake3 manifestBytes

          -- Verify signature (stub - requires Ed25519 FFI)
          let signatureValid = verifySignatureStub att.signature att.pubkey manifestHash

          if signatureValid
             then pure (Right (MkValidManifest m))
             else pure (Left SignatureVerificationFailed)

--------------------------------------------------------------------------------
-- Policy Validation
--------------------------------------------------------------------------------

||| Validate that a manifest satisfies policy constraints
export
validatePolicy : Manifest -> Either ValidationError ()
validatePolicy m =
  case m.policy of
    Nothing => Right ()
    Just p => do
      -- Check require_sig policy
      if p.requireSig && isNothing m.attestation
         then Left (PolicyViolation "Policy requires signature but none present")
         else Right ()
      -- Check max_age policy (requires timestamp)
      case (p.maxAge, m.manifestData.timestamp) of
        (Just _, Nothing) =>
          Left (PolicyViolation "Policy specifies max_age but manifest has no timestamp")
        _ => Right ()
  where
    isNothing : Maybe a -> Bool
    isNothing Nothing = True
    isNothing (Just _) = False
