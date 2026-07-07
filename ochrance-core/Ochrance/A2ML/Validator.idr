-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.A2ML.Validator - Semantic validation of parsed manifests
|||
||| Checks that a parsed Manifest satisfies all semantic constraints:
||| supported version, valid hash algorithms, well-formed digests,
||| signature verification, and policy consistency (require_sig and
||| max_age freshness).

module Ochrance.A2ML.Validator

import Data.Vect
import Data.Maybe
import System
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
public export
isVersionSupported : String -> Bool
isVersionSupported "0.1.0" = True
isVersionSupported _       = False

||| Length, in hex characters, of a supported digest. All supported hash
||| algorithms (BLAKE3, SHA-256, SHA3-256) produce 32-byte digests,
||| i.e. exactly 64 hex characters.
public export
digestHexLength : Nat
digestHexLength = 64

||| Check that a string consists of exactly `n` hex digits ([0-9a-fA-F]).
||| Rejects the empty string whenever `n` is non-zero.
public export
isValidHexStringN : (n : Nat) -> String -> Bool
isValidHexStringN n s =
  let cs = unpack s
  in length cs == n && all isHexDigit cs

||| Check if a hash value is a well-formed digest: exactly 64 hex characters.
||| (An earlier version accepted '.' and imposed no length bound, so empty,
||| truncated or padded "digests" passed validation.)
public export
isValidHexString : String -> Bool
isValidHexString s = isValidHexStringN digestHexLength s

||| Validate a single reference's hash
public export
validateRef : Ref -> Either ValidationError ()
validateRef ref =
  if not (isValidHexString ref.hash.value)
     then Left (InvalidHashValue ref.hash.value)
     else Right ()

--------------------------------------------------------------------------------
-- Timestamp Parsing (minimal total ISO-8601 subset)
--------------------------------------------------------------------------------

||| Decimal value of a digit character. Public so proofs and tests can
||| reduce `parseTimestamp` definitionally.
public export
digitVal : Char -> Maybe Integer
digitVal c =
  if isDigit c
     then Just (cast (ord c - ord '0'))
     else Nothing

||| Read a fixed run of decimal digits as a non-negative Integer.
public export
readDigits : Integer -> List Char -> Maybe Integer
readDigits acc []        = Just acc
readDigits acc (c :: cs) = case digitVal c of
  Nothing => Nothing
  Just d  => readDigits (acc * 10 + d) cs

||| Gregorian leap-year test.
public export
isLeapYear : Integer -> Bool
isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0

||| Days in a month (1-12). Defensively 31 outside that range; callers
||| range-check the month first.
public export
daysInMonth : (leap : Bool) -> (month : Integer) -> Integer
daysInMonth leap 2  = if leap then 29 else 28
daysInMonth _    4  = 30
daysInMonth _    6  = 30
daysInMonth _    9  = 30
daysInMonth _    11 = 30
daysInMonth _    _  = 31

||| Days before the first day of the given month (1-12) within one year.
public export
daysBeforeMonth : (leap : Bool) -> (month : Integer) -> Integer
daysBeforeMonth leap m =
  let base : Integer = case m of
        1 => 0;   2 => 31;  3 => 59;   4 => 90
        5 => 120; 6 => 151; 7 => 181;  8 => 212
        9 => 243; 10 => 273; 11 => 304; _ => 334
  in if leap && m > 2 then base + 1 else base

||| Leap years strictly before the given year (counting from year 1;
||| positive years only - truncating and floor division agree there).
public export
leapsBefore : Integer -> Integer
leapsBefore y =
  let p = y - 1
  in p `div` 4 - p `div` 100 + p `div` 400

||| Days from 1970-01-01 to January 1st of the given year (year >= 1970).
public export
daysSinceEpochToYear : Integer -> Integer
daysSinceEpochToYear y = 365 * (y - 1970) + (leapsBefore y - leapsBefore 1970)

||| Parse a manifest timestamp - the minimal total ISO-8601 subset
||| YYYY-MM-DDTHH:MM:SSZ (UTC only, the format A2ML manifests carry) -
||| into seconds since the Unix epoch. Rejects out-of-range date/time
||| fields and years before 1970.
public export
parseTimestamp : String -> Maybe Integer
parseTimestamp s = case unpack s of
  [y1,y2,y3,y4,'-',mo1,mo2,'-',d1,d2,'T',h1,h2,':',mi1,mi2,':',se1,se2,'Z'] => do
    year   <- readDigits 0 [y1,y2,y3,y4]
    month  <- readDigits 0 [mo1,mo2]
    day    <- readDigits 0 [d1,d2]
    hour   <- readDigits 0 [h1,h2]
    minute <- readDigits 0 [mi1,mi2]
    second <- readDigits 0 [se1,se2]
    let leap = isLeapYear year
    if year >= 1970
       && month >= 1 && month <= 12
       && day >= 1 && day <= daysInMonth leap month
       && hour <= 23 && minute <= 59 && second <= 59
       then let days = daysSinceEpochToYear year
                     + daysBeforeMonth leap month
                     + (day - 1)
            in Just (((days * 24 + hour) * 60 + minute) * 60 + second)
       else Nothing
  _ => Nothing

--------------------------------------------------------------------------------
-- Policy Validation
--------------------------------------------------------------------------------

||| require_sig: a policy that requires a signature is violated by a
||| manifest that carries no attestation.
public export
checkRequireSig : Policy -> Maybe Attestation -> Either ValidationError ()
checkRequireSig p Nothing =
  if p.requireSig
     then Left (PolicyViolation "Policy requires signature but none present")
     else Right ()
checkRequireSig _ (Just _) = Right ()

||| max_age: the constraint is only meaningful if the manifest carries a
||| timestamp in the supported ISO-8601 subset. The freshness comparison
||| itself needs a clock - see validatePolicyAt.
public export
checkMaxAgeShape : Policy -> Maybe String -> Either ValidationError ()
checkMaxAgeShape p ts = case p.maxAge of
  Nothing => Right ()
  Just _  => case ts of
    Nothing =>
      Left (PolicyViolation "Policy specifies max_age but manifest has no timestamp")
    Just t  => case parseTimestamp t of
      Nothing =>
        Left (PolicyViolation ("Policy specifies max_age but timestamp is not ISO-8601 YYYY-MM-DDTHH:MM:SSZ: " ++ t))
      Just _  => Right ()

||| Freshness: the manifest age (now - issued, both in seconds since the
||| Unix epoch) must not exceed max_age seconds.
public export
checkFreshness : (now : Integer) -> (issued : Integer) -> (maxAge : Nat) ->
                 Either ValidationError ()
checkFreshness now issued maxAge =
  if now - issued > cast maxAge
     then Left (PolicyViolation ("Manifest age " ++ show (now - issued)
                  ++ "s exceeds policy max_age " ++ show maxAge ++ "s"))
     else Right ()

||| Validate the clock-free policy constraints of a manifest: require_sig
||| demands an attestation, and max_age demands a parseable timestamp.
||| Enforced by validateManifest (and hence validateManifestIO).
public export
validatePolicy : Manifest -> Either ValidationError ()
validatePolicy m = case m.policy of
  Nothing => Right ()
  Just p  => do
    checkRequireSig p m.attestation
    checkMaxAgeShape p m.manifestData.timestamp

||| Validate all policy constraints of a manifest at a given time (seconds
||| since the Unix epoch): the clock-free constraints plus max_age
||| freshness. Pure and total - the caller supplies `now`;
||| validateManifestIO fetches it from the system clock.
public export
validatePolicyAt : (now : Integer) -> Manifest -> Either ValidationError ()
validatePolicyAt now m = do
  validatePolicy m
  case m.policy of
    Nothing => Right ()
    Just p  => case (p.maxAge, m.manifestData.timestamp) of
      (Just maxAge, Just ts) => case parseTimestamp ts of
        -- Unreachable after validatePolicy succeeds, but kept total
        -- and fail-closed rather than assuming reachability.
        Nothing =>
          Left (PolicyViolation ("Policy specifies max_age but timestamp is not ISO-8601 YYYY-MM-DDTHH:MM:SSZ: " ++ ts))
        Just issued => checkFreshness now issued maxAge
      _ => Right ()

--------------------------------------------------------------------------------
-- Manifest Validation
--------------------------------------------------------------------------------

||| Validate a complete manifest (pure version: no signature verification,
||| no clock). Enforces the structural constraints and the clock-free
||| policy constraints (require_sig, max_age shape). Use validateManifestIO
||| for full validation including signatures and max_age freshness.
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
  -- Enforce clock-free policy constraints (require_sig, max_age shape)
  validatePolicy m
  -- Signature verification and freshness are IO concerns - see validateManifestIO
  Right (MkValidManifest m)

--------------------------------------------------------------------------------
-- Canonical Signing Serialization (convention v1)
--
-- The bytes an attestation signature covers. Two properties matter:
--
--   1. Every semantically meaningful field is bound: version, subsystem,
--      timestamp, all refs, the policy, and the attestation's witness and
--      pubkey. Only the signature itself is excluded (it cannot sign
--      itself). An earlier form covered just version ++ subsystem ++ refs,
--      so timestamp, policy and witness were freely tamperable on a
--      "signed" manifest.
--
--   2. The encoding is delimited: every variable-length field carries an
--      8-byte big-endian length prefix, lists carry a count prefix, and
--      optional fields carry a presence byte. Bare concatenation let
--      distinct manifests serialize identically (boundary shift:
--      version="ab"/subsystem="c" vs version="a"/subsystem="bc").
--
-- Signers MUST produce exactly these bytes: construct the manifest with
-- its attestation (witness + pubkey, signature field ignored), then sign
-- blake3(serializeForSigning m). CI enforces the positive path end-to-end
-- in tests/ffi/CryptoFFITest.idr. Any change to this layout is a breaking
-- change to the signing convention and must bump the domain tag.
--------------------------------------------------------------------------------

stringToBytes : String -> List Bits8
stringToBytes s = map (cast . ord) (unpack s)

||| 8-byte big-endian encoding of a Nat (field lengths and list counts;
||| real values are far below 2^64).
natToBE8 : Nat -> List Bits8
natToBE8 n =
  let i = cast {to=Integer} n
      byte : Integer -> Bits8
      byte x = cast (x `mod` 256)
  in [ byte (i `div` 72057594037927936)   -- 256^7
     , byte (i `div` 281474976710656)     -- 256^6
     , byte (i `div` 1099511627776)       -- 256^5
     , byte (i `div` 4294967296)          -- 256^4
     , byte (i `div` 16777216)            -- 256^3
     , byte (i `div` 65536)               -- 256^2
     , byte (i `div` 256)
     , byte i ]

lenPrefixed : List Bits8 -> List Bits8
lenPrefixed bs = natToBE8 (length bs) ++ bs

fieldStr : String -> List Bits8
fieldStr s = lenPrefixed (stringToBytes s)

||| Optional fields carry an explicit presence byte so Nothing and
||| Just-with-empty-content cannot collide.
fieldOpt : (a -> List Bits8) -> Maybe a -> List Bits8
fieldOpt _ Nothing  = [0]
fieldOpt f (Just x) = 1 :: f x

fieldBool : Bool -> List Bits8
fieldBool False = [0]
fieldBool True  = [1]

refToBytes : Ref -> List Bits8
refToBytes r = fieldStr r.name
            ++ fieldStr (show r.hash.algorithm)
            ++ fieldStr r.hash.value

policyToBytes : Policy -> List Bits8
policyToBytes p = fieldStr (show p.mode)
               ++ fieldOpt natToBE8 p.maxAge
               ++ fieldBool p.requireSig

||| Witness and pubkey are signed; the signature field is excluded.
attestationToBytes : Attestation -> List Bits8
attestationToBytes a = fieldStr a.witness ++ fieldStr a.pubkey

||| Canonical signing bytes of a manifest — see the convention note above.
||| Exported so signer tooling and the end-to-end CI test produce exactly
||| the bytes the validator verifies.
export
serializeForSigning : Manifest -> List Bits8
serializeForSigning m =
     stringToBytes "ochrance-sign-v1"
  ++ fieldStr m.manifestData.version
  ++ fieldStr m.manifestData.subsystem
  ++ fieldOpt fieldStr m.manifestData.timestamp
  ++ natToBE8 (length m.refs)
  ++ concatMap refToBytes m.refs
  ++ fieldOpt policyToBytes m.policy
  ++ fieldOpt attestationToBytes m.attestation

||| Verify Ed25519 signature.
||| Returns False on hex parsing failure or buffer allocation failure.
verifySignatureIO : HasIO io => String -> String -> Vect 32 Bits8 -> io Bool
verifySignatureIO sigHex pubkeyHex hash = do
  -- Verify signature using Ed25519 FFI
  result <- ed25519VerifyHex sigHex pubkeyHex (toList hash)
  case result of
    Left _          => pure False  -- Buffer allocation failure => reject
    Right Nothing   => pure False  -- Hex parsing failure => reject
    Right (Just ok) => pure ok

||| Validate a complete manifest with signature verification and policy
||| freshness (IO version). This performs full validation including
||| cryptographic signature checks and max_age enforcement against the
||| system clock.
export
validateManifestIO : HasIO io => Manifest -> io (Either ValidationError ValidManifest)
validateManifestIO m = do
  -- Run pure validation first (structure + clock-free policy constraints)
  case validateManifest m of
    Left err => pure (Left err)
    Right _ => do
      -- Enforce max_age freshness against the system clock
      now <- time
      case validatePolicyAt now m of
        Left err => pure (Left err)
        Right () =>
          -- If attestation present, verify signature
          case m.attestation of
            Nothing => pure (Right (MkValidManifest m))
            Just att => do
              -- Compute manifest hash for signature verification
              let manifestBytes = serializeForSigning m
              hashResult <- blake3 manifestBytes

              case hashResult of
                Left _ => pure (Left SignatureVerificationFailed)
                Right manifestHash => do
                  -- Verify signature (Ed25519 via FFI)
                  signatureValid <- verifySignatureIO att.signature att.pubkey manifestHash

                  if signatureValid
                     then pure (Right (MkValidManifest m))
                     else pure (Left SignatureVerificationFailed)
