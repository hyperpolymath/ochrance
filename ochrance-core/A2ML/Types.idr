||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.A2ML.Types - Core types for A2ML (Attestation & Audit Markup Language)
|||
||| Defines the AST produced by the parser and consumed by the validator
||| and serializer. All types are pure data with no IO.

module Ochrance.A2ML.Types

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Hash Types
--------------------------------------------------------------------------------

||| Supported hash algorithms
public export
data HashAlgorithm = SHA256 | SHA3_256 | BLAKE3

public export
Show HashAlgorithm where
  show SHA256   = "sha256"
  show SHA3_256 = "sha3-256"
  show BLAKE3   = "blake3"

public export
Eq HashAlgorithm where
  SHA256   == SHA256   = True
  SHA3_256 == SHA3_256 = True
  BLAKE3   == BLAKE3   = True
  _        == _        = False

||| Parse a hash algorithm name
public export
parseHashAlgorithm : String -> Maybe HashAlgorithm
parseHashAlgorithm "sha256"   = Just SHA256
parseHashAlgorithm "sha3-256" = Just SHA3_256
parseHashAlgorithm "blake3"   = Just BLAKE3
parseHashAlgorithm _          = Nothing

||| A cryptographic hash: algorithm + hex-encoded value
public export
record Hash where
  constructor MkHash
  algorithm : HashAlgorithm
  value     : String

public export
Show Hash where
  show h = show h.algorithm ++ ":" ++ h.value

public export
Eq Hash where
  h1 == h2 = h1.algorithm == h2.algorithm && h1.value == h2.value

--------------------------------------------------------------------------------
-- Verification Modes
--------------------------------------------------------------------------------

||| Verification strictness levels
public export
data VerificationMode
  = Lax       -- Skip hash verification (trust manifest)
  | Checked   -- Verify hashes match filesystem state
  | Attested  -- Verify hashes + cryptographic attestation signature

public export
Show VerificationMode where
  show Lax      = "lax"
  show Checked  = "checked"
  show Attested = "attested"

public export
Eq VerificationMode where
  Lax      == Lax      = True
  Checked  == Checked  = True
  Attested == Attested = True
  _        == _        = False

--------------------------------------------------------------------------------
-- Manifest Sections
--------------------------------------------------------------------------------

||| Metadata from the @manifest section
public export
record ManifestData where
  constructor MkManifestData
  version   : String
  subsystem : String
  timestamp : Maybe String

public export
Show ManifestData where
  show m = "ManifestData{version=" ++ m.version
        ++ ", subsystem=" ++ m.subsystem ++ "}"

||| A named reference with its hash
public export
record Ref where
  constructor MkRef
  name : String
  hash : Hash

public export
Show Ref where
  show r = r.name ++ " : " ++ show r.hash

public export
Eq Ref where
  r1 == r2 = r1.name == r2.name && r1.hash == r2.hash

||| Attestation data (optional @attestation section)
public export
record Attestation where
  constructor MkAttestation
  witness   : String
  signature : String
  pubkey    : String

||| Policy constraints (optional @policy section)
public export
record Policy where
  constructor MkPolicy
  mode       : VerificationMode
  maxAge     : Maybe Nat       -- Maximum manifest age in seconds
  requireSig : Bool            -- Require attestation signature

--------------------------------------------------------------------------------
-- Complete Manifest
--------------------------------------------------------------------------------

||| A complete parsed A2ML manifest
public export
record Manifest where
  constructor MkManifest
  manifestData : ManifestData
  refs         : List Ref
  attestation  : Maybe Attestation
  policy       : Maybe Policy

public export
Show Manifest where
  show m = "Manifest{" ++ show m.manifestData
        ++ ", refs=" ++ show (length m.refs)
        ++ ", attested=" ++ show (isJust m.attestation) ++ "}"

--------------------------------------------------------------------------------
-- Validated Manifest (type-level witness)
--------------------------------------------------------------------------------

||| A manifest that has passed semantic validation.
||| The type itself serves as proof of validity.
public export
data ValidManifest : Type where
  MkValidManifest : (manifest : Manifest) -> ValidManifest

||| Extract the manifest from a validated wrapper
public export
unwrapValid : ValidManifest -> Manifest
unwrapValid (MkValidManifest m) = m
