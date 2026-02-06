||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.Framework.Error - Error taxonomy for the Ochránce framework
|||
||| Uses the q/p/z classification:
|||   q - Query errors (user input validation failures)
|||   p - Proof errors (verification/proof failures)
|||   z - Zone errors (system/IO/environment failures)

module Ochrance.Framework.Error

%default total

--------------------------------------------------------------------------------
-- Error Classification
--------------------------------------------------------------------------------

||| Error zone classification
public export
data ErrorZone = Q | P | Z

public export
Show ErrorZone where
  show Q = "q"
  show P = "p"
  show Z = "z"

--------------------------------------------------------------------------------
-- Query Errors (user input)
--------------------------------------------------------------------------------

||| Errors from invalid user input or queries
public export
data QueryError
  = InvalidManifestPath String
  | MalformedA2ML String
  | UnsupportedVersion String
  | InvalidHashAlgorithm String
  | MissingRequiredField String

public export
Show QueryError where
  show (InvalidManifestPath p)  = "q/invalid-path: " ++ p
  show (MalformedA2ML msg)      = "q/malformed-a2ml: " ++ msg
  show (UnsupportedVersion v)   = "q/unsupported-version: " ++ v
  show (InvalidHashAlgorithm a) = "q/invalid-hash-algo: " ++ a
  show (MissingRequiredField f) = "q/missing-field: " ++ f

--------------------------------------------------------------------------------
-- Proof Errors (verification failures)
--------------------------------------------------------------------------------

||| Errors from failed verification or proof checking
public export
data ProofError
  = HashMismatch String String String   -- ref name, expected, actual
  | MerkleRootMismatch String String    -- expected, actual
  | SignatureInvalid String
  | TotalityCheckFailed String
  | ProofTimeout Nat                    -- timeout in ms

public export
Show ProofError where
  show (HashMismatch name exp act) = "p/hash-mismatch: "
    ++ name ++ " expected=" ++ exp ++ " actual=" ++ act
  show (MerkleRootMismatch exp act) = "p/merkle-root-mismatch: expected="
    ++ exp ++ " actual=" ++ act
  show (SignatureInvalid msg)       = "p/signature-invalid: " ++ msg
  show (TotalityCheckFailed fn)     = "p/totality-failed: " ++ fn
  show (ProofTimeout ms)            = "p/timeout: " ++ show ms ++ "ms"

--------------------------------------------------------------------------------
-- Zone Errors (system/environment)
--------------------------------------------------------------------------------

||| Errors from the system environment
public export
data ZoneError
  = FileNotFound String
  | PermissionDenied String
  | IOFailure String
  | FFIError String
  | OutOfMemory

public export
Show ZoneError where
  show (FileNotFound path)    = "z/file-not-found: " ++ path
  show (PermissionDenied path) = "z/permission-denied: " ++ path
  show (IOFailure msg)        = "z/io-failure: " ++ msg
  show (FFIError msg)         = "z/ffi-error: " ++ msg
  show OutOfMemory            = "z/out-of-memory"

--------------------------------------------------------------------------------
-- Unified Error Type
--------------------------------------------------------------------------------

||| Top-level error type for the Ochránce framework
public export
data OchranceError
  = QError QueryError
  | PError ProofError
  | ZError ZoneError

public export
Show OchranceError where
  show (QError e) = show e
  show (PError e) = show e
  show (ZError e) = show e

||| Get the error zone classification
public export
errorZone : OchranceError -> ErrorZone
errorZone (QError _) = Q
errorZone (PError _) = P
errorZone (ZError _) = Z
