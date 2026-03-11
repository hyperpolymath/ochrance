||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.FFI.Echidna - FFI bindings to the ECHIDNA theorem prover
|||
||| Provides bidirectional communication with ECHIDNA's Rust core
||| via libechidna.so. Used for neural proof synthesis when automated
||| proof generation is needed.
|||
||| Phase 2 Design: Proof Obligation Export/Import Protocol
||| -------------------------------------------------------
||| Ochrance generates proof obligations during verification (e.g. "this
||| Merkle root matches the attested root"). These obligations are exported
||| in a structured format that ECHIDNA can consume via its neural synthesis
||| pipeline.
|||
||| Protocol flow:
|||   1. Ochrance verification identifies what needs to be proved
|||   2. Obligations are serialized to S-expression format
|||   3. Exported obligations are sent to ECHIDNA via FFI (or file exchange)
|||   4. ECHIDNA returns proof witnesses (success) or failure reasons
|||   5. Ochrance imports and validates the proof results
|||
||| S-expression format for obligations:
|||   (obligation
|||     (id "ob-001")
|||     (kind hash-equality)       ; or: merkle-inclusion, signature-valid
|||     (context "filesystem/block_0")
|||     (claim (= (hash data) expected_hash))
|||     (evidence ...))            ; filled by ECHIDNA on return
|||
||| Author: Jonathan D.A. Jewell

module Ochrance.FFI.Echidna

import Data.List
import System.FFI
import Ochrance.A2ML.Types
import Ochrance.Framework.Error

%default total

--------------------------------------------------------------------------------
-- Foreign Declarations
--------------------------------------------------------------------------------

||| Prove a theorem using ECHIDNA's neural synthesis pipeline
%foreign "C:echidna_prove,libechidna"
prim__echidnaProve : String -> PrimIO String

||| Verify a proof witness using ECHIDNA's multi-prover backend
%foreign "C:echidna_verify,libechidna"
prim__echidnaVerify : String -> PrimIO Int

--------------------------------------------------------------------------------
-- Safe Wrappers (Stubbed for now - FFI implementation pending)
--------------------------------------------------------------------------------

||| Attempt to prove a theorem string via ECHIDNA.
||| Returns Left on failure, Right with the proof witness on success.
export
echidnaProve : HasIO io => String -> io (Either String String)
echidnaProve theorem = pure (Left "FFI not yet implemented")

||| Verify a proof witness via ECHIDNA's prover backends.
||| Returns True if the proof is accepted by at least one prover.
export
echidnaVerify : HasIO io => String -> io Bool
echidnaVerify witness = pure False

--------------------------------------------------------------------------------
-- Phase 2: Proof Obligation Types
--------------------------------------------------------------------------------

||| Kind of proof obligation that Ochrance can generate
public export
data ObligationKind
  = HashEquality      -- Prove that hash(data) == expected
  | MerkleInclusion   -- Prove that a leaf is included in a Merkle tree
  | SignatureValid     -- Prove that a cryptographic signature is valid
  | RepairCorrectness -- Prove that repair produced a valid state
  | TotalityWitness   -- Prove that a function terminates

public export
Show ObligationKind where
  show HashEquality      = "hash-equality"
  show MerkleInclusion   = "merkle-inclusion"
  show SignatureValid     = "signature-valid"
  show RepairCorrectness = "repair-correctness"
  show TotalityWitness   = "totality-witness"

public export
Eq ObligationKind where
  HashEquality      == HashEquality      = True
  MerkleInclusion   == MerkleInclusion   = True
  SignatureValid     == SignatureValid     = True
  RepairCorrectness == RepairCorrectness = True
  TotalityWitness   == TotalityWitness   = True
  _                 == _                 = False

||| Status of a proof obligation after ECHIDNA processing
public export
data ObligationStatus
  = Pending         -- Not yet sent to ECHIDNA
  | Submitted       -- Sent, awaiting response
  | Proved String   -- Successfully proved; String is the witness
  | Refuted String  -- ECHIDNA found a counterexample; String is explanation
  | Timeout         -- ECHIDNA timed out
  | Unsupported     -- ECHIDNA cannot handle this obligation kind

public export
Show ObligationStatus where
  show Pending          = "pending"
  show Submitted        = "submitted"
  show (Proved w)       = "proved: " ++ w
  show (Refuted reason) = "refuted: " ++ reason
  show Timeout          = "timeout"
  show Unsupported      = "unsupported"

||| A proof obligation: a claim that needs external verification.
|||
||| Fields:
|||   obligationId - Unique identifier (e.g. "ob-001")
|||   kind         - What type of proof is needed
|||   context      - Where in the system this obligation arose (e.g. "filesystem/block_0")
|||   claim        - Human-readable description of what must be proved
|||   expected     - The expected value (hash, root, signature, etc.)
|||   actual       - The actual value observed (may be empty for generation requests)
|||   status       - Current status of the obligation
public export
record ProofObligation where
  constructor MkObligation
  obligationId : String
  kind         : ObligationKind
  context      : String
  claim        : String
  expected     : String
  actual       : String
  status       : ObligationStatus

public export
Show ProofObligation where
  show ob = "(obligation (id \"" ++ ob.obligationId ++ "\") "
         ++ "(kind " ++ show ob.kind ++ ") "
         ++ "(context \"" ++ ob.context ++ "\") "
         ++ "(claim \"" ++ ob.claim ++ "\") "
         ++ "(status " ++ show ob.status ++ "))"

||| Result of importing a proof from ECHIDNA
public export
record ProofResult where
  constructor MkProofResult
  obligationId : String
  status       : ObligationStatus
  witness      : Maybe String  -- The proof witness, if proved

--------------------------------------------------------------------------------
-- Phase 2: Serialization (S-expression format)
--------------------------------------------------------------------------------

||| Serialize a proof obligation to S-expression format for ECHIDNA consumption.
|||
||| Format:
|||   (obligation
|||     (id "ob-001")
|||     (kind hash-equality)
|||     (context "filesystem/block_0")
|||     (claim "hash(block_0) == abcdef...")
|||     (expected "abcdef...")
|||     (actual "123456..."))
export
exportObligation : ProofObligation -> String
exportObligation ob =
  "(obligation\n"
  ++ "  (id \"" ++ ob.obligationId ++ "\")\n"
  ++ "  (kind " ++ show ob.kind ++ ")\n"
  ++ "  (context \"" ++ ob.context ++ "\")\n"
  ++ "  (claim \"" ++ ob.claim ++ "\")\n"
  ++ "  (expected \"" ++ ob.expected ++ "\")\n"
  ++ "  (actual \"" ++ ob.actual ++ "\"))\n"

||| Serialize a batch of obligations for ECHIDNA.
||| Wraps multiple obligations in a top-level (obligations ...) form.
export
exportObligations : List ProofObligation -> String
exportObligations obs =
  "(obligations\n"
  ++ concatMap (\ob => "  " ++ exportObligation ob ++ "\n") obs
  ++ ")\n"

||| Import a proof result from ECHIDNA's response.
|||
||| Phase 2 stub: parses a minimal response format.
||| Full implementation will parse ECHIDNA's native response format
||| including proof witnesses, counterexamples, and confidence scores.
|||
||| Expected input format (S-expression):
|||   (result
|||     (id "ob-001")
|||     (status proved)
|||     (witness "...proof term..."))
|||
||| Returns Left with a q-error if the input cannot be parsed.
export
importProofResult : String -> Either OchranceError ProofResult
importProofResult input =
  -- Phase 2 stub: always returns parse error
  -- Full implementation will:
  --   1. Lex the S-expression
  --   2. Extract obligation ID
  --   3. Parse status (proved/refuted/timeout/unsupported)
  --   4. Extract witness if status == proved
  --   5. Validate witness format
  Left (QError (MalformedA2ML ("ECHIDNA proof result parsing not yet implemented: " ++ input)))

--------------------------------------------------------------------------------
-- Phase 2: Obligation Generation Helpers
--------------------------------------------------------------------------------

||| Create a hash equality obligation from a ref verification failure.
|||
||| Used when filesystem verification detects a hash mismatch:
||| the obligation asks ECHIDNA to prove whether the expected or actual
||| hash is correct (e.g. by re-computing from source data).
export
mkHashObligation : (obligationId : String)
                -> (refName : String)
                -> (expectedHash : Hash)
                -> (actualHash : Hash)
                -> ProofObligation
mkHashObligation obId refName expected actual = MkObligation
  obId
  HashEquality
  ("filesystem/" ++ refName)
  ("hash(" ++ refName ++ ") == " ++ show expected)
  (show expected)
  (show actual)
  Pending

||| Create a Merkle inclusion obligation.
|||
||| Used when verifying that a leaf hash is part of a Merkle tree
||| with a known root.
export
mkMerkleObligation : (obligationId : String)
                  -> (leafName : String)
                  -> (rootHash : String)
                  -> (leafHash : String)
                  -> ProofObligation
mkMerkleObligation obId leafName root leaf = MkObligation
  obId
  MerkleInclusion
  ("merkle/" ++ leafName)
  ("leaf " ++ leafName ++ " in tree with root " ++ root)
  root
  leaf
  Pending

||| Create a signature validity obligation.
|||
||| Used when attestation signature verification is needed but
||| the FFI to libochrance.so is unavailable (offline verification).
export
mkSignatureObligation : (obligationId : String)
                     -> (signatureHex : String)
                     -> (pubkeyHex : String)
                     -> ProofObligation
mkSignatureObligation obId sig pk = MkObligation
  obId
  SignatureValid
  "attestation/signature"
  ("ed25519_verify(sig, pk, manifest_hash)")
  sig
  pk
  Pending
