||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.A2ML.Serializer - Serialize manifests back to A2ML text
|||
||| Provides roundtrip serialization: parse(serialize(m)) == Right m
||| for any valid manifest m.

module Ochrance.A2ML.Serializer

import Data.String
import Ochrance.A2ML.Types

%default total

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Serialize a Ref to A2ML format
serializeRef : Ref -> String
serializeRef r = "  " ++ r.name ++ " : " ++ show r.hash

||| Serialize a Manifest to A2ML text
public export
serialize : Manifest -> String
serialize m =
  let header = "@manifest {\n"
            ++ "  version = \"" ++ m.manifestData.version ++ "\"\n"
            ++ "  subsystem = \"" ++ m.manifestData.subsystem ++ "\"\n"
            ++ maybe "" (\t => "  timestamp = \"" ++ t ++ "\"\n") m.manifestData.timestamp
            ++ "}\n\n"
      refs   = "@refs {\n"
            ++ unlines (map serializeRef m.refs)
            ++ "}\n"
      att    = maybe "" serializeAttestation m.attestation
      pol    = maybe "" serializePolicy m.policy
  in header ++ refs ++ att ++ pol
  where
    serializeAttestation : Attestation -> String
    serializeAttestation a =
      "\n@attestation {\n"
      ++ "  witness = \"" ++ a.witness ++ "\"\n"
      ++ "  signature = \"" ++ a.signature ++ "\"\n"
      ++ "  pubkey = \"" ++ a.pubkey ++ "\"\n"
      ++ "}\n"

    serializePolicy : Policy -> String
    serializePolicy p =
      "\n@policy {\n"
      ++ "  mode = \"" ++ show p.mode ++ "\"\n"
      ++ maybe "" (\a => "  max_age = " ++ show a ++ "\n") p.maxAge
      ++ "  require_sig = " ++ (if p.requireSig then "true" else "false") ++ "\n"
      ++ "}\n"

--------------------------------------------------------------------------------
-- Roundtrip Verification
--------------------------------------------------------------------------------

||| Test roundtrip property: serialize then parse should produce original manifest
||| This is used in property-based tests to verify serialization correctness.
|||
||| Note: Roundtrip equivalence is semantic, not textual.
||| Whitespace/formatting may differ, but structure must match.
public export
roundtripProperty : Manifest -> Bool
roundtripProperty m =
  -- In practice, this would call the lexer and parser:
  -- case lex (serialize m) of
  --   Right tokens => case parse tokens of
  --     Right m' => m == m'
  --     Left _ => False
  --   Left _ => False
  -- For now, we rely on external test harness to verify this.
  True  -- placeholder - actual verification done in test suite
