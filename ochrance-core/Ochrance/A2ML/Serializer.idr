-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| SPDX-License-Identifier: MPL-2.0
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

-- Round-trip correctness lives in `Ochrance.A2ML.Roundtrip`:
--   * `roundtripManifest` — a machine-checked theorem that the grammar is
--     invertible: `decodeManifest (encodeManifest m) = Just m` for every `m`.
--   * `roundtripProperty` — a runtime check composing this serializer with the
--     production lexer and parser.
-- Neither can live here: both must reference the parser, and a serializer that
-- imports the parser would create an import cycle. (The previous placeholder
-- `roundtripProperty m = True` asserted nothing and has been removed.)
