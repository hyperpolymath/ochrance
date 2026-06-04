-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Ochrance.A2ML.Roundtrip — a machine-checked manifest round-trip theorem.
|||
||| The production pipeline `parse . lex . serialize` cannot carry a compile-time
||| `= Right m` theorem in Idris2: the `lex` stage rests on primitive String
||| operations (`pack`/`unpack`/`++`/`parseInteger`) that have no provable
||| equational theory, and the production parser is built from `if`-on-comparison
||| (`expectToken`, `unless`) which the type-checker does not reduce.
|||
||| This module instead proves the grammar itself is *invertible*, with a clean
||| positional token codec over the production `Manifest`/`Ref`/... types:
|||
|||     decodeManifest (encodeManifest m) = Just m       -- for every m
|||
||| The codec pattern-matches on token *constructors* only (never on String
||| literals or `if`), and delegates enum tags to `parseHashAlgorithm`-style
||| helpers that do reduce — so the inductive proof goes through with no
||| `believe_me`, `assert_*`, or `postulate`. The production lexer/parser are
||| then exercised against this reference at runtime (see `roundtripProperty`).

module Ochrance.A2ML.Roundtrip

import Ochrance.A2ML.Types
import Ochrance.A2ML.Lexer
import Ochrance.A2ML.Parser
import Ochrance.A2ML.Serializer
import Data.List

%default total

--------------------------------------------------------------------------------
-- Enum tag round-trips (each by case analysis; every case is Refl)
--------------------------------------------------------------------------------

algoRT : (a : HashAlgorithm) -> parseHashAlgorithm (show a) = Just a
algoRT SHA256   = Refl
algoRT SHA3_256 = Refl
algoRT BLAKE3   = Refl

showMode : VerificationMode -> String
showMode Lax      = "lax"
showMode Checked  = "checked"
showMode Attested = "attested"

parseMode : String -> Maybe VerificationMode
parseMode "lax"      = Just Lax
parseMode "checked"  = Just Checked
parseMode "attested" = Just Attested
parseMode _          = Nothing

modeRT : (m : VerificationMode) -> parseMode (showMode m) = Just m
modeRT Lax      = Refl
modeRT Checked  = Refl
modeRT Attested = Refl

--------------------------------------------------------------------------------
-- Primitive field codecs (markers are nullary token constructors)
--------------------------------------------------------------------------------

||| Maybe String: present = LBRACE then the string; absent = RBRACE.
encMStr : Maybe String -> List Token
encMStr Nothing  = [RBRACE]
encMStr (Just s) = [LBRACE, STRING s]

decMStr : List Token -> Maybe (Maybe String, List Token)
decMStr (RBRACE :: rest)             = Just (Nothing, rest)
decMStr (LBRACE :: STRING s :: rest) = Just (Just s, rest)
decMStr _                            = Nothing

mstrRT : (x : Maybe String) -> (rest : List Token) -> decMStr (encMStr x ++ rest) = Just (x, rest)
mstrRT Nothing  rest = Refl
mstrRT (Just s) rest = Refl

||| Bool: True = LBRACE, False = RBRACE.
encBool : Bool -> List Token
encBool True  = [LBRACE]
encBool False = [RBRACE]

decBool : List Token -> Maybe (Bool, List Token)
decBool (LBRACE :: rest) = Just (True, rest)
decBool (RBRACE :: rest) = Just (False, rest)
decBool _                = Nothing

boolRT : (b : Bool) -> (rest : List Token) -> decBool (encBool b ++ rest) = Just (b, rest)
boolRT True  rest = Refl
boolRT False rest = Refl

||| Nat in unary (COLON per successor, EQUALS terminator) — proof-friendly,
||| avoiding the primitive Integer<->Nat cast.
encNat : Nat -> List Token
encNat Z     = [EQUALS]
encNat (S k) = COLON :: encNat k

decNat : List Token -> Maybe (Nat, List Token)
decNat (EQUALS :: rest) = Just (Z, rest)
decNat (COLON :: more)  = case decNat more of
  Just (k, rest) => Just (S k, rest)
  Nothing        => Nothing
decNat _                = Nothing

natRT : (n : Nat) -> (rest : List Token) -> decNat (encNat n ++ rest) = Just (n, rest)
natRT Z     rest = Refl
natRT (S k) rest = rewrite natRT k rest in Refl

||| Maybe Nat.
encMNat : Maybe Nat -> List Token
encMNat Nothing  = [RBRACE]
encMNat (Just n) = LBRACE :: encNat n

decMNat : List Token -> Maybe (Maybe Nat, List Token)
decMNat (RBRACE :: rest) = Just (Nothing, rest)
decMNat (LBRACE :: more) = case decNat more of
  Just (n, rest) => Just (Just n, rest)
  Nothing        => Nothing
decMNat _                = Nothing

mnatRT : (x : Maybe Nat) -> (rest : List Token) -> decMNat (encMNat x ++ rest) = Just (x, rest)
mnatRT Nothing  rest = Refl
mnatRT (Just n) rest = rewrite natRT n rest in Refl

--------------------------------------------------------------------------------
-- References (the recursive heart of the grammar)
--------------------------------------------------------------------------------

encRef : Ref -> List Token
encRef r = [IDENT r.name, COLON, HASH (show r.hash.algorithm) r.hash.value]

||| Reference list, terminated by RBRACE.
encRefs : List Ref -> List Token
encRefs []        = [RBRACE]
encRefs (r :: rs) = encRef r ++ encRefs rs

decRefs : List Token -> Maybe (List Ref, List Token)
decRefs (RBRACE :: rest) = Just ([], rest)
decRefs (IDENT n :: COLON :: HASH a v :: more) =
  case parseHashAlgorithm a of
    Just algo => case decRefs more of
      Just (rs, rest) => Just (MkRef n (MkHash algo v) :: rs, rest)
      Nothing         => Nothing
    Nothing => Nothing
decRefs _ = Nothing

refsRT : (rs : List Ref) -> (rest : List Token) -> decRefs (encRefs rs ++ rest) = Just (rs, rest)
refsRT []                            rest = Refl
refsRT (MkRef n (MkHash a v) :: rs)  rest =
  rewrite algoRT a in
  rewrite refsRT rs rest in Refl

--------------------------------------------------------------------------------
-- Attestation (optional)
--------------------------------------------------------------------------------

encMAtt : Maybe Attestation -> List Token
encMAtt Nothing  = [RBRACE]
encMAtt (Just a) = [LBRACE, STRING a.witness, STRING a.signature, STRING a.pubkey]

decMAtt : List Token -> Maybe (Maybe Attestation, List Token)
decMAtt (RBRACE :: rest) = Just (Nothing, rest)
decMAtt (LBRACE :: STRING w :: STRING s :: STRING p :: rest) =
  Just (Just (MkAttestation w s p), rest)
decMAtt _ = Nothing

mattRT : (x : Maybe Attestation) -> (rest : List Token) -> decMAtt (encMAtt x ++ rest) = Just (x, rest)
mattRT Nothing                    rest = Refl
mattRT (Just (MkAttestation w s p)) rest = Refl

--------------------------------------------------------------------------------
-- Policy (optional)
--------------------------------------------------------------------------------

encMPol : Maybe Policy -> List Token
encMPol Nothing  = [RBRACE]
encMPol (Just p) = LBRACE :: STRING (showMode p.mode) :: (encMNat p.maxAge ++ encBool p.requireSig)

decMPol : List Token -> Maybe (Maybe Policy, List Token)
decMPol (RBRACE :: rest) = Just (Nothing, rest)
decMPol (LBRACE :: STRING modeStr :: more) =
  case parseMode modeStr of
    Just mode => case decMNat more of
      Just (maxAge, more2) => case decBool more2 of
        Just (requireSig, rest) => Just (Just (MkPolicy mode maxAge requireSig), rest)
        Nothing                 => Nothing
      Nothing => Nothing
    Nothing => Nothing
decMPol _ = Nothing

mpolRT : (x : Maybe Policy) -> (rest : List Token) -> decMPol (encMPol x ++ rest) = Just (x, rest)
mpolRT Nothing                       rest = Refl
mpolRT (Just (MkPolicy mode ma rs))  rest =
  rewrite modeRT mode in
  rewrite sym (appendAssociative (encMNat ma) (encBool rs) rest) in
  rewrite mnatRT ma (encBool rs ++ rest) in
  rewrite boolRT rs rest in Refl

--------------------------------------------------------------------------------
-- Whole manifest
--------------------------------------------------------------------------------

encodeManifest : Manifest -> List Token
encodeManifest m =
  MANIFEST :: STRING m.manifestData.version :: STRING m.manifestData.subsystem
  :: (encMStr m.manifestData.timestamp
   ++ REFS :: encRefs m.refs
   ++ encMAtt m.attestation
   ++ encMPol m.policy
   ++ [EOF])

decodeManifest : List Token -> Maybe Manifest
decodeManifest (MANIFEST :: STRING ver :: STRING sub :: r0) =
  case decMStr r0 of
    Just (ts, REFS :: r2) => case decRefs r2 of
      Just (rs, r3) => case decMAtt r3 of
        Just (att, r4) => case decMPol r4 of
          Just (pol, EOF :: []) =>
            Just (MkManifest (MkManifestData ver sub ts) rs att pol)
          _ => Nothing
        Nothing => Nothing
      Nothing => Nothing
    _ => Nothing
decodeManifest _ = Nothing

||| THEOREM: the reference codec is invertible for every manifest.
export
roundtripManifest : (m : Manifest) -> decodeManifest (encodeManifest m) = Just m
roundtripManifest (MkManifest (MkManifestData ver sub ts) rs att pol) =
  rewrite mstrRT ts (REFS :: (encRefs rs ++ encMAtt att ++ encMPol pol ++ [EOF])) in
  rewrite refsRT rs (encMAtt att ++ encMPol pol ++ [EOF]) in
  rewrite mattRT att (encMPol pol ++ [EOF]) in
  rewrite mpolRT pol [EOF] in
  Refl

--------------------------------------------------------------------------------
-- Production-pipeline check (runtime)
--------------------------------------------------------------------------------

||| Round-trip check against the *production* pipeline: serialize, then lex and
||| parse, then confirm the result re-serializes identically.
|||
||| `roundtripManifest` above is the formal guarantee (over the reference token
||| codec). This complements it by exercising the real lexer + parser, whose
||| String stage lies outside the proof's reach (primitive `pack`/`unpack` etc.
||| have no equational theory). It holds for well-formed manifests (hex digests,
||| no in-string delimiters) and is exercised by the test suite.
export
roundtripProperty : Manifest -> Bool
roundtripProperty m =
  case lex (serialize m) of
    Left _ => False
    Right toks => case parse toks of
      Left _ => False
      Right m2 => serialize m2 == serialize m
