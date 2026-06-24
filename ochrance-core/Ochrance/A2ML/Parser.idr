-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.A2ML.Parser - Total parser for A2ML token streams
|||
||| Converts a token list from the Lexer into a Manifest AST.
||| All functions use structural recursion on the token list.

module Ochrance.A2ML.Parser

import Ochrance.A2ML.Lexer
import Ochrance.A2ML.Types

%default total

--------------------------------------------------------------------------------
-- Parse Errors
--------------------------------------------------------------------------------

||| Errors that can occur during parsing
public export
data ParseError
  = UnexpectedToken String Token Nat   -- expected, got, position
  | MissingSection String Nat          -- section name, position
  | DuplicateSection String Nat        -- section name, position
  | InvalidValue String String String Nat  -- field, value, reason, position
  | UnexpectedEOF String               -- context

public export
Show ParseError where
  show (UnexpectedToken exp got pos) = "Parse error at position "
    ++ show pos ++ ": expected " ++ exp ++ ", got " ++ show got
  show (MissingSection sec pos)      = "Missing required section @"
    ++ sec ++ " at position " ++ show pos
  show (DuplicateSection sec pos)    = "Duplicate section @"
    ++ sec ++ " at position " ++ show pos
  show (InvalidValue f v r pos)      = "Invalid value for "
    ++ f ++ "=" ++ v ++ " (" ++ r ++ ") at position " ++ show pos
  show (UnexpectedEOF ctx)           = "Unexpected end of input in " ++ ctx

--------------------------------------------------------------------------------
-- Parser State
--------------------------------------------------------------------------------

||| Parser state tracks position for error reporting
record ParserState where
  constructor MkState
  tokens   : List Token
  position : Nat

||| Advance the parser by consuming one token
advance : ParserState -> ParserState
advance (MkState [] p)        = MkState [] p
advance (MkState (_ :: ts) p) = MkState ts (S p)

||| Peek at the current token without consuming
peek : ParserState -> Maybe Token
peek (MkState [] _)       = Nothing
peek (MkState (t :: _) _) = Just t

--------------------------------------------------------------------------------
-- Parser Implementation
--------------------------------------------------------------------------------

||| Expect a specific token, consume it, and advance the parser state.
||| Uses structural recursion on the token list.
total
expectToken : Token -> ParserState -> Either ParseError ParserState
expectToken expected (MkState [] pos) =
  Left (UnexpectedEOF ("expected " ++ show expected))
expectToken expected st@(MkState (tok :: rest) pos) =
  if tok == expected
    then Right (MkState rest (S pos))
    else Left (UnexpectedToken (show expected) tok pos)

||| Parse a field assignment: IDENT EQUALS STRING
total
parseField : ParserState -> Either ParseError (String, String, ParserState)
parseField (MkState [] pos) =
  Left (UnexpectedEOF "field assignment")
parseField (MkState (IDENT name :: EQUALS :: STRING value :: rest) pos) =
  Right (name, value, MkState rest (pos + 3))
parseField (MkState (tok :: _) pos) =
  Left (UnexpectedToken "IDENT = STRING" tok pos)

||| Parse the @manifest section body.
||| Expects: version = "..." subsystem = "..." [timestamp = "..."] }
total
parseManifestBody : ParserState -> Either ParseError (ManifestData, ParserState)
parseManifestBody st = do
  -- Parse version field
  (vField, vValue, st1) <- parseField st
  unless (vField == "version") $
    Left (InvalidValue "manifest" vField "expected 'version' field" st.position)

  -- Parse subsystem field
  (sField, sValue, st2) <- parseField st1
  unless (sField == "subsystem") $
    Left (InvalidValue "manifest" sField "expected 'subsystem' field" st2.position)

  -- Check for optional timestamp or closing brace
  case peek st2 of
    Just RBRACE =>
      let manifest = MkManifestData vValue sValue Nothing
      in Right (manifest, advance st2)
    Just (IDENT "timestamp") =>
      case parseField st2 of
        Right (tField, tValue, st3) =>
          case expectToken RBRACE st3 of
            Right st4 =>
              let manifest = MkManifestData vValue sValue (Just tValue)
              in Right (manifest, st4)
            Left err => Left err
        Left err => Left err
    Just tok =>
      Left (UnexpectedToken "RBRACE or timestamp" tok st2.position)
    Nothing =>
      Left (UnexpectedEOF "manifest body")
  where
    unless : Bool -> Either ParseError () -> Either ParseError ()
    unless True  _   = Right ()
    unless False err = err

||| Parse the @refs section body, accumulating refs until RBRACE.
|||
||| Total by structural recursion: the inner loop matches on the token
||| list and each accepted ref (IDENT COLON HASH) recurses on a strict
||| tail, so the argument provably shrinks (no fuel, no unsafe escape hatch).
total
parseRefsBody : ParserState -> Either ParseError (List Ref, ParserState)
parseRefsBody (MkState toks pos) = go toks pos []
  where
    go : List Token -> Nat -> List Ref
      -> Either ParseError (List Ref, ParserState)
    go [] pos acc =
      Left (UnexpectedEOF "refs body")
    go (RBRACE :: rest) pos acc =
      Right (reverse acc, MkState rest (S pos))
    go (IDENT name :: COLON :: HASH alg value :: rest) pos acc =
      case parseHashAlgorithm alg of
        Just algorithm =>
          go rest (pos + 3) (MkRef name (MkHash algorithm value) :: acc)
        Nothing =>
          Left (InvalidValue "ref" name ("unsupported hash algorithm: " ++ alg) pos)
    go (tok :: _) pos acc =
      Left (UnexpectedToken "IDENT : HASH" tok pos)

||| Parse the optional @attestation section.
||| Returns Nothing if @attestation is not present.
total
parseOptionalAttestation : ParserState -> Either ParseError (Maybe Attestation, ParserState)
parseOptionalAttestation st =
  case peek st of
    Just ATTESTATION => do
      st1 <- expectToken ATTESTATION st
      st2 <- expectToken LBRACE st1

      -- Parse witness field
      (wField, wValue, st3) <- parseField st2
      unless (wField == "witness") $
        Left (InvalidValue "attestation" wField "expected 'witness' field" st.position)

      -- Parse signature field
      (sField, sValue, st4) <- parseField st3
      unless (sField == "signature") $
        Left (InvalidValue "attestation" sField "expected 'signature' field" st.position)

      -- Parse pubkey field
      (pField, pValue, st5) <- parseField st4
      unless (pField == "pubkey") $
        Left (InvalidValue "attestation" pField "expected 'pubkey' field" st.position)

      st6 <- expectToken RBRACE st5

      let attestation = MkAttestation wValue sValue pValue
      Right (Just attestation, st6)

    _ => Right (Nothing, st)
  where
    unless : Bool -> Either ParseError () -> Either ParseError ()
    unless True  _   = Right ()
    unless False err = err

||| Parse verification mode from string
total
parseVerificationMode : String -> Maybe VerificationMode
parseVerificationMode "lax"      = Just Lax
parseVerificationMode "checked"  = Just Checked
parseVerificationMode "attested" = Just Attested
parseVerificationMode _          = Nothing

||| Parse the optional @policy section.
||| Returns Nothing if @policy is not present.
|||
||| Total by structural recursion: the field loop matches on the token
||| list and each accepted field (IDENT EQUALS value) recurses on a strict
||| tail, so the argument provably shrinks (no fuel, no unsafe escape hatch).
total
parseOptionalPolicy : ParserState -> Either ParseError (Maybe Policy, ParserState)
parseOptionalPolicy st =
  case peek st of
    Just POLICY => do
      st1 <- expectToken POLICY st
      st2 <- expectToken LBRACE st1

      -- Parse mode field
      (mField, mValue, MkState toks pos) <- parseField st2
      unless (mField == "mode") $
        Left (InvalidValue "policy" mField "expected 'mode' field" st.position)

      mode <- case parseVerificationMode mValue of
        Just m => Right m
        Nothing => Left (InvalidValue "policy" "mode" ("invalid mode: " ++ mValue) st.position)

      -- Check for optional fields or closing brace
      parsePolicyFields toks pos mode Nothing False

    _ => Right (Nothing, st)
  where
    unless : Bool -> Either ParseError () -> Either ParseError ()
    unless True  _   = Right ()
    unless False err = err

    ||| Field loop over the raw token list (structurally decreasing).
    parsePolicyFields : List Token -> Nat -> VerificationMode -> Maybe Nat -> Bool
                     -> Either ParseError (Maybe Policy, ParserState)
    parsePolicyFields (RBRACE :: rest) pos mode maxAge requireSig =
      Right (Just (MkPolicy mode maxAge requireSig), MkState rest (S pos))
    parsePolicyFields (IDENT "max_age" :: EQUALS :: NUMBER value :: rest) pos mode maxAge requireSig =
      if value >= 0
        then parsePolicyFields rest (pos + 3) mode (Just (cast value)) requireSig
        else Left (InvalidValue "policy" "max_age" "must be non-negative" pos)
    parsePolicyFields (IDENT "require_sig" :: EQUALS :: IDENT "true" :: rest) pos mode maxAge _ =
      parsePolicyFields rest (pos + 3) mode maxAge True
    parsePolicyFields (IDENT "require_sig" :: EQUALS :: IDENT "false" :: rest) pos mode maxAge _ =
      parsePolicyFields rest (pos + 3) mode maxAge False
    parsePolicyFields (tok :: _) pos _ _ _ =
      Left (UnexpectedToken "RBRACE, max_age, or require_sig" tok pos)
    parsePolicyFields [] _ _ _ _ =
      Left (UnexpectedEOF "policy body")

||| Parse a list of tokens into a Manifest.
||| Expects: @manifest { ... } @refs { ... } [@attestation { ... }] [@policy { ... }]
public export
total
parse : List Token -> Either ParseError Manifest
parse tokens = do
  let st = MkState tokens 0

  -- Parse @manifest section
  st1 <- expectToken MANIFEST st
  st2 <- expectToken LBRACE st1
  (manifestData, st3) <- parseManifestBody st2

  -- Parse @refs section
  st4 <- expectToken REFS st3
  st5 <- expectToken LBRACE st4
  (refs, st6) <- parseRefsBody st5

  -- Parse optional @attestation section
  (attestation, st7) <- parseOptionalAttestation st6

  -- Parse optional @policy section
  (policy, st8) <- parseOptionalPolicy st7

  -- Enforce end-of-input: a complete manifest must be followed only by EOF.
  -- This rejects trailing tokens such as duplicate @sections, so malformed
  -- input cannot smuggle ignored data past the parser.
  case peek st8 of
    Just EOF => Right (MkManifest manifestData refs attestation policy)
    Nothing  => Right (MkManifest manifestData refs attestation policy)
    Just tok => Left (UnexpectedToken "end of input" tok st8.position)
