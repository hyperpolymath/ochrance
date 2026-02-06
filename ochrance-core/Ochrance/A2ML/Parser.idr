||| SPDX-License-Identifier: PMPL-1.0-or-later
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

||| Parse a number field: IDENT EQUALS NUMBER
total
parseNumberField : ParserState -> Either ParseError (String, Integer, ParserState)
parseNumberField (MkState [] pos) =
  Left (UnexpectedEOF "number field")
parseNumberField (MkState (IDENT name :: EQUALS :: NUMBER value :: rest) pos) =
  Right (name, value, MkState rest (pos + 3))
parseNumberField (MkState (tok :: _) pos) =
  Left (UnexpectedToken "IDENT = NUMBER" tok pos)

||| Parse a boolean field: IDENT EQUALS IDENT("true"|"false")
total
parseBoolField : ParserState -> Either ParseError (String, Bool, ParserState)
parseBoolField (MkState [] pos) =
  Left (UnexpectedEOF "boolean field")
parseBoolField (MkState (IDENT name :: EQUALS :: IDENT "true" :: rest) pos) =
  Right (name, True, MkState rest (pos + 3))
parseBoolField (MkState (IDENT name :: EQUALS :: IDENT "false" :: rest) pos) =
  Right (name, False, MkState rest (pos + 3))
parseBoolField (MkState (tok :: _) pos) =
  Left (UnexpectedToken "IDENT = IDENT(true|false)" tok pos)

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
    unless True  err = err
    unless False _   = Right ()

||| Parse a single ref entry: IDENT COLON HASH
total
parseRef : ParserState -> Either ParseError (Ref, ParserState)
parseRef (MkState [] pos) =
  Left (UnexpectedEOF "ref entry")
parseRef (MkState (IDENT name :: COLON :: HASH alg value :: rest) pos) =
  case parseHashAlgorithm alg of
    Just algorithm =>
      let hash = MkHash algorithm value
          ref = MkRef name hash
      in Right (ref, MkState rest (pos + 3))
    Nothing =>
      Left (InvalidValue "ref" name ("unsupported hash algorithm: " ++ alg) pos)
parseRef (MkState (tok :: _) pos) =
  Left (UnexpectedToken "IDENT : HASH" tok pos)

mutual
  ||| Parse the @refs section body (accumulates refs until RBRACE).
  ||| Uses mutual recursion to accumulate refs.
  covering
  parseRefsBody : ParserState -> Either ParseError (List Ref, ParserState)
  parseRefsBody st = parseRefsLoop st []

  covering
  parseRefsLoop : ParserState -> List Ref -> Either ParseError (List Ref, ParserState)
  parseRefsLoop st@(MkState [] pos) acc =
    Left (UnexpectedEOF "refs body")
  parseRefsLoop st@(MkState (RBRACE :: rest) pos) acc =
    Right (reverse acc, MkState rest (S pos))
  parseRefsLoop st acc = do
    (ref, st1) <- parseRef st
    parseRefsLoop st1 (ref :: acc)

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
    unless True  err = err
    unless False _   = Right ()

||| Parse verification mode from string
total
parseVerificationMode : String -> Maybe VerificationMode
parseVerificationMode "lax"      = Just Lax
parseVerificationMode "checked"  = Just Checked
parseVerificationMode "attested" = Just Attested
parseVerificationMode _          = Nothing

||| Parse the optional @policy section.
||| Returns Nothing if @policy is not present.
covering
parseOptionalPolicy : ParserState -> Either ParseError (Maybe Policy, ParserState)
parseOptionalPolicy st =
  case peek st of
    Just POLICY => do
      st1 <- expectToken POLICY st
      st2 <- expectToken LBRACE st1

      -- Parse mode field
      (mField, mValue, st3) <- parseField st2
      unless (mField == "mode") $
        Left (InvalidValue "policy" mField "expected 'mode' field" st.position)

      mode <- case parseVerificationMode mValue of
        Just m => Right m
        Nothing => Left (InvalidValue "policy" "mode" ("invalid mode: " ++ mValue) st.position)

      -- Check for optional fields or closing brace
      parsePolicyFields st3 mode Nothing False

    _ => Right (Nothing, st)
  where
    unless : Bool -> Either ParseError () -> Either ParseError ()
    unless True  err = err
    unless False _   = Right ()

    covering
    parsePolicyFields : ParserState -> VerificationMode -> Maybe Nat -> Bool
                     -> Either ParseError (Maybe Policy, ParserState)
    parsePolicyFields st mode maxAge requireSig =
      case peek st of
        Just RBRACE =>
          let policy = MkPolicy mode maxAge requireSig
          in Right (Just policy, advance st)

        Just (IDENT "max_age") => do
          (_, value, st1) <- parseNumberField st
          unless (value >= 0) $
            Left (InvalidValue "policy" "max_age" "must be non-negative" st.position)
          parsePolicyFields st1 mode (Just (cast value)) requireSig

        Just (IDENT "require_sig") => do
          (_, value, st1) <- parseBoolField st
          parsePolicyFields st1 mode maxAge value

        Just tok =>
          Left (UnexpectedToken "RBRACE, max_age, or require_sig" tok st.position)

        Nothing =>
          Left (UnexpectedEOF "policy body")

||| Parse a list of tokens into a Manifest.
||| Expects: @manifest { ... } @refs { ... } [@attestation { ... }] [@policy { ... }]
public export
covering
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

  -- Construct manifest
  Right (MkManifest manifestData refs attestation policy)
