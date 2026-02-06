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

-- TODO: Implement using structural recursion on token list.
-- Key functions needed:
--   expectKeyword : Token -> ParserState -> Either ParseError ParserState
--   expectToken   : Token -> ParserState -> Either ParseError ParserState
--   parseManifestBody : ParserState -> Either ParseError (ManifestData, ParserState)
--   parseRefsBody     : ParserState -> Either ParseError (List Ref, ParserState)
--   parseOptionalAttestation : ParserState -> Either ParseError (Maybe Attestation, ParserState)
--   parseOptionalPolicy      : ParserState -> Either ParseError (Maybe Policy, ParserState)

||| Parse a list of tokens into a Manifest.
||| Expects: @manifest { ... } @refs { ... } [@attestation { ... }] [@policy { ... }]
public export
parse : List Token -> Either ParseError Manifest
parse tokens = Left (UnexpectedEOF "parser not yet implemented")
