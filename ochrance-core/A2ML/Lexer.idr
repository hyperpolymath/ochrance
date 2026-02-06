||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.A2ML.Lexer - Total lexer for A2ML markup
|||
||| Tokenizes A2ML input strings using structural recursion on the
||| character list, guaranteeing termination.

module Ochrance.A2ML.Lexer

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Token Types
--------------------------------------------------------------------------------

||| Tokens produced by the A2ML lexer
public export
data Token
  = MANIFEST
  | REFS
  | ATTESTATION
  | POLICY
  | LBRACE
  | RBRACE
  | COLON
  | EQUALS
  | IDENT String
  | STRING String
  | NUMBER Integer
  | HASH String String   -- algorithm:value
  | EOF

public export
Show Token where
  show MANIFEST      = "MANIFEST"
  show REFS          = "REFS"
  show ATTESTATION   = "ATTESTATION"
  show POLICY        = "POLICY"
  show LBRACE        = "LBRACE"
  show RBRACE        = "RBRACE"
  show COLON         = "COLON"
  show EQUALS        = "EQUALS"
  show (IDENT s)     = "IDENT(" ++ s ++ ")"
  show (STRING s)    = "STRING(\"" ++ s ++ "\")"
  show (NUMBER n)    = "NUMBER(" ++ show n ++ ")"
  show (HASH alg v)  = "HASH(" ++ alg ++ ":" ++ v ++ ")"
  show EOF           = "EOF"

public export
Eq Token where
  MANIFEST     == MANIFEST     = True
  REFS         == REFS         = True
  ATTESTATION  == ATTESTATION  = True
  POLICY       == POLICY       = True
  LBRACE       == LBRACE       = True
  RBRACE       == RBRACE       = True
  COLON        == COLON        = True
  EQUALS       == EQUALS       = True
  (IDENT a)    == (IDENT b)    = a == b
  (STRING a)   == (STRING b)   = a == b
  (NUMBER a)   == (NUMBER b)   = a == b
  (HASH a1 v1) == (HASH a2 v2) = a1 == a2 && v1 == v2
  EOF          == EOF          = True
  _            == _            = False

--------------------------------------------------------------------------------
-- Lexer Errors
--------------------------------------------------------------------------------

||| Errors that can occur during lexing
public export
data LexError
  = UnexpectedChar Char Nat Nat
  | UnterminatedString Nat Nat
  | InvalidHash String Nat Nat

public export
Show LexError where
  show (UnexpectedChar c ln col)   = "Unexpected character '"
    ++ singleton c ++ "' at line " ++ show ln ++ ", col " ++ show col
  show (UnterminatedString ln col) = "Unterminated string at line "
    ++ show ln ++ ", col " ++ show col
  show (InvalidHash s ln col)      = "Invalid hash '"
    ++ s ++ "' at line " ++ show ln ++ ", col " ++ show col

--------------------------------------------------------------------------------
-- Helper: Classify Characters
--------------------------------------------------------------------------------

||| Is the character valid in an identifier or keyword?
isIdentChar : Char -> Bool
isIdentChar c = isAlpha c || isDigit c || c == '_' || c == '-'

||| Is the character valid in a hash value (hex)?
isHashChar : Char -> Bool
isHashChar c = isHexDigit c || c == '.'

--------------------------------------------------------------------------------
-- Lexer Implementation
--------------------------------------------------------------------------------

||| Match a keyword string to its token
keywordToken : String -> Token
keywordToken "manifest"    = MANIFEST
keywordToken "refs"        = REFS
keywordToken "attestation" = ATTESTATION
keywordToken "policy"      = POLICY
keywordToken other         = IDENT ("@" ++ other)

||| Collect identifier characters from the front of a list.
||| Returns (collected chars, remaining chars, count consumed).
collectIdent : List Char -> List Char -> (List Char, List Char)
collectIdent acc []        = (acc, [])
collectIdent acc (c :: cs) =
  if isIdentChar c
     then collectIdent (acc ++ [c]) cs
     else (acc, c :: cs)

||| Collect digit characters from the front of a list.
collectDigits : List Char -> List Char -> (List Char, List Char)
collectDigits acc []        = (acc, [])
collectDigits acc (c :: cs) =
  if isDigit c
     then collectDigits (acc ++ [c]) cs
     else (acc, c :: cs)

||| Collect hash value characters (hex digits and dots).
collectHashValue : List Char -> List Char -> (List Char, List Char)
collectHashValue acc []        = (acc, [])
collectHashValue acc (c :: cs) =
  if isHashChar c
     then collectHashValue (acc ++ [c]) cs
     else (acc, c :: cs)

||| Collect string characters until closing quote.
||| Returns Nothing on unterminated string.
collectString : List Char -> List Char -> Maybe (List Char, List Char)
collectString acc []              = Nothing  -- unterminated
collectString acc ('"' :: rest)   = Just (acc, rest)
collectString acc ('\\' :: c :: rest) = collectString (acc ++ [c]) rest
collectString acc (c :: rest)     = collectString (acc ++ [c]) rest

||| Core lexer: structural recursion on character list with fuel.
||| Fuel prevents non-termination on pathological input where
||| collectIdent/collectString consume 0 chars.
lexFuel : Nat -> List Char -> Nat -> Nat -> List Token
       -> Either LexError (List Token)
lexFuel Z     _  _  _   acc = Right (reverse (EOF :: acc))
lexFuel (S k) [] _  _   acc = Right (reverse (EOF :: acc))
lexFuel (S k) (' '  :: rest) ln col acc = lexFuel k rest ln (col + 1) acc
lexFuel (S k) ('\t' :: rest) ln col acc = lexFuel k rest ln (col + 4) acc
lexFuel (S k) ('\n' :: rest) ln col acc = lexFuel k rest (ln + 1) 1 acc
lexFuel (S k) ('\r' :: rest) ln col acc = lexFuel k rest ln col acc
lexFuel (S k) ('{' :: rest)  ln col acc = lexFuel k rest ln (col + 1) (LBRACE :: acc)
lexFuel (S k) ('}' :: rest)  ln col acc = lexFuel k rest ln (col + 1) (RBRACE :: acc)
lexFuel (S k) (':' :: rest)  ln col acc = lexFuel k rest ln (col + 1) (COLON :: acc)
lexFuel (S k) ('=' :: rest)  ln col acc = lexFuel k rest ln (col + 1) (EQUALS :: acc)
lexFuel (S k) ('@' :: rest)  ln col acc =
  let (chars, rest') = collectIdent [] rest
      kw = pack chars
      consumed = length chars
  in lexFuel k rest' ln (col + 1 + consumed) (keywordToken kw :: acc)
lexFuel (S k) ('"' :: rest)  ln col acc =
  case collectString [] rest of
    Nothing              => Left (UnterminatedString ln col)
    Just (chars, rest')  =>
      let consumed = length chars + 2  -- include quotes
      in lexFuel k rest' ln (col + consumed) (STRING (pack chars) :: acc)
lexFuel (S k) (c :: rest) ln col acc =
  if isDigit c
     then let (digits, rest') = collectDigits [c] rest
              numStr = pack digits
              consumed = length digits
          in case parseInteger numStr of
               Just n  => lexFuel k rest' ln (col + consumed) (NUMBER n :: acc)
               Nothing => Left (UnexpectedChar c ln col)
     else if isAlpha c
        then let (chars, rest') = collectIdent [c] rest
                 ident = pack chars
                 consumed = length chars
             in -- Check if this is algo:hexvalue (a hash literal)
                case rest' of
                  (':' :: rest'') =>
                    let (hchars, rest''') = collectHashValue [] rest''
                        hval = pack hchars
                        hconsumed = consumed + 1 + length hchars
                    in if length hchars > 0
                          then lexFuel k rest''' ln (col + hconsumed) (HASH ident hval :: acc)
                          else lexFuel k rest' ln (col + consumed) (IDENT ident :: acc)
                  _ => lexFuel k rest' ln (col + consumed) (IDENT ident :: acc)
        else Left (UnexpectedChar c ln col)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

||| Lex an A2ML input string into a list of tokens.
||| Uses fuel equal to input length + 1 to guarantee termination.
public export
lex : String -> Either LexError (List Token)
lex input =
  let chars = unpack input
      fuel  = length chars + 1
  in lexFuel fuel chars 1 1 []
