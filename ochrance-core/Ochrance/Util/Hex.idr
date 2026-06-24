-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Util.Hex - Hexadecimal encoding/decoding utilities
|||
||| Provides functions to convert between hex strings and bytes

module Ochrance.Util.Hex

import Data.Vect
import Data.List
import Data.String
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- Hex Character Parsing
--------------------------------------------------------------------------------

||| Parse a single hex character to its nibble value (0-15)
export
hexCharToNibble : Char -> Maybe Bits8
hexCharToNibble c = case c of
  '0' => Just 0;  '1' => Just 1;  '2' => Just 2;  '3' => Just 3
  '4' => Just 4;  '5' => Just 5;  '6' => Just 6;  '7' => Just 7
  '8' => Just 8;  '9' => Just 9;  'a' => Just 10; 'b' => Just 11
  'c' => Just 12; 'd' => Just 13; 'e' => Just 14; 'f' => Just 15
  'A' => Just 10; 'B' => Just 11; 'C' => Just 12; 'D' => Just 13
  'E' => Just 14; 'F' => Just 15
  _ => Nothing

||| Parse two hex characters into a byte
export
hexPairToByte : Char -> Char -> Maybe Bits8
hexPairToByte hi lo = do
  hiNibble <- hexCharToNibble hi
  loNibble <- hexCharToNibble lo
  -- Combine nibbles: hi * 16 + lo
  pure ((hiNibble * 16) + loNibble)

--------------------------------------------------------------------------------
-- Hex String Parsing
--------------------------------------------------------------------------------

||| Parse pairs of characters from a list
public export
parsePairs : List Char -> Maybe (List Bits8)
parsePairs [] = Just []
parsePairs [_] = Nothing  -- Odd number of chars
parsePairs (hi :: lo :: rest) = do
  byte <- hexPairToByte hi lo
  restBytes <- parsePairs rest
  pure (byte :: restBytes)

||| Parse a hex string into a list of bytes
export
hexStringToBytes : String -> Maybe (List Bits8)
hexStringToBytes s = parsePairs (unpack s)

||| Parse a hex string into exactly N bytes
export
hexStringToVect : (n : Nat) -> String -> Maybe (Vect n Bits8)
hexStringToVect n s = do
  bytes <- hexStringToBytes s
  case length bytes == n of
    False => Nothing
    True => toVect n bytes
  where
    toVect : (n : Nat) -> List Bits8 -> Maybe (Vect n Bits8)
    toVect Z [] = Just []
    toVect (S k) (x :: xs) = do
      rest <- toVect k xs
      pure (x :: rest)
    toVect _ _ = Nothing

--------------------------------------------------------------------------------
-- Hex Encoding
--------------------------------------------------------------------------------

||| Convert a nibble (0-15) to a hex character
export
nibbleToHexChar : Bits8 -> Char
nibbleToHexChar n = case n `mod` 16 of
  0 => '0'; 1 => '1'; 2 => '2'; 3 => '3'; 4 => '4'; 5 => '5'
  6 => '6'; 7 => '7'; 8 => '8'; 9 => '9'; 10 => 'a'; 11 => 'b'
  12 => 'c'; 13 => 'd'; 14 => 'e'; _ => 'f'

||| Convert a byte to two hex characters
export
byteToHexPair : Bits8 -> (Char, Char)
byteToHexPair b =
  -- Extract nibbles using division and modulo
  let hi = b `div` 16
      lo = b `mod` 16
  in (nibbleToHexChar hi, nibbleToHexChar lo)

||| The two hex characters of a single byte, as a list. Top-level (not a
||| `where`-local) and inlined to `[c1, c2]` so it reduces definitionally - this
||| is what `HexProof` reasons about.
public export
toHexPair : Bits8 -> List Char
toHexPair b = [nibbleToHexChar (b `div` 16), nibbleToHexChar (b `mod` 16)]

||| Bytes to hex characters, by explicit structural recursion (rather than
||| `concatMap`, whose Foldable/Monoid layer does not reduce definitionally - the
||| same hazard as `traverse_`). This is the round-trip partner of `parsePairs`.
public export
bytesToHexChars : List Bits8 -> List Char
bytesToHexChars []        = []
bytesToHexChars (b :: bs) = toHexPair b ++ bytesToHexChars bs

||| Convert bytes to hex string
export
bytesToHex : List Bits8 -> String
bytesToHex bytes = pack (bytesToHexChars bytes)

||| Convert a vector of bytes to hex string
export
vectToHex : Vect n Bits8 -> String
vectToHex v = bytesToHex (toList v)
