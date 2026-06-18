||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Util.HexProof - Stage 2.3: structural soundness of the hex codec.
|||
||| The full round-trip `hexStringToBytes (bytesToHex bs) = Just bs` cannot carry a
||| compile-time theorem: it threads through `pack`/`unpack` (primitives with no
||| equational theory - the same wall as the production lexer round-trip) and, per
||| byte, through primitive `Bits8` `div`/`mod`/`*`/`+`. What IS provable, axiom-free,
||| is the *structural* core at the `List Char` level - that `parsePairs` inverts
||| `bytesToHexChars` exactly, GIVEN the one irreducible per-byte fact: that a byte's
||| two hex characters parse back to it. That per-byte fact (the `Bits8` boundary) is
||| isolated as an explicit hypothesis `HexByteRoundtrip`, never assumed silently -
||| the IO<->pure-bridge discipline of Stage 1.3. No `believe_me`, no `postulate`.
module Ochrance.Util.HexProof

import Data.List
import Ochrance.Util.Hex

%default total

||| The isolated primitive-`Bits8` boundary: a byte's two hex characters parse back
||| to the byte. Discharging it needs the `Bits8` div/mod + nibble round-trip, which
||| rests on primitive machine arithmetic with no equational theory - so it is named
||| as an explicit hypothesis here rather than faked. (A concrete combiner would
||| discharge it by 256-way computation, outside the type theory.)
public export
HexByteRoundtrip : Type
HexByteRoundtrip =
  (b : Bits8) ->
  hexPairToByte (nibbleToHexChar (b `div` 16)) (nibbleToHexChar (b `mod` 16)) = Just b

||| STRUCTURAL SOUNDNESS (Stage 2.3): `parsePairs` inverts `bytesToHexChars` exactly,
||| given the per-byte boundary `hyp`. The list recursion is fully machine-checked;
||| only the `Bits8` per-byte step is assumed (and isolated in `hyp`). This is the
||| honest core of `hexStringToBytes (bytesToHex bs) = Just bs`, modulo the
||| `pack`/`unpack` primitive wall that the `String` form would additionally cross.
export
parsePairsRoundtrip : (hyp : HexByteRoundtrip) -> (bs : List Bits8) ->
                      parsePairs (bytesToHexChars bs) = Just bs
parsePairsRoundtrip hyp []          = Refl
parsePairsRoundtrip hyp (b :: rest) =
  rewrite hyp b in
  rewrite parsePairsRoundtrip hyp rest in Refl
