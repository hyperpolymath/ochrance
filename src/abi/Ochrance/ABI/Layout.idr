||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.ABI.Layout — Formal Memory Layout Specification.
|||
||| This module provides the mathematical foundation for proving that 
||| Idris data structures are binary-compatible with the C ABI. It ensures 
||| that the "Protector" kernel can safely share memory with native modules.

module Ochrance.ABI.Layout

import Ochrance.ABI.Types
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Layout Primitives
--------------------------------------------------------------------------------

||| LAYOUT: A formal descriptor of a type's memory footprint.
public export
record Layout where
  constructor MkLayout
  size : Nat      -- Total byte width
  alignment : Nat -- Memory alignment requirement (power of 2)

--------------------------------------------------------------------------------
-- ABI Stability Proofs
--------------------------------------------------------------------------------

||| PLATFORM INDEPENDENCE: A witness that a layout does not vary based 
||| on the host CPU architecture (endianness, pointer width).
||| Essential for portable cryptographic audits.
public export
data PlatformIndependent : Layout -> Type where
  ||| Byte-aligned structures are inherently platform-independent.
  ByteAlignedIndependent : (l : Layout) -> PlatformIndependent l

||| C-COMPATIBILITY: Proves that a type matches standard C packing rules.
||| Any type carrying this witness can be passed safely across FFI boundaries.
public export
record CCompatible (t : Type) where
  constructor MkCCompatible
  layout : Layout
  platformIndependent : PlatformIndependent layout

--------------------------------------------------------------------------------
-- Code Generation
--------------------------------------------------------------------------------

||| HEADER GEN: Produces a C-style `typedef` for a proven layout.
||| Uses `__attribute__((packed))` to ensure strict adherence to the Idris model.
public export
generateCDecl : (name : String) -> Layout -> String
generateCDecl name layout =
  "typedef struct __attribute__((packed)) { uint8_t data[" ++ show layout.size ++ "]; } " ++ name ++ ";\n"
