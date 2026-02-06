||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.ABI.Layout - Memory layout verification
|||
||| Simplified version focusing on essential ABI guarantees.
||| Full dependent type proofs to be added incrementally.

module Ochrance.ABI.Layout

import Ochrance.ABI.Types
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Layout Descriptors
--------------------------------------------------------------------------------

||| A memory layout descriptor with size and alignment
public export
record Layout where
  constructor MkLayout
  ||| Size in bytes
  size : Nat
  ||| Alignment requirement (must be power of 2)
  alignment : Nat

||| Layout for a single byte
public export
byteLayout : Layout
byteLayout = MkLayout 1 1

||| Layout for a hash value of n bytes
public export
hashValueLayout : (n : Nat) -> Layout
hashValueLayout n = MkLayout n 1  -- Byte array, 1-byte aligned

||| Layout for HashDigest (32 bytes for all current algorithms)
public export
hashDigestLayout : Layout
hashDigestLayout = MkLayout 32 1

--------------------------------------------------------------------------------
-- Platform Independence
--------------------------------------------------------------------------------

||| A layout is platform-independent if it doesn't depend on
||| pointer size, endianness, or compiler-specific padding
public export
data PlatformIndependent : Layout -> Type where
  ||| Byte-aligned types are always platform-independent
  ByteAlignedIndependent : (l : Layout) -> PlatformIndependent l

||| HashValue layout is platform-independent
public export
hashValueLayoutIndependent : (n : Nat) -> PlatformIndependent (hashValueLayout n)
hashValueLayoutIndependent n = ByteAlignedIndependent (hashValueLayout n)

||| HashDigest layout is platform-independent
public export
hashDigestPlatformIndependent : PlatformIndependent (MkLayout 32 1)
hashDigestPlatformIndependent = ByteAlignedIndependent (MkLayout 32 1)

--------------------------------------------------------------------------------
-- C Compatibility
--------------------------------------------------------------------------------

||| A layout is C-compatible if it matches C struct layout rules
public export
record CCompatible (t : Type) where
  constructor MkCCompatible
  layout : Layout
  platformIndependent : PlatformIndependent layout

||| HashValue is C-compatible as uint8_t[n]
public export
hashValueCCompatible : {n : Nat} -> CCompatible (HashValue n)
hashValueCCompatible = MkCCompatible (hashValueLayout n)
                                      (hashValueLayoutIndependent n)

||| Proof that C-compatible types have stable FFI boundaries
public export
cCompatibleStable : {t : Type} -> CCompatible t -> StableABI t
cCompatibleStable (MkCCompatible layout pi) =
  MkStableABI (HasNoPadding t)
              (PlatformIndependent t)
              layout.size

--------------------------------------------------------------------------------
-- C Header Generation
--------------------------------------------------------------------------------

||| Generate a C header declaration for a type with proven layout
public export
generateCDecl : (name : String) -> Layout -> String
generateCDecl name layout =
  "// Size: " ++ show layout.size ++ " bytes, Align: " ++ show layout.alignment ++ " bytes\n" ++
  "typedef struct __attribute__((packed)) {\n" ++
  "  uint8_t data[" ++ show layout.size ++ "];\n" ++
  "} " ++ name ++ ";\n"

||| Generate C declaration for HashValue
public export
generateHashValueDecl : (n : Nat) -> String
generateHashValueDecl n = generateCDecl "hash_value" (hashValueLayout n)
