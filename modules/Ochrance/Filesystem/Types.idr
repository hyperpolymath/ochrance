||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.Filesystem.Types - Filesystem state and block types
|||
||| Defines the core types for filesystem verification:
||| blocks, filesystem state, and manifest snapshots.

module Ochrance.Filesystem.Types

import Data.Vect
import Ochrance.A2ML.Types

%default total

--------------------------------------------------------------------------------
-- Block Types
--------------------------------------------------------------------------------

||| A fixed-size block of bytes (4096 bytes = standard block size)
public export
BlockSize : Nat
BlockSize = 4096

||| Raw block data
public export
Block : Type
Block = Vect BlockSize Bits8

||| Block index (position in filesystem)
public export
BlockIndex : Type
BlockIndex = Nat

--------------------------------------------------------------------------------
-- Filesystem State
--------------------------------------------------------------------------------

||| The filesystem state to be verified.
||| Parameterized by the number of blocks.
public export
record FSState where
  constructor MkFSState
  numBlocks : Nat
  blockHash : BlockIndex -> Maybe Hash   -- hash of block at index
  metadata  : ManifestData               -- associated manifest metadata

||| A snapshot of the filesystem for comparison
public export
record FSSnapshot where
  constructor MkFSSnapshot
  rootHash   : Hash
  blockCount : Nat
  refs       : List Ref
