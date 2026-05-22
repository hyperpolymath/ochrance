||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.Types — Verified Storage Models.
|||
||| This module defines the formal types used to verify the integrity 
||| of block-level storage. It provides the data structures for tracking 
||| block hashes and comparing filesystem snapshots.

module Ochrance.Filesystem.Types

import Data.Vect
import Ochrance.A2ML.Types

%default total

--------------------------------------------------------------------------------
-- Block Layer
--------------------------------------------------------------------------------

||| STANDARD BLOCK SIZE: Defined as 4096 bytes (4KB).
public export
BlockSize : Nat
BlockSize = 4096

||| BLOCK REPRESENTATION: A fixed-size vector of bytes.
public export
Block : Type
Block = Vect BlockSize Bits8

||| BLOCK ADDRESS: A unique natural number index within the filesystem.
public export
BlockIndex : Type
BlockIndex = Nat

--------------------------------------------------------------------------------
-- Verification State
--------------------------------------------------------------------------------

||| FS-STATE: The complete verified state of a filesystem.
||| Tracks the total number of blocks and maps indices to their expected hashes.
public export
record FSState where
  constructor MkFSState
  numBlocks : Nat
  blockHash : BlockIndex -> Maybe Hash   -- Integrity map
  metadata  : ManifestData               -- Associated provenance metadata

||| SNAPSHOT: A point-in-time capture of the filesystem root hash and block count.
||| Used for detecting unauthorized mutations (e.g. offline tampering).
public export
record FSSnapshot where
  constructor MkFSSnapshot
  rootHash   : Hash
  blockCount : Nat
  refs       : List Ref
