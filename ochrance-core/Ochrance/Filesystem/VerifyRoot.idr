||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.VerifyRoot - the root-comparing verify path (executable).
|||
||| This is the *plumbing* that the soundness theorem `merkleRootVerifyHashSound`
||| licenses: pad an arbitrary-length block-hash list to a power-of-two leaf `Vect`,
||| build the Merkle tree, and compare its root to a committed root (e.g.
||| `FSSnapshot.rootHash`). Engineering, not a proof - the *soundness* of accepting on
||| a root match is `Ochrance.Filesystem.VerifyMerkle.merkleRootVerifyHashSound`
||| (modulo its named `CollisionResistant` / `DecodeInjective` boundaries and the
||| primitive `==`). All total.
module Ochrance.Filesystem.VerifyRoot

import Data.Vect

import Ochrance.A2ML.Types
import Ochrance.Filesystem.Types
import Ochrance.Filesystem.Merkle
import Ochrance.Filesystem.VerifyMerkle

%default total

--------------------------------------------------------------------------------
-- Power-of-two leaf layout
--------------------------------------------------------------------------------

||| Pad (or truncate) a list to an exact length, filling any shortfall with `pad`.
||| Structural on the target length; the layout below always calls it with a target
||| >= the list length, so it never truncates in practice.
public export
padToLength : (m : Nat) -> a -> List a -> Vect m a
padToLength Z     _   _         = []
padToLength (S k) pad []        = pad :: padToLength k pad []
padToLength (S k) pad (x :: xs) = x :: padToLength k pad xs

||| Smallest exponent `n` with `k <= 2^n`, by doubling (fuelled by `k` for totality).
nextPow2ExpGo : (fuel, n, acc, k : Nat) -> Nat
nextPow2ExpGo Z     n _   _ = n
nextPow2ExpGo (S f) n acc k = if k <= acc then n else nextPow2ExpGo f (S n) (acc + acc) k

public export
nextPow2Exp : Nat -> Nat
nextPow2Exp k = nextPow2ExpGo k 0 1 k

||| Lay an arbitrary-length block-hash list out as a power-of-two leaf vector, padding
||| with `emptyHash`. The dependent pair carries the chosen height `n`.
public export
layoutLeaves : List HashBytes -> (n : Nat ** Vect (power 2 n) HashBytes)
layoutLeaves xs = let n = nextPow2Exp (length xs)
                  in (n ** padToLength (power 2 n) emptyHash xs)

--------------------------------------------------------------------------------
-- Root-comparing verify path
--------------------------------------------------------------------------------

||| Root-based block verification: build the padded Merkle tree from the block hashes
||| and compare its root to the expected (committed) root. `True` iff they match.
public export
verifyByRoot : Combiner -> List HashBytes -> HashBytes -> Bool
verifyByRoot h blocks expectedRoot =
  let (n ** leaves) = layoutLeaves blocks
  in rootHashWith h (buildMerkleTree {n} leaves) == expectedRoot

||| The fully live version: decode A2ML `Hash`es to bytes (via `hashToBytes`), then
||| root-verify. `Nothing` if any hash is malformed; `Just b` with the match result.
public export
verifyByRootHash : Combiner -> List Hash -> Hash -> Maybe Bool
verifyByRootHash h blocks expected =
  do blockBytes <- traverse hashToBytes blocks
     expBytes   <- hashToBytes expected
     pure (verifyByRoot h blockBytes expBytes)

--------------------------------------------------------------------------------
-- Wiring to FSState / FSSnapshot
--------------------------------------------------------------------------------

||| Indices `[0, 1, ..., n-1]`.
upTo : Nat -> List Nat
upTo Z     = []
upTo (S k) = upTo k ++ [k]

||| Collect a filesystem's block hashes in index order; `Nothing` if any is absent.
public export
fsBlockHashes : FSState -> Maybe (List Hash)
fsBlockHashes fs = traverse fs.blockHash (upTo fs.numBlocks)

||| The runtime root-verify path: check a filesystem against a snapshot's committed
||| Merkle root. `Nothing` if a block is missing or a hash is malformed; otherwise
||| `Just` the match decision. Soundness: `merkleRootVerifyHashSound`.
public export
verifySnapshotRoot : Combiner -> FSState -> FSSnapshot -> Maybe Bool
verifySnapshotRoot h fs snap =
  do blocks <- fsBlockHashes fs
     verifyByRootHash h blocks snap.rootHash
