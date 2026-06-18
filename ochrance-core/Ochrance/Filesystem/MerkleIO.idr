||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.MerkleIO - Stage 1.3: the IO<->pure bridge.
|||
||| The production crypto path (`rootHashBytesIO` / `verifyProofIO`) runs in IO
||| and returns `Either OchranceError` to absorb BLAKE3 buffer-allocation
||| failure. IO is opaque, so we *decompose* the bridge:
|||
|||   * a pure `Either`-monad mirror of each fold (`rootHashBytesE` /
|||     `verifyProofE`) whose agreement with the proven pure spec
|||     (`rootHashWith` / `verifyProofWith`) is fully machine-checked - this is
|||     the "modulo the Either allocation-failure plumbing" content; and
|||   * the one assumed, undischargeable step (IO is opaque): that the IO fold
|||     equals `pure` of its pure mirror. This is supplied as an *explicit
|||     hypothesis argument* (`reflect`), never a `postulate`/`believe_me`, so the
|||     production-soundness results are stated honestly *conditionally* on it.
module Ochrance.Filesystem.MerkleIO

import Data.Vect
import Ochrance.Framework.Error
import Ochrance.Filesystem.Merkle

%default total

||| A pure combiner that may fail: the error-monad model of `hashPairBlake3`,
||| whose `Left` is the BLAKE3 buffer-allocation failure (z/out-of-memory).
public export
CombinerE : Type
CombinerE = HashBytes -> HashBytes -> Either OchranceError HashBytes

--------------------------------------------------------------------------------
-- Pure error-monad mirrors of the IO folds
--------------------------------------------------------------------------------

||| Pure `Either`-monad mirror of `rootHashBytesIO`: the same control flow, with
||| a pure failing combiner `cf` in place of the IO `hashPairBlake3`.
public export
rootHashBytesE : CombinerE -> MerkleTree n -> Either OchranceError HashBytes
rootHashBytesE cf (Leaf x)   = Right x
rootHashBytesE cf (Node l r) =
  case rootHashBytesE cf l of
    Left err    => Left err
    Right lHash => case rootHashBytesE cf r of
      Left err    => Left err
      Right rHash => cf lHash rHash

||| Pure `Either`-monad mirror of `verifyProofIO`.
public export
verifyProofE : CombinerE -> (root, leaf : HashBytes) -> MerkleProof -> Either OchranceError Bool
verifyProofE cf root leaf [] = Right (root == leaf)
verifyProofE cf root leaf ((GoLeft,  sib) :: rest) =
  case cf leaf sib of
    Left err     => Left err
    Right parent => verifyProofE cf root parent rest
verifyProofE cf root leaf ((GoRight, sib) :: rest) =
  case cf sib leaf of
    Left err     => Left err
    Right parent => verifyProofE cf root parent rest

--------------------------------------------------------------------------------
-- Pure bridge (machine-checked): the error-monad fold computes the spec
--------------------------------------------------------------------------------

||| If `cf` models a pure combiner `h` with no failure (`cf a b = Right (h a b)`),
||| the error-monad root fold returns exactly the proven pure root. This is the
||| machine-checked half of the bridge.
export
rootHashBytesE_spec : (h : Combiner) -> (cf : CombinerE) ->
  (model : (a, b : HashBytes) -> cf a b = Right (h a b)) ->
  (t : MerkleTree n) ->
  rootHashBytesE cf t = Right (rootHashWith h t)
rootHashBytesE_spec h cf model (Leaf x)   = Refl
rootHashBytesE_spec h cf model (Node l r) =
  rewrite rootHashBytesE_spec h cf model l in
  rewrite rootHashBytesE_spec h cf model r in
  model (rootHashWith h l) (rootHashWith h r)

||| Likewise for proof verification: the error-monad check computes the proven
||| pure Bool verdict when `cf` models `h`.
export
verifyProofE_spec : (h : Combiner) -> (cf : CombinerE) ->
  (model : (a, b : HashBytes) -> cf a b = Right (h a b)) ->
  (root, leaf : HashBytes) -> (prf : MerkleProof) ->
  verifyProofE cf root leaf prf = Right (verifyProofWith h root leaf prf)
verifyProofE_spec h cf model root leaf [] = Refl
verifyProofE_spec h cf model root leaf ((GoLeft, sib) :: rest) =
  rewrite model leaf sib in
  verifyProofE_spec h cf model root (h leaf sib) rest
verifyProofE_spec h cf model root leaf ((GoRight, sib) :: rest) =
  rewrite model sib leaf in
  verifyProofE_spec h cf model root (h sib leaf) rest

--------------------------------------------------------------------------------
-- Production connection (conditional on the thin FFI reflection hypothesis)
--------------------------------------------------------------------------------

||| Production root soundness, *conditional* on the FFI reflection hypothesis
||| `reflect` - the IO fold equals `pure` of its pure mirror. That step is the
||| sole assumed, undischargeable boundary (IO is opaque); everything else is
||| machine-checked. Given it and a model `cf` of a pure combiner `h`, the
||| production `rootHashBytesIO` computes the proven root.
export
rootHashBytesIO_spec : {io : Type -> Type} -> HasIO io => {n : Nat} ->
  (h : Combiner) -> (cf : CombinerE) ->
  (model : (a, b : HashBytes) -> cf a b = Right (h a b)) ->
  (t : MerkleTree n) ->
  (reflect : rootHashBytesIO {io} t = pure (rootHashBytesE cf t)) ->
  rootHashBytesIO {io} t = pure (Right (rootHashWith h t))
rootHashBytesIO_spec h cf model t reflect =
  trans reflect (cong pure (rootHashBytesE_spec h cf model t))

||| Production verification soundness, conditional on the FFI reflection
||| hypothesis: `verifyProofIO` computes the proven Bool verdict.
export
verifyProofIO_spec : {io : Type -> Type} -> HasIO io =>
  (h : Combiner) -> (cf : CombinerE) ->
  (model : (a, b : HashBytes) -> cf a b = Right (h a b)) ->
  (root, leaf : HashBytes) -> (prf : MerkleProof) ->
  (reflect : verifyProofIO {io} root leaf prf = pure (verifyProofE cf root leaf prf)) ->
  verifyProofIO {io} root leaf prf = pure (Right (verifyProofWith h root leaf prf))
verifyProofIO_spec h cf model root leaf prf reflect =
  trans reflect (cong pure (verifyProofE_spec h cf model root leaf prf))
