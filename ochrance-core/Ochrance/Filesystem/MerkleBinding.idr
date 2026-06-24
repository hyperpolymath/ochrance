||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.MerkleBinding - Stage 1.4: the crypto-binding interface.
|||
||| Decision D2. The *security* half of the Merkle argument is binding: the root
||| commits to the leaves, so two leaf vectors with the same root must be the same
||| vector. Computationally this rests on collision resistance of the combiner,
||| which is not a constructive fact - so it is modelled as an explicit, typed
||| Idris hypothesis `CollisionResistant h` (injectivity of the combiner), and the
||| binding theorem `merkleBinding` is *discharged against it*, never asserted.
||| `CollisionResistant h` itself is the sole assumed boundary - no `postulate`,
||| no `believe_me`. It is the irreducible cryptographic trust root: full injectivity
||| is pigeonhole-false for a compressing combiner, so it can NEVER be discharged by
||| proof (see `Ochrance.Filesystem.MerkleAssumption`) - it stays an explicit
||| hypothesis, supplied at the verification boundary. Everything from the hypothesis
||| through `merkleBinding` is fully machine-checked here.
|||
||| The proof rides on the root-fold law (Stage 1.2): because the root is a fold
||| over the leaves (`foldRoot`), injectivity of the combiner lifts, level by
||| level, to injectivity of the whole fold - which is exactly binding.
module Ochrance.Filesystem.MerkleBinding

import Data.Vect
import Data.Nat

import Ochrance.Filesystem.Merkle
import Ochrance.Filesystem.MerkleBuild
import Ochrance.Util.VectLemmas

%default total

--------------------------------------------------------------------------------
-- The typed crypto-binding hypothesis (D2)
--------------------------------------------------------------------------------

||| Collision resistance of a combiner, modelled as *injectivity*: equal outputs
||| force equal inputs, pairwise. This is the type-level content of collision
||| resistance - the strongest fact one can name without leaving the model into
||| concrete computation. It is the single assumed hypothesis of the binding theorem,
||| and the irreducible cryptographic trust root: full injectivity is pigeonhole-false
||| for a compressing combiner, so it is never dischargeable by proof (see
||| `MerkleAssumption.constNotCollisionResistant`, which shows it has teeth).
public export
CollisionResistant : Combiner -> Type
CollisionResistant h =
  (a, b, c, d : HashBytes) -> h a b = h c d -> (a = c, b = d)

--------------------------------------------------------------------------------
-- foldRoot unfolding at a successor height
--------------------------------------------------------------------------------

||| `foldRoot` at height `S k`, exposed on an explicit split: when `splitAt` cuts
||| the (transported) leaf vector into `(l, r)`, the fold is exactly
||| `h (foldRoot l) (foldRoot r)`. This is the definitional unfolding of `foldRoot`
||| made usable as an equation to rewrite with (it consumes the *same* split that
||| `rootFoldLaw` does).
foldRootSplitEq : (h : Combiner) -> {k : Nat} ->
  (hs : Vect (power 2 (S k)) HashBytes) ->
  (l, r : Vect (power 2 k) HashBytes) ->
  splitAt (power 2 k)
          (replace {p = \w => Vect w HashBytes} (powerTwoSucc k) hs) = (l, r) ->
  foldRoot h {n = S k} hs = h (foldRoot h {n = k} l) (foldRoot h {n = k} r)
foldRootSplitEq h hs l r sp
    with (splitAt (power 2 k)
                  (replace {p = \w => Vect w HashBytes} (powerTwoSucc k) hs))
  _ | (l', r') = rewrite cong fst sp in rewrite cong snd sp in Refl

--------------------------------------------------------------------------------
-- Binding: equal roots force equal leaves (discharged against CollisionResistant)
--------------------------------------------------------------------------------

||| Inductive step on the already-split halves: from injectivity of the combiner
||| (`cr`) applied to the two sub-roots, plus the height-`k` binding IH, conclude
||| both halves coincide. (Mirrors the `*Split` helpers in `MerkleBuild`; the IH
||| is a higher-order argument so the recursion stays structural in the height.)
bindSplit : (h : Combiner) -> CollisionResistant h -> {k : Nat} ->
  (l1, r1, l2, r2 : Vect (power 2 k) HashBytes) ->
  ((u, v : Vect (power 2 k) HashBytes) ->
     foldRoot h {n = k} u = foldRoot h {n = k} v -> u = v) ->
  h (foldRoot h {n = k} l1) (foldRoot h {n = k} r1)
    = h (foldRoot h {n = k} l2) (foldRoot h {n = k} r2) ->
  (l1 = l2, r1 = r2)
bindSplit h cr l1 r1 l2 r2 ih eqRoot =
  let (eL, eR) = cr (foldRoot h l1) (foldRoot h r1)
                    (foldRoot h l2) (foldRoot h r2) eqRoot
  in (ih l1 l2 eL, ih r1 r2 eR)

||| BINDING (Stage 1.4 / D2), discharged against `CollisionResistant h`: two leaf
||| vectors that fold to the same Merkle root are equal. The root therefore
||| *binds* the leaves - the commitment is unequivocal - modulo the single typed
||| hypothesis `cr`, which Stage 4 discharges for the cryptographic combiner. The
||| combiner is otherwise opaque, so this holds at XOR and BLAKE3 alike.
|||
||| Composed with `rootFoldLaw`
||| (`rootHashWith h (buildMerkleTree hs) = foldRoot h hs`), this gives binding for
||| the built tree's actual root for free.
export
merkleBinding : (h : Combiner) -> CollisionResistant h -> {n : Nat} ->
                (xs, ys : Vect (power 2 n) HashBytes) ->
                foldRoot h {n} xs = foldRoot h {n} ys -> xs = ys
merkleBinding h cr {n = Z} [x] [y] prf = cong (\z => z :: []) prf
merkleBinding h cr {n = S k} xs ys prf =
  let eq : (power 2 (S k) = power 2 k + power 2 k)
      eq = powerTwoSucc k
      xs' : Vect (power 2 k + power 2 k) HashBytes
      xs' = replace {p = \w => Vect w HashBytes} eq xs
      ys' : Vect (power 2 k + power 2 k) HashBytes
      ys' = replace {p = \w => Vect w HashBytes} eq ys
      l1 : Vect (power 2 k) HashBytes
      l1 = fst (splitAt (power 2 k) xs')
      r1 : Vect (power 2 k) HashBytes
      r1 = snd (splitAt (power 2 k) xs')
      l2 : Vect (power 2 k) HashBytes
      l2 = fst (splitAt (power 2 k) ys')
      r2 : Vect (power 2 k) HashBytes
      r2 = snd (splitAt (power 2 k) ys')
      spx : (splitAt (power 2 k) xs' = (l1, r1))
      spx = splitAtEta (power 2 k) xs'
      spy : (splitAt (power 2 k) ys' = (l2, r2))
      spy = splitAtEta (power 2 k) ys'
      ufx : (foldRoot h {n = S k} xs
             = h (foldRoot h {n = k} l1) (foldRoot h {n = k} r1))
      ufx = foldRootSplitEq h xs l1 r1 spx
      ufy : (foldRoot h {n = S k} ys
             = h (foldRoot h {n = k} l2) (foldRoot h {n = k} r2))
      ufy = foldRootSplitEq h ys l2 r2 spy
      eqRoot : (h (foldRoot h {n = k} l1) (foldRoot h {n = k} r1)
                = h (foldRoot h {n = k} l2) (foldRoot h {n = k} r2))
      eqRoot = trans (sym ufx) (trans prf ufy)
      halves : (l1 = l2, r1 = r2)
      halves = bindSplit h cr l1 r1 l2 r2 (merkleBinding h cr {n = k}) eqRoot
      catEq : (l1 ++ r1 = l2 ++ r2)
      catEq = rewrite fst halves in cong (\z => l2 ++ z) (snd halves)
      xs'Cat : (xs' = l1 ++ r1)
      xs'Cat = sym (splitAtConcat (power 2 k) xs')
      ys'Cat : (ys' = l2 ++ r2)
      ys'Cat = sym (splitAtConcat (power 2 k) ys')
      transEq : (xs' = ys')
      transEq = trans xs'Cat (trans catEq (sym ys'Cat))
  in replaceInj {p = \w => Vect w HashBytes} eq transEq

||| Binding for the *built tree's* root, as a direct corollary: two leaf vectors
||| whose `buildMerkleTree`s carry the same root are equal. Composes
||| `merkleBinding` with `rootFoldLaw`, which rewrites each tree root to its leaf
||| fold. This is the form a verifier actually holds - "same committed root ⇒ same
||| data" - again modulo only the typed `cr` hypothesis (Stage 4).
export
merkleBindingTree : (h : Combiner) -> CollisionResistant h -> {n : Nat} ->
                    (xs, ys : Vect (power 2 n) HashBytes) ->
                    rootHashWith h (buildMerkleTree {n} xs)
                      = rootHashWith h (buildMerkleTree {n} ys) ->
                    xs = ys
merkleBindingTree h cr xs ys prf =
  merkleBinding h cr xs ys
    (trans (sym (rootFoldLaw h xs)) (trans prf (rootFoldLaw h ys)))
