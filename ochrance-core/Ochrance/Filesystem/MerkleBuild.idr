||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Filesystem.MerkleBuild - buildMerkleTree / getLeafHash correctness.
|||
||| Proves `buildGetLeaf`: reading leaf `i` back out of `buildMerkleTree hs`
||| returns exactly `index i hs`. This is the round-trip soundness of the
||| balanced Merkle constructor against the by-index leaf accessor.

module Ochrance.Filesystem.MerkleBuild

import Data.Vect
import Data.Fin
import Data.Fin.Split
import Data.Fin.Properties
import Data.Nat

import Ochrance.A2ML.Types
import Ochrance.Filesystem.Merkle
import Ochrance.Util.VectLemmas

%default total

--------------------------------------------------------------------------------
-- Bridging the Bool `idx < power 2 k` to the splitSum case
--
-- For Nat, `(<)` is the Ord-interface default: `x < y = compare x y == LT`,
-- and `compare = compareNat`. So `idx < m` is `compareNat idx m == LT`.
--------------------------------------------------------------------------------

||| `compareNat a b` is `LT` whenever `a` is strictly below `b`.
compareNatLT : (a, b : Nat) -> LT a b -> compareNat a b = LT
compareNatLT Z     (S _) _          = Refl
compareNatLT (S a) (S b) (LTESucc p) = compareNatLT a b p

||| Any `Fin m` index is `< m` as a Bool. (`<` here is the Nat Ord default.)
finBoundLT : (a : Fin m) -> (finToNat a < m) = True
finBoundLT a =
  rewrite compareNatLT (finToNat a) m (elemSmallerThanBound a) in Refl

||| `compareNat (p + x) p` is never `LT`: `p + x` is at least `p`.
compareNatPlusNotLT : (p, x : Nat) ->
                      Either (compareNat (p + x) p = EQ) (compareNat (p + x) p = GT)
compareNatPlusNotLT Z     Z     = Left Refl
compareNatPlusNotLT Z     (S _) = Right Refl
compareNatPlusNotLT (S p) x     = compareNatPlusNotLT p x

||| `(p + x) < p` is `False`: a left-padded index never falls in the left half.
plusNotLT : (p, x : Nat) -> ((p + x) < p) = False
plusNotLT p x = case compareNatPlusNotLT p x of
  Left  eq => rewrite eq in Refl
  Right eq => rewrite eq in Refl

--------------------------------------------------------------------------------
-- buildMerkleTree / getLeafHash round-trip soundness
--------------------------------------------------------------------------------

||| Core of the inductive step at height `S k`, stated on the *already split*
||| halves so the `splitAt`/`splitSum` reasoning is decoupled from the transport.
|||
||| `left ++ right` is the (transported) leaf vector and `j` is the (transported)
||| index into it; the two IH calls discharge each subtree.
buildGetLeafSplit :
  {k : Nat} ->
  (left, right : Vect (power 2 k) HashBytes) ->
  (j : Fin (power 2 k + power 2 k)) ->
  ((bl : Vect (power 2 k) HashBytes) -> (a : Fin (power 2 k)) ->
     getLeafHash (buildMerkleTree {n = k} bl) (finToNat a) = Just (index a bl)) ->
  getLeafHash (Node (buildMerkleTree {n = k} left) (buildMerkleTree {n = k} right))
              (finToNat j)
    = Just (index j (left ++ right))
buildGetLeafSplit {k} left right j ih with (splitSum {m = power 2 k} {n = power 2 k} j)
                                         proof splitEq
  _ | Left a =
        -- j = weakenN (power 2 k) a ; index lands in `left`, THEN branch taken.
        let jIs : (j = weakenN (power 2 k) a)
            jIs = trans (sym (indexOfSplitSumInverse {m = power 2 k} {n = power 2 k} j))
                        (cong indexSum splitEq)
            finEq : (finToNat j = finToNat a)
            finEq = trans (cong finToNat jIs) (finToNatWeakenNNeutral (power 2 k) a)
            idxEq : (index j (left ++ right) = index a left)
            idxEq = trans (cong (\z => index z (left ++ right)) jIs)
                          (indexAppendLeft a left right)
        in rewrite finEq in
           rewrite finBoundLT a in
           rewrite idxEq in
           ih left a
  _ | Right b =
        -- j = shift (power 2 k) b ; index lands in `right`, ELSE branch taken.
        let jIs : (j = shift (power 2 k) b)
            jIs = trans (sym (indexOfSplitSumInverse {m = power 2 k} {n = power 2 k} j))
                        (cong indexSum splitEq)
            finEq : (finToNat j = power 2 k + finToNat b)
            finEq = trans (cong finToNat jIs) (finToNatShift (power 2 k) b)
            idxEq : (index j (left ++ right) = index b right)
            idxEq = trans (cong (\z => index z (left ++ right)) jIs)
                          (indexAppendRight b left right)
        in rewrite finEq in
           rewrite plusNotLT (power 2 k) (finToNat b) in
           rewrite minusPlus {n = finToNat b} (power 2 k) in
           rewrite idxEq in
           ih right b

||| Reading leaf `i` back out of `buildMerkleTree hs` returns `index i hs`.
||| The constructor places leaf `i` at exactly position `finToNat i`.
export
buildGetLeaf : {n : Nat} -> (hs : Vect (power 2 n) HashBytes) -> (i : Fin (power 2 n)) ->
               getLeafHash (buildMerkleTree {n} hs) (finToNat i) = Just (index i hs)
buildGetLeaf {n = Z} [h] FZ = Refl
buildGetLeaf {n = S k} hs i with (splitAt (power 2 k)
                                    (replace {p = \x => Vect x HashBytes} (powerTwoSucc k) hs))
                                  proof splitPrf
  _ | (left, right) =
        let i' : Fin (power 2 k + power 2 k)
            i' = replace {p = Fin} (powerTwoSucc k) i
            hs' : Vect (power 2 k + power 2 k) HashBytes
            hs' = replace {p = \x => Vect x HashBytes} (powerTwoSucc k) hs
            -- The two split halves re-append to the transported vector.
            catEq : (left ++ right = hs')
            catEq = trans (sym (cong (\z => fst z ++ snd z) splitPrf))
                          (splitAtConcat (power 2 k) hs')
            -- index/finToNat are preserved by the transport.
            finEq : (finToNat i = finToNat i')
            finEq = sym (finToNatReplace (powerTwoSucc k) i)
            idxEq : (index i hs = index i' (left ++ right))
            idxEq = trans (sym (indexReplace (powerTwoSucc k) i hs))
                          (sym (cong (index i') catEq))
        in rewrite finEq in
           rewrite idxEq in
           buildGetLeafSplit left right i' buildGetLeaf

--------------------------------------------------------------------------------
-- Root-fold law (Stage 1.2): the root is a deterministic fold over the leaves
--------------------------------------------------------------------------------

||| The Merkle root expressed as a direct fold over the leaf vector, independent
||| of the tree representation: a singleton folds to its element; a `2^(S k)`
||| vector splits into halves (the *same* split `buildMerkleTree` uses) and
||| combines the two sub-folds with `h`. This is the representation-independent
||| *spec* of the root — a pure function of the leaves.
public export
foldRoot : (h : Combiner) -> {n : Nat} -> Vect (power 2 n) HashBytes -> HashBytes
foldRoot h {n = Z}   [x] = x
foldRoot h {n = S k} hs  =
  let hs' : Vect (power 2 k + power 2 k) HashBytes
      hs' = replace {p = \x => Vect x HashBytes} (powerTwoSucc k) hs
  in case splitAt (power 2 k) hs' of
       (l, r) => h (foldRoot h l) (foldRoot h r)

||| Inductive step of the root-fold law on the *already split* halves, so the
||| `rootHashWith`-of-`Node` reduction is clean and decoupled from the `with`
||| abstraction (mirrors `buildGetLeafSplit`). The IH is a higher-order argument.
rootFoldSplit : (h : Combiner) -> {k : Nat} ->
  (l, r : Vect (power 2 k) HashBytes) ->
  ((v : Vect (power 2 k) HashBytes) ->
     rootHashWith h (buildMerkleTree {n = k} v) = foldRoot h {n = k} v) ->
  rootHashWith h (Node (buildMerkleTree {n = k} l) (buildMerkleTree {n = k} r))
    = h (foldRoot h {n = k} l) (foldRoot h {n = k} r)
rootFoldSplit h l r ih = rewrite ih l in rewrite ih r in Refl

||| Root-fold law: the root of the built tree equals the direct leaf fold.
||| `rootHashWith h (buildMerkleTree hs) = foldRoot h hs`. Pairs with
||| `buildGetLeaf` to fully characterise `buildMerkleTree`, and hands the binding
||| argument a leaf-only handle on the root ("the root is a function of the
||| leaves"). The combiner `h` is opaque, so this holds for XOR and BLAKE3 alike.
export
rootFoldLaw : (h : Combiner) -> {n : Nat} -> (hs : Vect (power 2 n) HashBytes) ->
              rootHashWith h (buildMerkleTree {n} hs) = foldRoot h {n} hs
rootFoldLaw h {n = Z} [x] = Refl
rootFoldLaw h {n = S k} hs
    with (splitAt (power 2 k) (replace {p = \x => Vect x HashBytes} (powerTwoSucc k) hs))
  _ | (l, r) = rootFoldSplit h l r (rootFoldLaw h)
