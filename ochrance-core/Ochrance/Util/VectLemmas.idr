||| SPDX-License-Identifier: MPL-2.0
|||
||| Ochrance.Util.VectLemmas - reusable Vect/Fin lemmas for the Merkle proofs.
|||
||| index-over-append (both halves), transport cancellation for `replace`,
||| finToNat under transport, and the splitAt re-append law.
module Ochrance.Util.VectLemmas

import Data.Vect
import Data.Fin
import Data.Fin.Split
import Data.Fin.Properties

%default total

||| Indexing into the left half of an append: weakenN selects from xs.
public export
indexAppendLeft : (a : Fin m) -> (xs : Vect m e) -> (ys : Vect n e) ->
                  index (weakenN n a) (xs ++ ys) = index a xs
indexAppendLeft FZ     (_ :: _)  _  = Refl
indexAppendLeft (FS a) (_ :: xs) ys = indexAppendLeft a xs ys

||| Indexing into the right half of an append: shift selects from ys.
public export
indexAppendRight : {m : Nat} -> (b : Fin n) -> (xs : Vect m e) -> (ys : Vect n e) ->
                   index (shift m b) (xs ++ ys) = index b ys
indexAppendRight b []        _  = Refl
indexAppendRight b (_ :: xs) ys = indexAppendRight b xs ys

||| Transport cancellation: indexing a length-transported vector with the
||| matching length-transported index is the same element.
public export
indexReplace : (eq : m = n) -> (i : Fin m) -> (v : Vect m e) ->
               index (replace {p = Fin} eq i) (replace {p = \x => Vect x e} eq v) = index i v
indexReplace Refl _ _ = Refl

||| A length transport preserves the underlying Nat of a Fin.
public export
finToNatReplace : (eq : m = n) -> (i : Fin m) ->
                  finToNat (replace {p = Fin} eq i) = finToNat i
finToNatReplace Refl _ = Refl

||| splitAt's two halves re-append to the original (the concat half of splitAt).
public export
splitAtConcat : {m : Nat} -> (k : Nat) -> (xs : Vect (k + m) e) ->
                fst (splitAt {m} k xs) ++ snd (splitAt {m} k xs) = xs
splitAtConcat Z     xs        = Refl
splitAtConcat (S k) (x :: xs) with (splitAt {m} k xs) proof eq
  _ | (tk, dr) = cong (x ::) (replace {p = \z => fst z ++ snd z = xs} eq (splitAtConcat k xs))
