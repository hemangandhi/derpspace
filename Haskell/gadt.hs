{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators, TypeFamilies, PolyKinds #-}

import Data.Type.Equality

data Nat :: * where
  ZNat :: Nat
  SNat :: Nat -> Nat

data FixedVec :: * -> Nat -> * where
  Empty :: FixedVec a ZNat
  Cons :: a -> FixedVec a n -> FixedVec a (SNat n)

toInt :: Num n => Nat -> n
toInt ZNat     = 0
toInt (SNat x) = 1 + (toInt x)

type family Plus (x :: Nat) (y :: Nat) :: Nat
type instance Plus ZNat     m = m
type instance Plus (SNat n) m = SNat (Plus n m)

fconcat :: FixedVec a n -> FixedVec a m -> FixedVec a (Plus n m)
fconcat Empty       ys = ys
fconcat (Cons x xs) ys = Cons x $ fconcat xs ys

data ReifyNat (n :: Nat) where
  Zeify :: ReifyNat ZNat
  Seify :: ReifyNat n -> ReifyNat (SNat n)

reifyVecLen :: FixedVec a n -> ReifyNat n
reifyVecLen Empty       = Zeify
reifyVecLen (Cons _ xs) = Seify $ reifyVecLen xs

sumSuccRight :: ReifyNat x -> ReifyNat y
  -> (SNat (Plus x y)) :~: (Plus x (SNat y))
sumSuccRight Zeify     Zeify     = Refl
sumSuccRight (Seify n) Zeify     = apply Refl
  $ sumSuccRight n Zeify
sumSuccRight Zeify     (Seify n) = apply Refl
  $ sumSuccRight Zeify n
sumSuccRight (Seify m) (Seify n) = apply Refl
  $ sumSuccRight m (Seify n)

fdup :: FixedVec a n -> FixedVec a (Plus n n)
fdup Empty       = Empty
fdup (Cons x xs) = case
  sumSuccRight (reifyVecLen xs) (reifyVecLen xs) of
    Refl -> Cons x (Cons x (fdup xs))
