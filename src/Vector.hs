{-# LANGUAGE ExplicitForAll, GADTs, TypeFamilies, TypeOperators, TypeInType #-}

module Vector where

import Prelude hiding ((++), lookup)
import Data.Kind (Type)

data Nat :: Type where
  Zero :: Nat
  Succ :: Nat -> Nat

instance Show Nat where
  show Zero     = "0"
  show (Succ n) = show (read (show n) + 1)

n = Succ (Succ (Succ Zero))

data Vec :: Type -> Nat -> Type where
  VNil  :: ∀ (a :: Type).           Vec a 'Zero
  VCons :: ∀ (a :: Type)(n :: Nat). a -> Vec a n -> Vec a ('Succ n)

instance ∀ (a :: Type)(n :: Nat). Show a => Show (Vec a n) where
  show VNil         =  "[]"
  show (VCons x xs) =  let end = case show xs of
                             "[]"  ->  "]"
                             ys    ->  concat [", ", tail ys]
                       in concat ["[", show x, end]

x = VCons "a" ((VCons "b") VNil)
y = VCons "c" ((VCons "d") ((VCons "e") VNil))
z = x ++ y

type family (m :: Nat) + (n :: Nat) where
  Zero    + n = n
  'Succ m + n = 'Succ (m + n)

(++) :: ∀ (a :: Type)(m :: Nat)(n :: Nat). Vec a m -> Vec a n -> Vec a (m + n)
VNil      ++ v = v
VCons x u ++ v = VCons x (u ++ v)

data Fin :: Nat -> Type where
  FZero :: ∀ (n :: Nat).          Fin (Succ n)
  FSucc :: ∀ (n :: Nat). Fin n -> Fin (Succ n)

lookup :: ∀ (a :: Type)(n :: Nat). Fin n -> Vec a n -> a
--lookup  n          VNil          = undefined
lookup FZero     (VCons x _)  = x
lookup (FSucc n) (VCons _ xs) = lookup n xs

a = lookup (FSucc (FZero)) x

data (a :: k) :~: (b :: k) where
  Refl :: ∀ (k :: Type)(a :: k). a :~: a

infix 4 :~:
infix 5 <

type family (m :: Nat) < (n :: Nat) where
  _         < Zero      = False
  'Zero     < ('Succ _) = True
  ('Succ m) < ('Succ n) = m < n

data SNat :: Nat -> Type where
  SZero :: SNat 'Zero
  SSucc :: ∀ (n :: Nat). SNat n -> SNat ('Succ n)

--lookupBad :: ∀ (a :: Type)(m :: Nat)(n :: Nat). m -> m < n :~: True -> Vec a n -> a

lookup' :: ∀ (a :: Type)(m :: Nat)(n :: Nat). SNat m -> m < n :~: True -> Vec a n -> a
lookup' _         _     VNil        = undefined
lookup' SZero     Refl (VCons x _)  = x
lookup' (SSucc m) Refl (VCons _ xs) = lookup' m Refl xs

nth :: ∀ (a :: Type)(m :: Nat)(n :: Nat). (m < n) ~ 'True => SNat m -> Vec a n -> a
nth _         VNil         =  undefined
nth SZero     (VCons a _)  = a
nth (SSucc m) (VCons _ as) = nth m as
