{-# LANGUAGE UnicodeSyntax, ExplicitForAll, GADTs, TypeFamilies, TypeOperators, TypeInType #-}

module Vector where

import Prelude hiding ((++), lookup)
import Data.Kind (Type)

data Nat ∷ Type where
  Zero ∷ Nat
  Succ ∷ Nat → Nat

instance Show Nat where
  show Zero     = "0"
  show (Succ n) = show (read (show n) + 1)

n = Succ (Succ (Succ Zero))

data Vec ∷ Type → Nat → Type where
  Nil  ∷ ∀ (a ∷ Type).           Vec a 'Zero
  (:>) ∷ ∀ (a ∷ Type)(n ∷ Nat). a → Vec a n → Vec a ('Succ n)

infixr 5 :>  
infixr 4 ++  

instance ∀ (a ∷ Type)(n ∷ Nat). Show a => Show (Vec a n) where
  show Nil       =  "[]"
  show (x :> xs) =  let end = case show xs of
                             "[]"  →  "]"
                             ys    →  concat [", ", tail ys]
                       in concat ["[", show x, end]

x = "a" :> "b" :> Nil
y = "c" :> "d" :> "e" :> Nil
z = x ++ y

type family (m ∷ Nat) + (n ∷ Nat) where
  Zero    + n = n
  'Succ m + n = 'Succ (m + n)

(++) ∷ ∀ (a ∷ Type)(m ∷ Nat)(n ∷ Nat). Vec a m → Vec a n → Vec a (m + n)
Nil    ++ v = v
x :> u ++ v = x :> (u ++ v)

data Fin ∷ Nat → Type where
  FZero ∷ ∀ (n ∷ Nat).          Fin (Succ n)
  FSucc ∷ ∀ (n ∷ Nat). Fin n → Fin (Succ n)

lookup ∷ ∀ (a ∷ Type)(n ∷ Nat). Fin n → Vec a n → a
--lookup  n          VNil          = undefined
lookup FZero     (x :> _)  = x
lookup (FSucc n) (_ :> xs) = lookup n xs

a = lookup (FSucc (FZero)) x

data (a ∷ k) :~: (b ∷ k) where
  Refl ∷ ∀ (k ∷ Type)(a ∷ k). a :~: a

infix 4 :~:
infix 5 <

type family (m ∷ Nat) < (n ∷ Nat) where
  _         < Zero      = False
  'Zero     < ('Succ _) = True
  ('Succ m) < ('Succ n) = m < n

data SNat ∷ Nat → Type where
  SZero ∷ SNat 'Zero
  SSucc ∷ ∀ (n ∷ Nat). SNat n → SNat ('Succ n)

--lookupBad ∷ ∀ (a ∷ Type)(m ∷ Nat)(n ∷ Nat). m → m < n :~: True → Vec a n → a

lookup' ∷ ∀ (a ∷ Type)(m ∷ Nat)(n ∷ Nat). SNat m → m < n :~: True → Vec a n → a
lookup' _         _     Nil      = undefined
lookup' SZero     Refl (x :> _)  = x
lookup' (SSucc m) Refl (_ :> xs) = lookup' m Refl xs

nth ∷ ∀ (a ∷ Type)(m ∷ Nat)(n ∷ Nat). (m < n) ~ 'True => SNat m → Vec a n → a
--nth _         VNil         =  undefined
nth SZero     (a :> _)  = a
nth (SSucc m) (_ :> as) = nth m as
