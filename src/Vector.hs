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

n2, n3, n5 ∷ Nat
n2 = Succ (Succ Zero)
n3 = Succ (Succ (Succ Zero))
n5 = Succ (Succ (Succ (Succ (Succ Zero))))

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

xx ∷ Vec String ('Succ ('Succ 'Zero))
xx = "a" :> "b" :> Nil

yy ∷ Vec String ('Succ ('Succ ('Succ 'Zero)))
yy = "c" :> "d" :> "e" :> Nil

zz ∷ Vec String ('Succ ('Succ ('Succ ('Succ ('Succ 'Zero)))))
zz = xx ++ yy

type family (m ∷ Nat) + (n ∷ Nat) ∷ Nat where
  'Zero   + n = n
  'Succ m + n = 'Succ (m + n)

(++) ∷ ∀ (a ∷ Type)(m ∷ Nat)(n ∷ Nat). Vec a m → Vec a n → Vec a (m + n)
Nil    ++ v = v
x :> u ++ v = x :> (u ++ v)

data Fin ∷ Nat → Type where
  FZero ∷ ∀ (n ∷ Nat).         Fin ('Succ n)
  FSucc ∷ ∀ (n ∷ Nat). Fin n → Fin ('Succ n)

lookup ∷ ∀ (a ∷ Type)(n ∷ Nat). Fin n → Vec a n → a
--lookup  n          VNil          = undefined
lookup FZero     (x :> _)  = x
lookup (FSucc n) (_ :> xs) = lookup n xs

aa ∷ String
aa = lookup (FSucc (FZero)) xx

data (a ∷ k) ≡ (b ∷ k) where
  Refl ∷ ∀ (k ∷ Type)(a ∷ k). a ≡ a

infix 4 ≡
infix 5 <

type family (m ∷ Nat) < (n ∷ Nat) ∷ Bool where
  _         < 'Zero      = 'False
  'Zero     < ('Succ _) = 'True
  ('Succ m) < ('Succ n) = m < n

data SNat ∷ Nat → Type where
  SZero ∷ SNat 'Zero
  SSucc ∷ ∀ (n ∷ Nat). SNat n → SNat ('Succ n)

--lookupBad ∷ ∀ (a ∷ Type)(m ∷ Nat)(n ∷ Nat). m → m < n ≡ True → Vec a n → a
--lookupBad2 ∷ ∀ (a ∷ Type)(m ∷ Nat)(n ∷ Nat). Nat → m < n ≡ True → Vec a n → a
--lookupBad2 x p v = undefined

data Dummy ∷ Nat → Type where
  Dummy ∷ ∀ (n ∷ Nat). Dummy n

--lookupBad3 ∷ ∀ (a ∷ Type)(m ∷ Nat)(n ∷ Nat). Dummy m → Nat → m < n ≡ 'True → Vec a n → a
--lookupBad3 Dummy Zero     Refl (x :> _)  = x
--lookupBad3 Dummy (Succ m) p (_ :> xs) = lookupBad3 Dummy m p xs

type family LookupUp (a ∷ Type) (m ∷ Nat) (n ∷ Nat) (p ∷ m < n ≡ 'True) (v ∷ Vec a n) :: a where
  LookupUp a 'Zero     ('Succ n) 'Refl (x ':> _)  = x
  LookupUp a ('Succ m) ('Succ n) p     (_ ':> xs) = LookupUp a m n p xs

vv ∷ Vec Nat ('Succ ('Succ 'Zero))
vv = Zero :> Succ Zero :> Nil

--ve ∷ Nat
--ve = LookupUp Nat 'Zero ('Succ ('Succ 'Zero)) 'Refl ('Zero ':> 'Succ 'Zero ':> 'Nil)
-- In ghci, try running
-- :set -XTypeInType
-- :set -XTypeOperators
-- :kind! LookupUp Nat 'Zero ('Succ ('Succ 'Zero)) 'Refl ('Zero ':> 'Succ 'Zero ':> 'Nil)
-- This normalizes the kind.  But only evaluates one step.  Try this too:
-- :kind! LookupUp Nat ('Succ 'Zero) ('Succ ('Succ 'Zero)) 'Refl ('Zero ':> 'Succ 'Zero ':> 'Nil)

lookup' ∷ ∀ (a ∷ Type)(m ∷ Nat)(n ∷ Nat). SNat m → m < n ≡ 'True → Vec a n → a
-- note that if Refl is replaced by _ in the following line there is no compiler error
--lookup' _         Refl     Nil      = undefined
lookup' SZero     Refl (x :> _)  = x
lookup' (SSucc m) Refl (_ :> xs) = lookup' m Refl xs

nth ∷ ∀ (a ∷ Type)(m ∷ Nat)(n ∷ Nat). (m < n) ~ 'True => SNat m → Vec a n → a
--nth _         Nil         =  undefined
nth SZero     (a :> _)  = a
nth (SSucc m) (_ :> as) = nth m as
