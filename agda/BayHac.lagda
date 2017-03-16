%<*nat>
\begin{code}
data ℕ : Set where
  zero  :  ℕ
  suc   :  ℕ → ℕ
\end{code}
%</nat>

%<*vec>
\begin{code}
data Vec (A : Set) : ℕ → Set where
  []   :  Vec A zero
  _∷_  :  {n : ℕ} → A → Vec A n → Vec A (suc n)
\end{code}
%</vec>

%<*plus>
\begin{code}
_+_ : ℕ → ℕ → ℕ
zero   +  n  =  n
suc m  +  n  =  suc (m + n)
\end{code}
%</plus>

%<*append>
\begin{code}
_++_ :  {A : Set} → {m n : ℕ} →
        Vec A m → Vec A n → Vec A (m + n)
[]        ++  y  =  y
(x ∷ xs)  ++  y  =  x ∷ (xs ++ y)
\end{code}
%</append>

%<*fin>
\begin{code}
data Fin : ℕ → Set where
  zero  :  {n : ℕ}          → Fin (suc n)
  suc   :  {n : ℕ} → Fin n  → Fin (suc n)
\end{code}
%</fin>

%<*lookup>
\begin{code}
lookup : {A : Set} → {n : ℕ} → Fin n → Vec A n → A
lookup  ()       []           -- can be omitted
lookup  zero     (x ∷ _ )  =  x
lookup  (suc n)  (_ ∷ xs)  =  lookup n xs
\end{code}
%</lookup>

%<*bool>
\begin{code}
data Bool : Set where
  true   :  Bool
  false  :  Bool
\end{code}
%</bool>

%<*eq>
\begin{code}
data _≡_ {A : Set} : A → A → Set where
  refl : {a : A} → a ≡ a
\end{code}
%</eq>

%<*lt>
\begin{code}
_<_ : ℕ → ℕ → Bool
_      <  zero   =  false
zero   <  suc n  =  true
suc m  <  suc n  =  m < n
\end{code}
%</lt>

%<*lookup2>
\begin{code}
lookup' :  {A : Set} → {n : ℕ} →
           (m : ℕ) → m < n ≡ true → Vec A n → A
lookup' _        ()    []           -- required
lookup' zero     refl  (x ∷ _ )  =  x
lookup' (suc m)  p     (_ ∷ xs)  =  lookup' m p xs
\end{code}
%</lookup2>

%<*infix>
\begin{code}
infixl 6 _+_
infixr 5 _∷_ _++_
infix  4 _<_
infix  2 _≡_
\end{code}
%</infix>
