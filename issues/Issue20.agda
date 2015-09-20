{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Issue20 where

postulate
  Ty  : Set
  _≡_ : {A : Set} → A → A → Set

_≡Ty_ : Ty → Ty → Set
_≡Ty_ = _≡_ {Ty}
{-# ATP definition _≡Ty_ #-}

postulate foo : (t : Ty) → t ≡Ty t
{-# ATP prove foo #-}

-- An internal error has occurred. Please report this as a bug.
-- Location of the error: src/Apia/Utils/AgdaAPI/EtaExpansion.hs:143
