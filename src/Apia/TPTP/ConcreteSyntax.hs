------------------------------------------------------------------------------
-- |
-- Module      : Apia.TPTP.ConcreteSyntax
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- TPTP concrete syntax to FOF and TFFO languages.
------------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}  -- Implies TypeSynonymInstances.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.TPTP.ConcreteSyntax
  ( ToTPTP(toTPTP)
  ) where

------------------------------------------------------------------------------

import Agda.Syntax.Abstract.Name ( Name(nameId), QName(QName) )

import Agda.Syntax.Common
  ( NameId(NameId)
  , TPTPRole(TPTPAxiom, TPTPConjecture, TPTPDefinition, TPTPHint, TPTPType)
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import Apia.Common ( Lang(FOF, TFF0) )

import Apia.Logic.Types
  ( LFormula( And
            , Equiv
            , Exists
            , FALSE
            , ForAll
            , Implies
            , Not
            , Or
            , Predicate
            , TRUE
           )
  , LTerm(Fun, Var)
  , LType(LType)
  , VarName
  )

import Apia.TPTP.Types ( AF(AFor) )

import Apia.Utils.Text ( (+++), parens, toUpperFirst )

import Data.Char
  ( chr
  , isAsciiLower
  , isAsciiUpper
  , isDigit
  , isUpper
  , ord
  )

import Data.Text ( Text )
import qualified Data.Text as T

#include "undefined.h"

------------------------------------------------------------------------------
-- | TPTP type synonym.
type TPTP = Text

-- | Translation to TPTP concrete syntax.
class ToTPTP a where
  toTPTP ∷ Lang → a → TPTP

-- | Constant, function, predicate or type.
data CFPT = C | F | P | T

------------------------------------------------------------------------------
-- Auxiliary functions

-- We prefixed the names with @n@ because TPTP does not accept names
-- starting with digits or @_@.
prefixLetter ∷ TPTP → TPTP
prefixLetter name =
  case T.uncons name of
    Just (x, _)
      | isDigit x || x == '_' → T.cons 'n' name
      | otherwise             → name
    Nothing → __IMPOSSIBLE__

-- From the technical manual of TPTP
-- (http://www.cs.miami.edu/~tptp/TPTP/TR/TPTPTR.shtml)

-- ... variables start with upper case letters, ... predicates and
-- functors either start with lower case and contain alphanumerics and
-- underscore ...

-- | Constants, functions, predicates and types names to TPTP concrete
-- syntax.
cfptNameToTPTP ∷ Lang → CFPT → String → TPTP
cfptNameToTPTP lang cfp name =
  if isUpper (T.head nameTPTP) then T.cons (symbol cfp) nameTPTP else nameTPTP
  where
  symbol ∷ CFPT → Char
  -- If a function is applied to zero arguments, then if the function
  -- name start by an uppper case letter, we add a @'c'@.
  symbol C = 'c'
  -- If a function name start by an uppper case letter, we add an
  -- @'f'@.
  symbol F = 'f'
  -- If a predicate name start by an uppper case letter, we add a @'p'@.
  symbol P = 'p'
  -- If a type name start by an uppper case letter, we add a @'t'@.
  symbol T = 't'

  nameTPTP ∷ Text
  nameTPTP = toTPTP lang name

-- If a variable name start by an lower case letter, we add a @'V'@.
--
-- We are not using this function because we don't have a test case
-- where the variables names clash.

-- varName2TPTP ∷ String → TPTP
-- varName2TPTP name =
--   if isLower (head nameTPTP) then 'V' : nameTPTP else nameTPTP
--   where
--   nameTPTP ∷ String
--   nameTPTP = toTPTP name

quantifierBodyToTPTP ∷ Lang → Maybe LType → VarName → (LTerm → LFormula) →
                       TPTP
quantifierBodyToTPTP FOF _ var f =
  "[" +++ toUpperFirst (T.pack var) +++ "] : "
  +++ toTPTP FOF (f (Var var))

quantifierBodyToTPTP TFF0 (Just (LType ty)) var f =
  "[" +++ toUpperFirst (T.pack var) +++ " : " +++ cfptNameToTPTP TFF0 T ty
  +++ "] : "
  +++ toTPTP TFF0 (f (Var var))

quantifierBodyToTPTP TFF0 Nothing _ _ = __IMPOSSIBLE__

------------------------------------------------------------------------------
-- Translation of Agda/Haskell types to TPTP concrete syntax.

instance ToTPTP Char where
  toTPTP _ c
    -- From Agda wiki (10 December 2012): A name part is a string of
    -- printable characters not containing any of the following
    -- characters: _;."(){}@.
    | c == ';'  = __IMPOSSIBLE__
    | c == '.'  = __IMPOSSIBLE__
    | c == '"'  = __IMPOSSIBLE__
    | c == '('  = __IMPOSSIBLE__
    | c == ')'  = __IMPOSSIBLE__
    | c == '{'  = __IMPOSSIBLE__
    | c == '}'  = __IMPOSSIBLE__
    | c == '@'  = __IMPOSSIBLE__

    -- We use the character @_@ to separate the Agda NameId (see
    -- below).
    | c == '_' = T.singleton c

    -- The character is a subscript digit (i.e. ₀, ₁, ..., ₉).
    | ord c `elem` [8320 .. 8329]  = T.singleton $ chr (ord c - 8272)

    | isDigit c || isAsciiUpper c || isAsciiLower c = T.singleton c
    | otherwise  = T.pack $ show $ ord c

------------------------------------------------------------------------------
-- Translation of Agda types to TPTP concrete syntax.

instance ToTPTP NameId where
  -- The @Show@ instance @Agda.Syntax.Abstract.Name@ separates the
  -- unique identifier of the top-level module (the second argument)
  -- with '@'. We use '_' because '@' is not TPTP valid.
  --
  -- TODO (02 July 2014). Improve the implemention.
  toTPTP _ (NameId x i) =
    prefixLetter $ (T.pack . show) x +++ "_" +++ (T.pack . show) i

instance ToTPTP QName where
  toTPTP lang (QName _ name) = toTPTP lang $ nameId name

------------------------------------------------------------------------------
-- Translation of the target logic formulae to TPTP concrete syntax.

-- We use this instance because the names on the first-order logic
-- formulae was generated by a @show qname@. Therefore, we need drop
-- the non-valid TPTP symbols.

-- Requires @TypeSynonymInstances@.
instance ToTPTP String where
  toTPTP lang = prefixLetter . T.concat . map (toTPTP lang)

instance ToTPTP LTerm where
  toTPTP lang (Fun name []) = cfptNameToTPTP lang C name
  toTPTP lang (Fun name terms) =
    cfptNameToTPTP lang F name +++ parens (toTPTP lang terms)
  toTPTP _ (Var name) = toUpperFirst $ T.pack name

-- Requires @FlexibleInstances@.
instance ToTPTP [LTerm] where
  toTPTP _    []       = __IMPOSSIBLE__
  toTPTP lang [a]      = toTPTP lang a
  toTPTP lang (a : as) = toTPTP lang a +++ "," +++ toTPTP lang as

instance ToTPTP LFormula where
  -- We translate the hard-coded logic predicate @equal_@ as the
  -- predefined equality in the ATP.
  toTPTP lang (Predicate "equal_" [t1, t2] ) =
    parens $ toTPTP lang t1 +++ " = " +++ toTPTP lang t2

  toTPTP _ (Predicate "equal_" _) = __IMPOSSIBLE__

  -- If the predicate represents a propositional logic variable,
  -- following the TPTP syntax, we do not print the internal
  -- parenthesis.
  toTPTP lang (Predicate name []) = parens $ cfptNameToTPTP lang P name

  toTPTP lang (Predicate name terms) =
    cfptNameToTPTP lang P name +++ parens (toTPTP lang terms)

  toTPTP lang (And f1 f2)        = parens $ toTPTP lang f1 +++ " & " +++ toTPTP lang f2
  toTPTP lang (Or f1 f2)         = parens $ toTPTP lang f1 +++ " | " +++ toTPTP lang f2
  toTPTP lang (Not f)            = parens $ T.cons '~' (toTPTP lang f)
  toTPTP lang (Implies f1 f2)    = parens $ toTPTP lang f1 +++ " => " +++ toTPTP lang f2
  toTPTP lang (Equiv f1 f2)      = parens $ toTPTP lang f1 +++ " <=> " +++ toTPTP lang f2
  toTPTP lang (ForAll var ty f)  = "( ! " +++ quantifierBodyToTPTP lang ty var f +++ " )"
  toTPTP lang (Exists var ty f)  = "( ? " +++ quantifierBodyToTPTP lang ty var f +++ " )"
  toTPTP _    TRUE               = parens "$true"
  toTPTP _    FALSE              = parens "$false"

instance ToTPTP TPTPRole where
  toTPTP _    TPTPAxiom      = "axiom"
  toTPTP _    TPTPConjecture = "conjecture"
  toTPTP _    TPTPDefinition = "definition"
  toTPTP _    TPTPHint       = "hypothesis"
  toTPTP FOF  TPTPType       = __IMPOSSIBLE__
  toTPTP TFF0 TPTPType       = "type"

instance ToTPTP Lang where
  toTPTP _ FOF  = "fof"
  toTPTP _ TFF0 = "tff"

-- Translation of annotated formulae to TPTP concrete syntax.
instance ToTPTP AF where
  toTPTP lang (AFor qName atpRole formula) =
    toTPTP lang lang
    +++ "("
    +++ toTPTP lang qName +++ ", "
    +++ toTPTP lang atpRole +++ ", "
    +++ toTPTP lang formula
    +++ ")." +++ "\n\n"
