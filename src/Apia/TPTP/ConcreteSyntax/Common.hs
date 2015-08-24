------------------------------------------------------------------------------
-- |
-- Module      : Apia.TPTP.ConcreteSyntax.Common
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Common TPTP concrete syntax to FOF and TFFO languages.
------------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}  -- Implies TypeSynonymInstances.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.TPTP.ConcreteSyntax.Common
  ( CFP(P)
  , cfpNameToTPTP
  , G
  , quantifierHelper
  , runG
  , ToTPTP(toTPTP)
  , TPTP  -- Required by Haddock.
  ) where

------------------------------------------------------------------------------

import Agda.Syntax.Abstract.Name ( Name(nameId), QName(QName) )
import Agda.Syntax.Common        ( NameId(NameId) )
import Agda.Utils.Impossible     ( Impossible(Impossible), throwImpossible )

import Apia.Logic.Types ( LFormula, LTerm(Fun, Var) )
import Apia.Utils.Name  ( freshName )
import Apia.Utils.Text  ( (+++), toUpperFirst )

import Control.Monad.Trans.State
  ( evalState
  , evalStateT
 , get
 , put
  , StateT
  )

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
type GState = [String]

-- The initial state.
initGState ∷ GState
initGState = []

-- | The generating TPTP files monad.
type G = StateT GState IO

-- | Fresh variable.
newGVar ∷ G String
newGVar = fmap (evalState freshName) get

-- | Pop a variable from the state.
popGVar ∷ G ()
popGVar = do
  state ← get
  case state of
    []       → __IMPOSSIBLE__
    (_ : xs) → put xs

-- | Push a variable in the state.
pushGVar ∷ String → G ()
pushGVar x = do
  state ← get
  put $ x : state

-- | Create a fresh variable and push it in the state.
pushGNewVar ∷ G String
pushGNewVar = newGVar >>= \freshVar → pushGVar freshVar >> return freshVar

-- | Running the generating TPTP files monad.
runG ∷ G a → IO a
runG ga = evalStateT ga initGState

------------------------------------------------------------------------------
-- | TPTP type synonym
type TPTP = Text

-- | Translation to TPTP concrete syntax.
class ToTPTP a where
  toTPTP ∷ a → G TPTP

-- Constant, function or predicate.
data CFP = C | F | P

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

-- Constants, functions and predicates names to TPTP
cfpNameToTPTP ∷ CFP → String → G TPTP
cfpNameToTPTP cfp name = do
  name_ ← toTPTP name
  if isUpper (T.head name_)
    then return $ T.cons (symbol cfp) name_
    else return name_
  where
  symbol ∷ CFP → Char
  -- If a function is applied to zero arguments, then if the function
  -- name start by an uppper case letter, we add a @'c'@.
  symbol C = 'c'
  -- If a function name start by an uppper case letter, we add an
  -- @'f'@.
  symbol F = 'f'
  -- If a predicate name start by an uppper case letter, we add a @'p'@.
  symbol P = 'p'

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

quantifierHelper ∷ (LFormula → G Text) → (LTerm → LFormula) → G (String, Text)
quantifierHelper toSomething f = do
  freshVar ← pushGNewVar
  f_       ← toSomething (f (Var freshVar))
  popGVar
  return (freshVar, f_)

------------------------------------------------------------------------------
-- Translation of Agda/Haskell types to TPTP concrete syntax.

instance ToTPTP Char where
  toTPTP c
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
    | c == '_' = return $ T.singleton c
    -- The character is a subscript digit (i.e. ₀, ₁, ..., ₉).
    | ord c `elem` [8320 .. 8329]  = return $ T.singleton $ chr (ord c - 8272)
    | isDigit c || isAsciiUpper c || isAsciiLower c = return $ T.singleton c
    | otherwise = return $ T.pack $ show $ ord c

------------------------------------------------------------------------------
-- Translation of Agda types to TPTP concrete syntax.

instance ToTPTP NameId where
  -- The @Show@ instance @Agda.Syntax.Abstract.Name@ separates the
  -- unique identifier of the top-level module (the second argument)
  -- with '@'. We use '_' because '@' is not TPTP valid.
  --
  -- TODO (02 July 2014). Improve the implemention.
  toTPTP (NameId x i) = return $
    prefixLetter $ (T.pack . show) x +++ "_" +++ (T.pack . show) i

instance ToTPTP QName where
  toTPTP (QName _ name) = toTPTP $ nameId name

------------------------------------------------------------------------------
-- Translation of first-order logic formulae to TPTP concrete syntax.

-- We use this instance because the names on the first-order logic
-- formulae was generated by a @show qname@. Therefore, we need drop
-- the non-valid TPTP symbols.

-- Requires @TypeSynonymInstances@.
instance ToTPTP String where
  toTPTP xs = do
    xs_ ← mapM toTPTP xs
    return $ prefixLetter $ T.concat xs_

instance ToTPTP LTerm where
  toTPTP (Fun name []) = cfpNameToTPTP C name

  toTPTP (Fun name terms) = do
    terms_ ← toTPTP terms
    name_  ← cfpNameToTPTP F name
    return $ name_ +++ "(" +++ terms_ +++ ")"

  toTPTP (Var name) = return $ toUpperFirst $ T.pack name

-- Requires @FlexibleInstances@.
instance ToTPTP [LTerm] where
  toTPTP []       = __IMPOSSIBLE__
  toTPTP [a]      = toTPTP a
  toTPTP (a : as) = do
    a_ ← toTPTP a
    as_ ← toTPTP as
    return $ a_ +++ "," +++ as_
