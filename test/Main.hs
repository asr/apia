{-# LANGUAGE UnicodeSyntax #-}

module Main ( main ) where

import qualified Fail.Errors.Test            as Errors
import qualified Succeed.NonConjectures.Test as NonConjectures

import System.Environment ( getEnv )

import Test.Tasty                    ( testGroup, TestTree )
import Test.Tasty.Silver.Filter      ( RegexFilter )
import Test.Tasty.Silver.Interactive ( defaultMain1 )

------------------------------------------------------------------------------

allTests ∷ IO TestTree
allTests = do
  allErrorsTests ← Errors.errorsTests
  return $ testGroup "tests"
    [ NonConjectures.allNonConjecturesTests
    , allErrorsTests
    ]

couldBeDisabledTests :: [RegexFilter]
couldBeDisabledTests = NonConjectures.couldBeDisabledTests

main ∷ IO ()
main = do
  user ← getEnv "USER"

  let disabledTests ∷ [RegexFilter]
      disabledTests = if user == "asr" then [] else couldBeDisabledTests

  defaultMain1 disabledTests =<< allTests
