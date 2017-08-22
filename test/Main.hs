{-# LANGUAGE UnicodeSyntax #-}

module Main ( main ) where

import Fail.Errors.Test            ( errorsTests )
import Succeed.NonConjectures.Test ( allNonConjecturesTests )

import Test.Tasty                    ( testGroup, TestTree )
import Test.Tasty.Silver.Interactive ( defaultMain )

------------------------------------------------------------------------------

allTests ∷ IO TestTree
allTests = do
  allErrorsTests ← errorsTests
  return $ testGroup "tests" [ allNonConjecturesTests, allErrorsTests ]

main ∷ IO ()
main = defaultMain =<< allTests
