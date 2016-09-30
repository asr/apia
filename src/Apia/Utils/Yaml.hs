
-- | Yaml module.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.Utils.Yaml
  ( module YAP
  , (.:.)
  , (.?.)
  , (.@.)
  , hypen
  , loadYAML
  , lower
  , underscore
  , upper
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import           Control.Applicative ((<|>))
import           Data.Aeson          as YAP (withObject)
import           Data.Aeson.Types    as YAP (camelTo2)
import qualified Data.Text           as T
import           Data.Yaml           as YAP
import           Data.Yaml.Include   as YamlInclude

------------------------------------------------------------------------------

underscore ∷ T.Text → T.Text
underscore field = T.pack $ camelTo2 '_' $ T.unpack field

hypen ∷ T.Text → T.Text
hypen field = T.pack $ camelTo2 '-' $T.unpack field

lower ∷ T.Text → T.Text
lower = T.toLower

upper ∷ T.Text → T.Text
upper = T.toUpper

(.?.) :: FromJSON a => Object → T.Text → Parser (Maybe a)
x .?. field = x .:? field
  <|> x .:? underscore field
  <|> x .:? hypen field
  <|> x .:? lower field
  <|> x .:? upper field

(.:.) :: FromJSON a => Object → T.Text → Parser a
x .:. field = x .: field
  <|> x .: underscore field
  <|> x .: hypen field
  <|> x .: lower field
  <|> x .: upper field

(.@.) ∷ FromJSON a ⇒ [Object] → T.Text → Parser a
[]  .@. _ = fail  "failed. Expected at least one key-value"
[x] .@. field = x .:. field
(x:xs) .@. field = do
  value ← x .?. field
  maybe (xs .@. field) return value

loadYAML ∷ FilePath → IO (Maybe Object)
loadYAML path = do
  decoded ← YamlInclude.decodeFileEither path
  case decoded of
    Right o   → return $ Just o
    Left msg  → do
      print msg
      return Nothing
