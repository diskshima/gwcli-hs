module Bitbucket.Utils
  (
    jsonOptions
  ) where

import           Data.Aeson        (Options (..), defaultOptions)
import           Data.Aeson.Casing (snakeCase)

jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = snakeCase }
