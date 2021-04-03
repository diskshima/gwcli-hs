module GitHub.Utils
  (
    jsonOptions
  ) where

import           Data.Aeson             (defaultOptions, Options(..))
import           Data.Aeson.Casing      (snakeCase)

jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = snakeCase }
