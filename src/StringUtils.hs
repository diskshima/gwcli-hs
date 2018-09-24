module StringUtils
  (
    orBlank
  ) where

import           Data.Maybe (fromMaybe)

orBlank :: Maybe String -> String
orBlank = fromMaybe ""
