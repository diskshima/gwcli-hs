module Types.Issue
  (
    Issue(..),
    formatIssue
  ) where

import           Prelude     hiding (id)
import           StringUtils (orBlank)
import           Text.Printf (printf)

data Issue = Issue {
  id    :: Maybe String,
  title :: String,
  body  :: Maybe String,
  url   :: Maybe String
} deriving (Show)

formatIssue :: Issue -> String
formatIssue i = printf "#%s: %s\n%s" (orBlank (id i)) (title i) (orBlank (url i))
