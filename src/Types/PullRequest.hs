module Types.PullRequest
  (
    PullRequest(..),
    formatPullRequest
  ) where

import           Prelude     hiding (id)
import           StringUtils (orBlank)
import           Text.Printf (printf)

data PullRequest = PullRequest {
  id         :: Maybe String,
  title      :: String,
  srcBranch  :: String,
  destBranch :: String,
  body       :: Maybe String,
  url        :: Maybe String
} deriving (Show)

formatPullRequest :: PullRequest -> String
formatPullRequest pr = printf "#%s: %s\n%s" (orBlank (id pr)) (title pr) (orBlank (url pr))
