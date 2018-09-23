module Types.PullRequest
  (
   PullRequest(..)
  ) where

data PullRequest = PullRequest {
  title      :: String,
  srcBranch  :: String,
  destBranch :: String,
  body       :: Maybe String
} deriving (Show)
