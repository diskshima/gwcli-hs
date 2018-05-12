module Types
  (
    IssueDetails(..)
  , PullRequestDetails(..)
  ) where

data IssueDetails = IssueDetails {
  idTitle :: String,
  idBody  :: String
} deriving (Show)

data PullRequestDetails = PullRequestDetails {
  prTitle      :: String,
  prBody       :: String,
  prDestBranch :: String,
  prSrcBranch  :: String
} deriving (Show)
