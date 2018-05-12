module Types
  (
    IssueDetails(..)
  , PRDetails(..)
  ) where

data IssueDetails = IssueDetails {
  idTitle :: String,
  idBody  :: String
} deriving (Show)

data PRDetails = PRDetails {
  prTitle      :: String,
  prSrcBranch  :: String,
  prDestBranch :: String,
  prBody       :: String
} deriving (Show)
