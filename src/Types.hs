module Types
  (
    IssueDetails(..)
  , PRDetails(..)
  ) where

data IssueDetails = IssueDetails {
  idTitle :: String,
  idBody  :: Maybe String
} deriving (Show)

data PRDetails = PRDetails {
  prTitle      :: String,
  prSrcBranch  :: String,
  prDestBranch :: String,
  prBody       :: Maybe String
} deriving (Show)
