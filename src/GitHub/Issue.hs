{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GitHub.Issue
  (
    IssueGet (..)
  , IssuePost (..)
  ) where

import           Data.Aeson   (FromJSON (parseJSON), ToJSON (toJSON),
                               genericParseJSON, genericToJSON)
import           GHC.Generics
import           GitHub.Utils (jsonOptions)

data IssuePullRequest = IssuePullRequest
  { url     :: String
  , htmlUrl :: String
  } deriving (Show, Generic)

instance FromJSON IssuePullRequest where
  parseJSON = genericParseJSON jsonOptions

data IssueGet = IssueGet
  { number      :: Integer
  , htmlUrl     :: String
  , title       :: String
  , pullRequest :: Maybe IssuePullRequest
  } deriving (Show, Generic)

instance FromJSON IssueGet where
  parseJSON = genericParseJSON jsonOptions

data IssuePost = IssuePost
  { title :: String
  , body  :: Maybe String
  } deriving (Show, Generic)

instance ToJSON IssuePost where
  toJSON = genericToJSON jsonOptions
