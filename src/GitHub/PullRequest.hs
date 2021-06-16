{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GitHub.PullRequest
  (
    PullRequestGet (..)
  , PullRequestPost (..)
  ) where

import           Data.Aeson   (FromJSON (parseJSON), ToJSON (toJSON),
                               genericParseJSON, genericToJSON)
import           GHC.Generics
import           GitHub.Utils (jsonOptions)

data PullRequestGet = PullRequestGet
  { number  :: Integer
  , htmlUrl :: String
  , title   :: String
  } deriving (Show, Generic)

instance FromJSON PullRequestGet where
  parseJSON = genericParseJSON jsonOptions

data PullRequestPost = PullRequestPost
  { title :: String
  , head  :: String
  , base  :: String
  , body  :: Maybe String
  } deriving (Show, Generic)

instance ToJSON PullRequestPost where
  toJSON = genericToJSON jsonOptions
