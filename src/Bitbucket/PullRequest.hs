{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bitbucket.PullRequest
  (
    BranchDetails (..)
  , PullRequest (..)
  , PullRequestBranch (..)
  , PullRequests (..)
  , PullRequestPost (..)
  ) where

import           Bitbucket.Common (Links(..))
import           Bitbucket.Utils  (jsonOptions)
import           Data.Aeson       (FromJSON (parseJSON), ToJSON (toJSON),
                                   genericParseJSON, genericToJSON)
import           GHC.Generics

data PullRequest = PullRequest
  { id          :: Integer
  , title       :: String
  , links       :: Links
  , source      :: PullRequestBranch
  , destination :: PullRequestBranch
  } deriving (Show, Generic)

instance FromJSON PullRequest where
  parseJSON = genericParseJSON jsonOptions

data PullRequestPost = PullRequestPost
  { title       :: String
  , source      :: PullRequestBranch
  , destination :: PullRequestBranch
  , description :: String
  } deriving (Show, Generic)

instance ToJSON PullRequestPost where
  toJSON = genericToJSON jsonOptions

newtype PullRequestBranch = PullRequestBranch
  { branch :: BranchDetails
  } deriving (Show, Generic)

instance FromJSON PullRequestBranch where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PullRequestBranch where
  toJSON = genericToJSON jsonOptions

newtype BranchDetails = BranchDetails
  { name :: String
  } deriving (Show, Generic)

instance FromJSON BranchDetails where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BranchDetails where
  toJSON = genericToJSON jsonOptions

data PullRequests = PullRequests
  { size   :: Integer
  , values :: [PullRequest]
  , next   :: Maybe String
  } deriving (Show, Generic)

instance FromJSON PullRequests where
  parseJSON = genericParseJSON jsonOptions
