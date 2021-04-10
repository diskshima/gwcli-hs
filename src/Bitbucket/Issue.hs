{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bitbucket.Issue
  (
    Issue (..)
  , IssueContent (..)
  , Issues (..)
  , IssuePost (..)
  ) where

import           Bitbucket.Common (Links (..))
import           Bitbucket.Utils  (jsonOptions)
import           Data.Aeson       (FromJSON (parseJSON), ToJSON (toJSON),
                                   genericParseJSON, genericToJSON)
import           GHC.Generics

data Issue = Issue
  { id    :: Integer
  , title :: String
  , links :: Links
  } deriving (Show, Generic)

instance FromJSON Issue where
  parseJSON = genericParseJSON jsonOptions

data IssueContent = IssueContent
  { raw    :: String
  , markup :: String
  } deriving (Show, Generic)

instance ToJSON IssueContent where
  toJSON = genericToJSON jsonOptions

data IssuePost = IssuePost
  { title   :: String
  , content :: IssueContent
  } deriving (Show, Generic)

instance ToJSON IssuePost where
  toJSON = genericToJSON jsonOptions

data Issues = Issues
  { size   :: Integer
  , values :: [Issue]
  , next   :: Maybe String
  } deriving (Show, Generic)

instance FromJSON Issues where
  parseJSON = genericParseJSON jsonOptions
