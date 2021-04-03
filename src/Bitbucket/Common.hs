{-# LANGUAGE DeriveGeneric         #-}

module Bitbucket.Common
  (
    Html (..)
  , Links (..)
  ) where

import           Bitbucket.Utils  (jsonOptions)
import           Data.Aeson       (FromJSON (parseJSON), genericParseJSON)
import           GHC.Generics

newtype Html = Html
  { href :: String
  } deriving (Show, Generic)

instance FromJSON Html where
  parseJSON = genericParseJSON jsonOptions

data Links = Links
  { self     :: Html
  , html     :: Maybe Html
  , comments :: Maybe Html
  } deriving (Show, Generic)

instance FromJSON Links where
  parseJSON = genericParseJSON jsonOptions
