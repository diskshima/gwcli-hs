module RemoteTypes
  ( Message (..)
  , Remote (..)
  ) where

import           WebUtils (Token)

data Remote = GitHub Token
            | Bitbucket Token

data Message = Message {
  title :: String,
  body  :: String
} deriving (Show)
