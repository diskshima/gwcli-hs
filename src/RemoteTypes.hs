module RemoteTypes
  ( Remote (..)
  ) where

import           WebUtils (Token)

data Remote = GitHub Token
            | Bitbucket Token
