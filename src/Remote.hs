module Remote
  (
    Remote(..),
    Token
  ) where

type Token = String

data Remote = GitHub Token
            | Bitbucket Token
