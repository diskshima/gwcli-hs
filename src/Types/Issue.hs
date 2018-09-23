module Types.Issue
  (
    Issue(..)
  ) where

data Issue = Issue {
  title :: String,
  body  :: Maybe String
} deriving (Show)
