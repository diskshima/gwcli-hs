module Remote
  (
    Remote(..),
    Token
  ) where

import           Types.Issue
import           Types.PullRequest

type Token = String

class Remote a where
  createIssue :: a -> Issue -> IO ()
  getIssue :: a -> String -> IO Issue
  listIssues :: a -> IO [Issue]
  createPullRequest :: a -> PullRequest -> IO ()
  getPullRequest :: a -> String -> IO PullRequest
  listPullRequests :: a -> IO [PullRequest]
  open :: a -> IO ()
