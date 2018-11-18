module Remote
  (
    Remote(..),
    Token
  ) where

import           Types.Issue
import           Types.PullRequest

type Token = String

class Remote a where
  createIssue :: a -> Issue -> IO Issue
  getIssue :: a -> String -> IO Issue
  listIssues :: a -> Bool -> IO [Issue]
  createPullRequest :: a -> PullRequest -> IO PullRequest
  getPullRequest :: a -> String -> IO PullRequest
  listPullRequests :: a -> Bool -> IO [PullRequest]
  open :: a -> IO ()
