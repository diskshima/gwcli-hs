module Remote where
import Types.Issue
import Types.PullRequest

type Token = String

class Remote a where
  createIssue :: a -> Issue -> IO ()
  getIssue :: a -> Token -> Issue
  listIssues :: a -> Token -> [Issue]
  createPullRequest :: a -> PullRequest -> IO ()
  getPullRequest :: a -> Token -> PullRequest
  listPullRequests :: a -> Token -> [PullRequest]
