{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Remote where

import           BitbucketApi      as BB
import           Data.Maybe        (fromMaybe)
import           GitHubApi         as GH
import           GitUtils          (Branch, RepoInfo (..), getCurrentBranch,
                                    repoInfoFromRepo)
import           Opener            (openUrl)
import           Prelude           as P
import           RemoteTypes       (Remote (..))
import           Text.Printf       (printf)
import qualified Types.Issue       as I
import qualified Types.PullRequest as PR
import           WebUtils          (Tokens)

authenticate :: Remote -> IO Tokens
authenticate (GitHub _)    = undefined
authenticate (Bitbucket _) = BB.authenticate

getIssue :: Remote -> String -> IO I.Issue
getIssue (GitHub token)    = GH.getIssue token
getIssue (Bitbucket token) = BB.getIssue token

listIssues :: Remote -> Bool -> IO [I.Issue]
listIssues (GitHub token)    = GH.listIssues token
listIssues (Bitbucket token) = BB.listIssues token

createIssue :: Remote -> I.Issue -> IO I.Issue
createIssue (GitHub token)    = GH.createIssue token
createIssue (Bitbucket token) = BB.createIssue token

getPullRequest :: Remote -> String -> IO PR.PullRequest
getPullRequest (GitHub token)    = GH.getPullRequest token
getPullRequest (Bitbucket token) = BB.getPullRequest token

listPullRequests :: Remote -> Bool -> IO [PR.PullRequest]
listPullRequests (GitHub token) showAll = GH.listPullRequests token showAll
listPullRequests (Bitbucket token) _    = BB.listPullRequests token

createPullRequest :: Remote -> PR.PullRequest -> IO PR.PullRequest
createPullRequest (GitHub token)    = GH.createPullRequest token
createPullRequest (Bitbucket token) = BB.createPullRequest token

open :: Remote -> Maybe String -> IO ()
open remote file = do
  maybeRi <- repoInfoFromRepo
  maybeBranch <- getCurrentBranch
  let branch = fromMaybe "master" maybeBranch
  case maybeRi of
    Just ri -> openUrl $ browserPath ri remote branch file
    Nothing -> P.error "Could not identify repo info."

browserPath :: RepoInfo -> Remote -> Branch -> Maybe FilePath -> String
browserPath ri remote br mFP =
  case (remote, mFP) of
    (GitHub _, Nothing)    -> printf "https://github.com/%s/%s/tree/%s" org rep br
    (GitHub _, Just fp)    -> printf "https://github.com/%s/%s/blob/%s/%s" org rep br fp
    (Bitbucket _, Nothing) -> printf "https://bitbucket.org/%s/%s/src/%s" org rep br
    (Bitbucket _, Just fp) -> printf "https://bitbucket.org/%s/%s/src/%s/%s" org rep br fp
  where org = organization ri
        rep = repository ri
