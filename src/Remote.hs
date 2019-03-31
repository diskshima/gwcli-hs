{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Remote where

import           BitbucketApi      as BB
import           GitHubApi         as GH
import           GitUtils          (RepoInfo (..), repoInfoFromRepo)
import           Opener            (openUrl)
import           Prelude           as P
import           RemoteTypes       (Remote (..))
import           Text.Printf       (printf)
import qualified Types.Issue       as I
import qualified Types.PullRequest as PR

authenticate :: Remote -> IO String
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

open :: Remote -> IO ()
open remote = do
  maybeRi <- repoInfoFromRepo
  case maybeRi of
    Just ri -> openUrl $ browserPath ri remote
    Nothing -> P.error "Could not identify repo info."

browserPath :: RepoInfo -> Remote -> String
browserPath ri (GitHub _) = printf "https://github.com/%s/%s" (organization ri) (repository ri)
browserPath ri (Bitbucket _) = printf "https://bitbucket.org/%s/%s" (organization ri) (repository ri)
