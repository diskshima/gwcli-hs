{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHubApi
  (
    createIssue
  , createPullRequest
  , getDefaultBranch
  , getIssue
  , getPullRequest
  , listIssues
  , listPullRequests
  , prToPullRequestPost
  , readIssueTemplate
  , readPRTemplate
  , responseToPullRequest
  , runCreate
  ) where

import           Control.Lens.Operators    ((.~), (^.))
import           Data.Aeson                (FromJSON (parseJSON),
                                            ToJSON (toJSON), genericParseJSON)
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.UTF8      as U8
import           Data.Function             ((&))
import           Data.Git.Storage          (findRepoMaybe)
import           Filesystem.Path.CurrentOS (encodeString)
import           GHC.Generics
import           GitHub.Issue              as IG (IssueGet (..))
import           GitHub.Issue              as IP (IssuePost (..))
import           GitHub.PullRequest        as PRG (PullRequestGet (..))
import           GitHub.PullRequest        as PRP (PullRequestPost (..))
import           GitHub.Utils              (jsonOptions)
import           GitUtils                  (Branch, RepoInfo (..),
                                            repoInfoFromRepo)
import           JsonUtils                 (decodeResponse,
                                            decodeResponseAsList)
import           Network.HTTP.Types.URI    (renderQuery)
import           Network.Wreq              (Options, Response, defaults,
                                            getWith, header, linkURL, postWith,
                                            responseLink)
import           Network.Wreq.Types        (Postable)
import           Prelude                   as P
import           System.Directory          (doesPathExist)
import           Text.Printf               (printf)
import qualified Types.Issue               as I
import qualified Types.PullRequest         as PR
import           WebUtils                  (ParamList, Token, toParamList)

newtype RepoGet = RepoGet
  { defaultBranch :: String
  } deriving (Show, Generic)

instance FromJSON RepoGet where
  parseJSON = genericParseJSON jsonOptions

gitHubBaseUrl :: String
gitHubBaseUrl = "https://api.github.com"

getIssue :: Token -> String -> IO I.Issue
getIssue token issueId = responseToIssue <$> runItemQuery token path
    where path = "/issues/" ++ issueId

listIssues :: Token -> Bool -> IO [I.Issue]
listIssues token = runListQuery token "/issues" responseToIssue

createIssue :: Token -> I.Issue -> IO I.Issue
createIssue token details = responseToIssue <$> runCreate token "/issues" param
  where param = issueToIssuePost details

getPullRequest :: Token -> String -> IO PR.PullRequest
getPullRequest token prId = responseToPullRequest <$> runItemQuery token path
  where path = "/pulls/" ++ prId

listPullRequests :: Token -> Bool -> IO [PR.PullRequest]
listPullRequests token = runListQuery token "/pulls" responseToPullRequest

createPullRequest :: Token -> PR.PullRequest -> IO PR.PullRequest
createPullRequest token item = responseToPullRequest <$> runCreate token "/pulls" param
  where param = prToPullRequestPost item

getDefaultBranch :: Token -> IO (Maybe Branch)
getDefaultBranch token = do
  maybeUrl <- buildUrl "" Nothing
  case maybeUrl of
    Just url -> do
      response <- getGitHub token url
      extractBranch response
    Nothing -> return Nothing

reposPath :: RepoInfo -> String
reposPath ri = printf "/repos/%s/%s" (organization ri) (repository ri)

gitHubHeader :: Token -> Options
gitHubHeader token = defaults & header "Authorization" .~ [U8.fromString $ "token " ++ token]

getGitHub :: Token -> String -> IO (Response BL.ByteString)
getGitHub token = getWith $ gitHubHeader token

postGitHub :: Postable a => Token -> String -> a -> IO (Response BL.ByteString)
postGitHub token = postWith $ gitHubHeader token

responseToIssue :: IG.IssueGet -> I.Issue
responseToIssue i =
  I.Issue (Just . show $ IG.number i) (IG.title i) Nothing (Just $ IG.htmlUrl i)

responseToPullRequest :: PullRequestGet -> PR.PullRequest
responseToPullRequest pr =
  PR.PullRequest prNumber prTitle prSrcBranch prDestBranch prBody prUrl
  where prNumber = Just . show $ PRG.number pr
        prTitle = PRG.title pr
        prSrcBranch = ""
        prDestBranch = ""
        prBody = Nothing
        prUrl = Just $ PRG.htmlUrl pr

readNextLink :: Response BL.ByteString -> U8.ByteString
readNextLink resp = resp ^. responseLink "rel" "next" . linkURL

getItemsFromUrl :: FromJSON a => Token -> String -> IO [a]
getItemsFromUrl _ "" = return []
getItemsFromUrl token url = do
  resp <- getGitHub token url
  nextItems <- getItemsFromUrl token (U8.toString (readNextLink resp))
  return $ decodeResponseAsList resp ++ nextItems

buildUrl :: String -> Maybe ParamList -> IO (Maybe String)
buildUrl suffix maybeParams = do
  maybeRi <- repoInfoFromRepo
  return $ case maybeRi of
             Just ri -> Just (gitHubBaseUrl ++ reposPath ri ++ suffix ++ query)
             Nothing -> Nothing
    where query = case maybeParams of
                    Just params -> U8.toString $ renderQuery True params
                    Nothing     -> ""

runItemQuery :: FromJSON a => Token -> String -> IO a
runItemQuery token suffix = do
  maybeUrl <- buildUrl suffix Nothing
  case maybeUrl of
    Just url -> do
      maybeItem <- decodeResponse <$> getGitHub token url
      case maybeItem of
        Just item -> return item
        Nothing   -> P.error "Failed to parse response."
    Nothing -> P.error "Could not identify remote URL."

runListQuery :: FromJSON a => Token -> String -> (a -> b) -> Bool -> IO [b]
runListQuery token suffix converter showAll = do
  maybeUrl <- buildUrl suffix params
  case maybeUrl of
    Just url -> fmap converter <$> getItemsFromUrl token url
    Nothing  -> P.error "Could not identify remote URL."
  where params = if showAll
                    then (Just . toParamList) [("filter", "all"), ("state", "all")]
                    else Nothing

issueToIssuePost :: I.Issue -> IssuePost
issueToIssuePost issue = IssuePost (I.title issue) (I.body issue)

prToPullRequestPost :: PR.PullRequest -> PullRequestPost
prToPullRequestPost pr =
  PullRequestPost (PR.title pr) (PR.srcBranch pr) (PR.destBranch pr) (PR.body pr)

runCreate :: (ToJSON a, FromJSON b) => Token -> String -> a -> IO b
runCreate token suffix param = do
  maybeUrl <- buildUrl suffix Nothing
  case maybeUrl of
    Just url -> do
      maybeItem <- decodeResponse <$> postGitHub token url (toJSON param)
      case maybeItem of
        Just item -> return item
        Nothing   -> P.error "Failed to parse response."
    Nothing -> P.error "Could not identify remote URL."

readIssueTemplate :: IO String
readIssueTemplate = do
  mRepoPath <- findRepoMaybe
  case mRepoPath of
    Just repoPath -> do
      exists <- doesPathExist templatePath
      if exists then readFile templatePath else return ""
        where templatePath = encodeString repoPath ++ "/../.github/ISSUE_TEMPLATE.md"
    Nothing -> return ""

readPRTemplate :: IO String
readPRTemplate = do
  mRepoPath <- findRepoMaybe
  case mRepoPath of
    Just repoPath -> do
      exists <- doesPathExist templatePath
      if exists then readFile templatePath else return ""
        where templatePath = encodeString repoPath ++ "/../.github/PULL_REQUEST_TEMPLATE.md"
    Nothing -> return ""

extractBranch :: Response BL.ByteString -> IO (Maybe Branch)
extractBranch response =
  case decodeResponse response of
    Just item -> return (Just $ defaultBranch item)
    Nothing   -> return Nothing
