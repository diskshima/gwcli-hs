{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub where

import           Control.Lens.Operators ((.~), (^.))
import           Data.Aeson             (FromJSON (parseJSON), ToJSON (toJSON),
                                         decode, genericParseJSON,
                                         genericToJSON)
import           Data.Aeson.Casing      (aesonPrefix, snakeCase)
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.UTF8   as U8
import           Data.Function          ((&))
import           Data.Maybe             (fromMaybe)
import           GHC.Generics
import           GitUtils               (RepoInfo (..), repoInfoFromRepo)
import           Network.Wreq           (Options, Response, defaults, getWith,
                                         header, linkURL, postWith,
                                         responseBody, responseLink)
import           Network.Wreq.Types     (Postable)
import           Opener                 (openUrl)
import           Remote                 (Remote (..), Token)
import           Text.Printf            (printf)
import qualified Types.Issue            as I
import qualified Types.PullRequest      as PR

data IssueGet = IssueGet {
  issuegetNumber  :: Integer,
  issuegetHtmlUrl :: String,
  issuegetTitle   :: String
} deriving (Show, Generic)

instance FromJSON IssueGet where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data IssuePost = IssuePost {
  issuepostTitle :: String,
  issuepostBody  :: Maybe String
} deriving (Show, Generic)

instance ToJSON IssuePost where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data PullRequestPost = PullRequestPost {
  pullrequestpostTitle :: String,
  pullrequestpostHead  :: String,
  pullrequestpostBase  :: String,
  pullrequestpostBody  :: Maybe String
} deriving (Show, Generic)

instance ToJSON PullRequestPost where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data PullRequestGet = PullRequestGet {
  pullrequestgetNumber  :: String,
  pullrequestgetHtmlUrl :: String,
  pullrequestgetTitle   :: String
} deriving (Show, Generic)

instance FromJSON PullRequestGet where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype GitHub = GitHub {
  accessToken :: Token
}

instance Remote GitHub where
  getIssue remote issueId = responseToIssue <$> runItemQuery token path
      where path = "/issues/" ++ issueId
            token = Just $ accessToken remote
  listIssues remote = runListQuery token "/issues" responseToIssue
      where token = Just $ accessToken remote
  createIssue remote details = runCreate token "/issues" param
    where param = issueToIssuePost details
          token = Just $ accessToken remote
  getPullRequest remote prId = responseToPullRequest <$> runItemQuery token path
      where path = "/pulls/" ++ prId
            token = Just $ accessToken remote
  listPullRequests remote = runListQuery token "/pulls" responseToPullRequest
      where token = Just $ accessToken remote
  createPullRequest remote details = runCreate token "/pulls" param
    where param = prToPullRequestPost details
          token = Just $ accessToken remote
  open _ = do
    maybeRi <- repoInfoFromRepo
    case maybeRi of
      Just ri -> openUrl $ browserPath ri
      Nothing -> error "Could not identify repo info."

gitHubBaseUrl :: String
gitHubBaseUrl = "https://api.github.com"

reposPath :: RepoInfo -> String
reposPath ri = printf "/repos/%s/%s" (organization ri) (repository ri)

browserPath :: RepoInfo -> String
browserPath ri = printf "https://github.com/%s/%s" (organization ri) (repository ri)

gitHubHeader :: String -> Options
gitHubHeader token = defaults & header "Authorization" .~ [U8.fromString $ "token " ++ token]

getGitHub :: Maybe String -> String -> IO (Response BL.ByteString)
getGitHub token = getWith opt
  where opt = maybe defaults gitHubHeader token

postGitHub :: Postable a => Maybe String -> String -> a -> IO (Response BL.ByteString)
postGitHub token = postWith opt
  where opt = maybe defaults gitHubHeader token

responseToIssue :: IssueGet -> I.Issue
responseToIssue i =
  I.Issue (Just . show $ issuegetNumber i) (issuegetTitle i) Nothing (Just $ issuegetHtmlUrl i)

responseToPullRequest :: PullRequestGet -> PR.PullRequest
responseToPullRequest pr =
  PR.PullRequest (Just . show $ pullrequestgetNumber pr) (pullrequestgetTitle pr)
                 "" "" Nothing Nothing

readItem :: FromJSON a => Response BL.ByteString -> Maybe a
readItem resp = decode (resp ^. responseBody)

readItems :: FromJSON a => Response BL.ByteString -> [a]
readItems resp = fromMaybe [] items
  where items = decode (resp ^. responseBody)

readNextLink :: Response BL.ByteString -> U8.ByteString
readNextLink resp = resp ^. responseLink "rel" "next" . linkURL

getItemsFromUrl :: FromJSON a => Maybe String -> String -> IO [a]
getItemsFromUrl _ "" = return []
getItemsFromUrl token url = do
  resp <- getGitHub token url
  nextItems <- getItemsFromUrl token (U8.toString (readNextLink resp))
  return $ readItems resp ++ nextItems

buildUrl :: String -> IO (Maybe String)
buildUrl suffix = do
  maybeRi <- repoInfoFromRepo
  return $ case maybeRi of
             Just ri -> Just (gitHubBaseUrl ++ reposPath ri ++ suffix)
             Nothing -> Nothing

runItemQuery :: FromJSON a => Maybe String -> String -> IO a
runItemQuery token suffix = do
  maybeUrl <- buildUrl suffix
  case maybeUrl of
    Just url -> do
      maybeItem <- readItem <$> getGitHub token url
      case maybeItem of
        Just item -> return item
        Nothing -> error "Failed to parse response."
    Nothing -> error "Could not identify remote URL."

runListQuery :: FromJSON a => Maybe String -> String -> (a -> b) -> IO [b]
runListQuery token suffix converter= do
  maybeUrl <- buildUrl suffix
  case maybeUrl of
    Just url -> fmap converter <$> getItemsFromUrl token url
    Nothing  -> error "Could not identify remote URL."

issueToIssuePost :: I.Issue -> IssuePost
issueToIssuePost issue = IssuePost (I.title issue) (I.body issue)

prToPullRequestPost :: PR.PullRequest -> PullRequestPost
prToPullRequestPost pr =
  PullRequestPost (PR.title pr) (PR.srcBranch pr) (PR.destBranch pr) (PR.body pr)

runCreate :: (ToJSON a, FromJSON b) => Maybe String -> String -> a -> IO b
runCreate token suffix param = do
  maybeUrl <- buildUrl suffix
  case maybeUrl of
    Just url -> do
      maybeItem <- readItem <$> postGitHub token url (toJSON param)
      case maybeItem of
        Just item -> return item
        Nothing -> error "Failed to parse response."
    Nothing -> error "Could not identify remote URL."
