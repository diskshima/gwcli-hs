{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

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
import           Network.HTTP.Types.URI (renderQuery)
import           Network.Wreq           (Options, Response, defaults, getWith,
                                         header, linkURL, postWith,
                                         responseBody, responseLink)
import           Network.Wreq.Types     (Postable)
import           Opener                 (openUrl)
import           Remote                 (Remote (..), Token)
import           Text.Printf            (printf)
import qualified Types.Issue            as I
import qualified Types.PullRequest      as PR

data IssueGet = IssueGet
  { issuegetNumber  :: Integer
  , issuegetHtmlUrl :: String
  , issuegetTitle   :: String
  } deriving (Show, Generic)

instance FromJSON IssueGet where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data IssuePost = IssuePost
  { issuepostTitle :: String
  , issuepostBody  :: Maybe String
  } deriving (Show, Generic)

instance ToJSON IssuePost where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data PullRequestPost = PullRequestPost
  { pullrequestpostTitle :: String
  , pullrequestpostHead  :: String
  , pullrequestpostBase  :: String
  , pullrequestpostBody  :: Maybe String
  } deriving (Show, Generic)

instance ToJSON PullRequestPost where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data PullRequestGet = PullRequestGet
  { pullrequestgetNumber  :: Integer
  , pullrequestgetHtmlUrl :: String
  , pullrequestgetTitle   :: String
  } deriving (Show, Generic)

instance FromJSON PullRequestGet where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

getIssue :: Remote -> String -> IO I.Issue
getIssue (GitHub token) issueId = responseToIssue <$> runItemQuery token path
    where path = "/issues/" ++ issueId
getIssue (Bitbucket _) _ = undefined

listIssues :: Remote -> Bool -> IO [I.Issue]
listIssues (GitHub token) = runListQuery token "/issues" responseToIssue
listIssues (Bitbucket _)  = undefined

createIssue :: Remote -> I.Issue -> IO I.Issue
createIssue (GitHub token) details =
  responseToIssue <$> runCreate token "/issues" param
    where param = issueToIssuePost details
createIssue (Bitbucket _) _ = undefined

getPullRequest :: Remote -> String -> IO PR.PullRequest
getPullRequest (GitHub token) prId = responseToPullRequest <$> runItemQuery token path
    where path = "/pulls/" ++ prId
getPullRequest (Bitbucket _) _ = undefined

listPullRequests :: Remote -> Bool -> IO [PR.PullRequest]
listPullRequests (GitHub token) = runListQuery token "/pulls" responseToPullRequest
listPullRequests (Bitbucket _) = undefined

createPullRequest :: Remote -> PR.PullRequest -> IO PR.PullRequest
createPullRequest (GitHub token) details =
  responseToPullRequest <$> runCreate token "/pulls" param
    where param = prToPullRequestPost details
createPullRequest (Bitbucket _) _ = undefined

open :: Remote -> IO ()
open remote = do
  maybeRi <- repoInfoFromRepo
  case maybeRi of
    Just ri -> openUrl $ browserPath ri remote
    Nothing -> error "Could not identify repo info."

browserPath :: RepoInfo -> Remote -> String
browserPath ri (GitHub _) = printf "https://github.com/%s/%s" (organization ri) (repository ri)
browserPath ri (Bitbucket _) = printf "https://bitbucket.org/%s/%s" (organization ri) (repository ri)

gitHubBaseUrl :: String
gitHubBaseUrl = "https://api.github.com"

reposPath :: RepoInfo -> String
reposPath ri = printf "/repos/%s/%s" (organization ri) (repository ri)

gitHubHeader :: Token -> Options
gitHubHeader token = defaults & header "Authorization" .~ [U8.fromString $ "token " ++ token]

getGitHub :: Token -> String -> IO (Response BL.ByteString)
getGitHub token = getWith $ gitHubHeader token

postGitHub :: Postable a => Token -> String -> a -> IO (Response BL.ByteString)
postGitHub token = postWith $ gitHubHeader token

responseToIssue :: IssueGet -> I.Issue
responseToIssue i =
  I.Issue (Just . show $ issuegetNumber i) (issuegetTitle i) Nothing (Just $ issuegetHtmlUrl i)

responseToPullRequest :: PullRequestGet -> PR.PullRequest
responseToPullRequest pr =
  PR.PullRequest (Just . show $ pullrequestgetNumber pr) (pullrequestgetTitle pr)
                 "" "" Nothing (Just $ pullrequestgetHtmlUrl pr)

readItem :: FromJSON a => Response BL.ByteString -> Maybe a
readItem resp = decode (resp ^. responseBody)

readItems :: FromJSON a => Response BL.ByteString -> [a]
readItems resp = fromMaybe [] items
  where items = decode (resp ^. responseBody)

readNextLink :: Response BL.ByteString -> U8.ByteString
readNextLink resp = resp ^. responseLink "rel" "next" . linkURL

getItemsFromUrl :: FromJSON a => Token -> String -> IO [a]
getItemsFromUrl _ "" = return []
getItemsFromUrl token url = do
  resp <- getGitHub token url
  nextItems <- getItemsFromUrl token (U8.toString (readNextLink resp))
  return $ readItems resp ++ nextItems

type ParamList = [(U8.ByteString, Maybe U8.ByteString)]

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
      maybeItem <- readItem <$> getGitHub token url
      case maybeItem of
        Just item -> return item
        Nothing   -> error "Failed to parse response."
    Nothing -> error "Could not identify remote URL."

toParamList :: [(String, String)] -> ParamList
toParamList = map (\(k, v) -> (U8.fromString k, Just $ U8.fromString v))

runListQuery :: FromJSON a => Token -> String -> (a -> b) -> Bool -> IO [b]
runListQuery token suffix converter showAll = do
  maybeUrl <- buildUrl suffix params
  case maybeUrl of
    Just url -> fmap converter <$> getItemsFromUrl token url
    Nothing  -> error "Could not identify remote URL."
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
      maybeItem <- readItem <$> postGitHub token url (toJSON param)
      case maybeItem of
        Just item -> return item
        Nothing   -> error "Failed to parse response."
    Nothing -> error "Could not identify remote URL."
