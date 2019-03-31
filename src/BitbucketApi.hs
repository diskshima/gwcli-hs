{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module BitbucketApi
  (
    authenticate
  , createIssue
  , createPullRequest
  , getIssue
  , getPullRequest
  , listIssues
  , listPullRequests
  ) where

import           Control.Lens.Operators       ((.~))
import           Data.Aeson                   (FromJSON (parseJSON),
                                               ToJSON (..), genericParseJSON,
                                               genericToJSON)
import           Data.Aeson.Casing            (aesonPrefix, snakeCase)
import qualified Data.ByteString.Lazy         as BL
import           Data.ByteString.Lazy.Builder (toLazyByteString)
import           Data.ByteString.Lazy.Char8   as BL8
import qualified Data.ByteString.UTF8         as U8
import           Data.Function                ((&))
import           Data.Maybe                   (fromMaybe)
import           Data.Text.Lazy               as TL
import           GHC.Generics
import           GitUtils                     (RepoInfo (..), repoInfoFromRepo)
import           JsonUtils                    (decodeResponse,
                                               decodeResponseOrError)
import           Network.HTTP.Types.URI       (QueryItem, renderQuery)
import           Network.OAuth.OAuth2         (OAuth2 (..), authorizationUrl)
import           Network.Wreq                 (Options, Response, defaults,
                                               getWith, header, postWith)
import           Network.Wreq.Types           (Postable)
import           Prelude                      as P
import           System.Environment           (lookupEnv)
import           Text.Printf                  (printf)
import qualified Types.Issue                  as I
import qualified Types.PullRequest            as PR
import           URI.ByteString               (serializeURIRef)
import           URI.ByteString.QQ
import           WebUtils                     (ParamList, Token,
                                               fetchOAuth2AccessToken,
                                               receiveWebRequest)

newtype Html = Html
  { htmlHref :: String
  } deriving (Show, Generic)

instance FromJSON Html where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Links = Links
  { linksSelf     :: Html
  , linksHtml     :: Maybe Html
  , linksComments :: Maybe Html
  } deriving (Show, Generic)

instance FromJSON Links where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Issue = Issue
  { issueId    :: Integer
  , issueTitle :: String
  , issueLinks :: Links
  } deriving (Show, Generic)

instance FromJSON Issue where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data IssueContent = IssueContent
  { issuecontentRaw    :: String
  , issuecontentMarkup :: String
  } deriving (Show, Generic)

instance ToJSON IssueContent where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data IssuePost = IssuePost
  { issuepostTitle   :: String
  , issuepostContent :: IssueContent
  } deriving (Show, Generic)

instance ToJSON IssuePost where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data Issues = Issues
  { issuesSize   :: Integer
  , issuesValues :: [Issue]
  , issuesNext   :: Maybe String
  } deriving (Show, Generic)

instance FromJSON Issues where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data PullRequest = PullRequest
  { pullrequestId    :: Integer
  , pullrequestTitle :: String
  , pullrequestLinks :: Links
  , pullrequestSource :: PullRequestBranch
  , pullrequestDestination :: PullRequestBranch
  } deriving (Show, Generic)

instance FromJSON PullRequest where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data PullRequestPost = PullRequestPost
  { pullrequestpostTitle :: String
  , pullrequestpostSource :: PullRequestBranch
  , pullrequestpostDestination :: PullRequestBranch
  , pullrequestpostDescription :: String
  } deriving (Show, Generic)

instance ToJSON PullRequestPost where
  toJSON = genericToJSON $ aesonPrefix snakeCase

newtype PullRequestBranch = PullRequestBranch
  { branchBranch :: BranchDetails
  } deriving (Show, Generic)

instance FromJSON PullRequestBranch where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON PullRequestBranch where
  toJSON = genericToJSON $ aesonPrefix snakeCase

newtype BranchDetails = BranchDetails
  { branchName :: String
  } deriving (Show, Generic)

instance FromJSON BranchDetails where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON BranchDetails where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data PullRequests = PullRequests
  { pullrequestsSize   :: Integer
  , pullrequestsValues :: [PullRequest]
  , pullrequestsNext   :: Maybe String
  } deriving (Show, Generic)

instance FromJSON PullRequests where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

urlFromIssue :: Issue -> String
urlFromIssue = maybe "" htmlHref . linksHtml . issueLinks

urlFromPullRequest :: PullRequest -> String
urlFromPullRequest  = maybe "" htmlHref . linksHtml . pullrequestLinks

responseToIssue :: Issue -> I.Issue
responseToIssue i =
   I.Issue (Just . show $ issueId i) (issueTitle i) Nothing (Just $ urlFromIssue i)

responseToPullRequest :: PullRequest -> PR.PullRequest
responseToPullRequest pr =
  PR.PullRequest (Just . show $ pullrequestId pr) (pullrequestTitle pr)
                 "" "" Nothing (Just htmlLink)
                   where htmlLink = urlFromPullRequest pr

prToPullRequestPost :: PR.PullRequest -> PullRequestPost
prToPullRequestPost pr = PullRequestPost (PR.title pr) source dest (fromMaybe "" (PR.body pr))
  where source = PullRequestBranch $ BranchDetails (PR.srcBranch pr)
        dest = PullRequestBranch $ BranchDetails (PR.destBranch pr)

baseUrl :: String
baseUrl = "https://api.bitbucket.org/2.0"

reposPath :: RepoInfo -> String
reposPath ri = printf "/repositories/%s/%s" (organization ri) (repository ri)

bitbucketKey :: String -> String -> OAuth2
bitbucketKey clientId clientSecret =
  OAuth2 { oauthClientId = (TL.toStrict . TL.pack) clientId
          , oauthClientSecret = (TL.toStrict . TL.pack) clientSecret
          , oauthCallback = Just [uri|http://127.0.0.1:8080/bitbucketCallback|]
          , oauthOAuthorizeEndpoint = [uri|https://bitbucket.org/site/oauth2/authorize|]
          , oauthAccessTokenEndpoint = [uri|https://bitbucket.org/site/oauth2/access_token|]
}

extractAuthCode :: [QueryItem] -> U8.ByteString
extractAuthCode queryItems = do
  let mbAuthCode = (snd . P.head) $ P.filter (\i -> fst i == U8.fromString "code") queryItems
  fromMaybe (P.error "Could not find authorization code") mbAuthCode

authenticate :: IO String
authenticate = do
  mClientId <- lookupEnv "BITBUCKET_CLIENT_ID"
  case mClientId of
    Nothing -> P.error "Missing Bitbucket Client ID"
    Just clientId -> do
      mClientSecret <- lookupEnv "BITBUCKET_CLIENT_SECRET"
      case mClientSecret of
        Nothing -> P.error "Missing Bitbucket Client ID"
        Just clientSecret -> do
          let oauth2Key = bitbucketKey clientId clientSecret
          let authUrl = BL8.unpack $ toLazyByteString $ serializeURIRef $ authorizationUrl oauth2Key
          P.putStrLn "Please access the below URL:"
          P.putStrLn authUrl
          queryItems <- receiveWebRequest 8080
          let authCode = extractAuthCode queryItems
          fetchOAuth2AccessToken oauth2Key authCode

buildUrl :: String -> Maybe ParamList -> IO (Maybe String)
buildUrl suffix maybeParams = do
  maybeRi <- repoInfoFromRepo
  return $ case maybeRi of
             Just ri -> Just (baseUrl ++ reposPath ri ++ suffix ++ query)
             Nothing -> Nothing
    where query = case maybeParams of
                    Just params -> U8.toString $ renderQuery True params
                    Nothing     -> ""

bearerAuthHeader :: Token -> Options
bearerAuthHeader token = defaults & header "Authorization" .~ [U8.fromString $ "Bearer " ++ token]

getBitbucket :: Token -> String -> IO (Response BL.ByteString)
getBitbucket token = getWith $ bearerAuthHeader token

getIssue :: Token -> String -> IO I.Issue
getIssue token itemId = responseToIssue <$> runItemQuery token path
  where path = "/issues/" ++ itemId

createIssue :: Token -> I.Issue -> IO I.Issue
createIssue token issue = responseToIssue <$> runCreate token "/issues" param
  where param = issueToIssuePost issue

issueToIssuePost :: I.Issue -> IssuePost
issueToIssuePost issue = IssuePost (I.title issue) (IssueContent body "plaintext")
  where body = fromMaybe "" $ I.body issue

listIssues :: String -> Bool -> IO [I.Issue]
listIssues token _ = do
  maybeUrl <- buildUrl "/issues" Nothing
  case maybeUrl of
    Just url -> do
      issues <- getIssuesFromUrl token url
      return $ responseToIssue <$> issues
    Nothing  -> P.error "Could not identify remote URL."

getIssuesFromUrl :: Token -> String -> IO [Issue]
getIssuesFromUrl _ "" = return []
getIssuesFromUrl token url = do
  resp <- getBitbucket token url
  let decoded = decodeResponseOrError resp :: Issues
      issues = issuesValues decoded
  nextIssues <- getIssuesFromUrl token (fromMaybe "" (issuesNext decoded))
  return $ issues ++ nextIssues

getPullRequest :: Token -> String -> IO PR.PullRequest
getPullRequest token itemId = responseToPullRequest <$> runItemQuery token path
  where path = "/pullrequests/" ++ itemId

listPullRequests :: Token -> IO [PR.PullRequest]
listPullRequests token = do
  maybeUrl <- buildUrl "/pullrequests" Nothing
  case maybeUrl of
    Just url -> do
      items <- getPullRequestsFromUrl token url
      return $ responseToPullRequest <$> items
    Nothing  -> P.error "Could not identify remote URL."

createPullRequest :: Token -> PR.PullRequest -> IO PR.PullRequest
createPullRequest token item = responseToPullRequest <$> runCreate token "/pullrequests" param
  where param = prToPullRequestPost item

getPullRequestsFromUrl :: Token -> String -> IO [PullRequest]
getPullRequestsFromUrl _ "" = return []
getPullRequestsFromUrl token url = do
  resp <- getBitbucket token url
  let decoded = decodeResponseOrError resp :: PullRequests
      items = pullrequestsValues decoded
  nextPullRequests <- getPullRequestsFromUrl token (fromMaybe "" (pullrequestsNext decoded))
  return $ items ++ nextPullRequests

runItemQuery :: FromJSON a => Token -> String -> IO a
runItemQuery token suffix = do
  maybeUrl <- buildUrl suffix Nothing
  case maybeUrl of
    Just url -> do
      maybeItem <- decodeResponse <$> getBitbucket token url
      case maybeItem of
        Just item -> return item
        Nothing   -> P.error "Failed to parse response."
    Nothing -> P.error "Could not identify remote URL."

runCreate :: (ToJSON a, FromJSON b) => Token -> String -> a -> IO b
runCreate token suffix param = do
  -- print $ toJSON param
  maybeUrl <- buildUrl suffix Nothing
  case maybeUrl of
    Just url -> decodeResponseOrError <$> postBitbucket token url (toJSON param)
    Nothing -> P.error "Could not identify remote URL."

postBitbucket :: Postable a => Token -> String -> a -> IO (Response BL.ByteString)
postBitbucket token = postWith $ bearerAuthHeader token
