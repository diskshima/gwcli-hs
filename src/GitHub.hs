{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module GitHub where

import           Control.Lens.Operators          ((.~), (^.))
import           Data.Aeson                      (FromJSON (parseJSON),
                                                  ToJSON (toJSON), decode,
                                                  genericParseJSON,
                                                  genericToJSON)
import           Data.Aeson.Casing               (aesonPrefix, snakeCase)
import qualified Data.ByteString.Lazy            as BL
import           Data.ByteString.Lazy.Builder    (toLazyByteString)
import           Data.ByteString.Lazy.Char8      as BL8
import qualified Data.ByteString.UTF8            as U8
import           Data.Function                   ((&))
import           Data.Maybe                      (fromMaybe)
import           Data.String.Conversions         (convertString)
import           Data.Text.Lazy                  as TL
import           GHC.Generics
import           GitUtils                        (RepoInfo (..),
                                                  repoInfoFromRepo)
import           Network.HTTP.Conduit            (newManager,
                                                  tlsManagerSettings)
import           Network.HTTP.Types.URI          (QueryItem, renderQuery)
import           Network.OAuth.OAuth2            (ExchangeToken (..),
                                                  OAuth2 (..), accessToken,
                                                  atoken, authorizationUrl)
import           Network.OAuth.OAuth2.HttpClient (fetchAccessToken)
import           Network.Wreq                    (Options, Response, defaults,
                                                  getWith, header, linkURL,
                                                  postWith, responseBody,
                                                  responseLink)
import           Network.Wreq.Types              (Postable)
import           Opener                          (openUrl)
import           Prelude                         as P
import           Remote                          (Remote (..), Token)
import           System.Environment              (lookupEnv)
import           Text.Printf                     (printf)
import qualified Types.Issue                     as I
import qualified Types.PullRequest               as PR
import           URI.ByteString                  (serializeURIRef)
import           URI.ByteString.QQ
import           WebUtils                        (receiveWebRequest)

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
    Nothing -> P.error "Could not identify repo info."

browserPath :: RepoInfo -> Remote -> String
browserPath ri (GitHub _) = printf "https://github.com/%s/%s" (organization ri) (repository ri)
browserPath ri (Bitbucket _) = printf "https://bitbucket.org/%s/%s" (organization ri) (repository ri)

bitbucketKey :: String -> String -> OAuth2
bitbucketKey clientId clientSecret =
  OAuth2 { oauthClientId = (TL.toStrict . TL.pack) clientId
          , oauthClientSecret = (TL.toStrict . TL.pack) clientSecret
          , oauthCallback = Just [uri|http://127.0.0.1:8080/bitbucketCallback|]
          , oauthOAuthorizeEndpoint = [uri|https://bitbucket.org/site/oauth2/authorize|]
          , oauthAccessTokenEndpoint = [uri|https://bitbucket.org/site/oauth2/access_token|]
}

fetchOAuth2AccessToken :: OAuth2 -> U8.ByteString -> IO String
fetchOAuth2AccessToken oauth2 authCode = do
  manager <- newManager tlsManagerSettings
  let textAuthCode = convertString authCode
  resp <- fetchAccessToken manager oauth2 ExchangeToken { extoken = textAuthCode }
  case resp of
    Left err    -> P.error $ show err
    Right token -> return $ (convertString . atoken . accessToken) token

extractAuthCode :: [QueryItem] -> U8.ByteString
extractAuthCode queryItems = do
  let mbAuthCode = (snd . P.head) $ P.filter (\i -> fst i == U8.fromString "code") queryItems
  fromMaybe (P.error "Could not find authorization code") mbAuthCode

authenticate :: Remote -> IO String
authenticate (GitHub _) = undefined
authenticate (Bitbucket _) = do
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
        Nothing   -> P.error "Failed to parse response."
    Nothing -> P.error "Could not identify remote URL."

toParamList :: [(String, String)] -> ParamList
toParamList = P.map (\(k, v) -> (U8.fromString k, Just $ U8.fromString v))

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
      maybeItem <- readItem <$> postGitHub token url (toJSON param)
      case maybeItem of
        Just item -> return item
        Nothing   -> P.error "Failed to parse response."
    Nothing -> P.error "Could not identify remote URL."
