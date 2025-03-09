{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module BitbucketApi
  (
    authenticate
  , createIssue
  , createPullRequest
  , getIssue
  , getPullRequest
  , listIssues
  , listPullRequests
  , readIssueTemplate
  , readPRTemplate
  ) where

import           Bitbucket.Common           as BC
import           Bitbucket.Issue            as BI (Issue (..),
                                                   IssueContent (..),
                                                   Issues (..))
import           Bitbucket.Issue            as BIP (IssuePost (..))
import           Bitbucket.PullRequest      as BP (BranchDetails (..),
                                                   PullRequest (..),
                                                   PullRequestBranch (..),
                                                   PullRequests (..))
import           Bitbucket.PullRequest      as BPP (PullRequestPost (..))
import           Control.Lens               ((^.))
import           Control.Lens.Operators     ((.~))
import           CredentialUtils            (Credentials (..), credFilePath,
                                             readCredential, writeCredential)
import           Data.Aeson                 (FromJSON, ToJSON (..))
import           Data.ByteString.Builder    (toLazyByteString)
import qualified Data.ByteString.Lazy       as BL
import           Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.UTF8       as U8
import           Data.Function              ((&))
import           Data.Maybe                 (fromMaybe)
import           Data.String.Conversions    (convertString)
import           Data.Text.Lazy             as TL
import           GitUtils                   (RepoInfo (..), repoInfoFromRepo)
import           JsonUtils                  (decodeResponse,
                                             decodeResponseOrError)
import           Network.HTTP.Types.URI     (QueryItem, renderQuery)
import           Network.OAuth.OAuth2       (OAuth2 (..), authorizationUrl)
import           Network.Wreq               (Options, Response, defaults,
                                             getWith, header, postWith,
                                             responseStatus, statusCode)
import           Network.Wreq.Types         (Postable)
import           Prelude                    as P
import           System.Environment         (lookupEnv)
import           Text.Printf                (printf)
import qualified Types.Issue                as I
import qualified Types.PullRequest          as PR
import           URI.ByteString             (serializeURIRef)
import           URI.ByteString.QQ
import           WebUtils                   (ParamList, Token, Tokens (..),
                                             fetchOAuth2AccessToken,
                                             receiveWebRequest,
                                             refreshOAuth2AccessToken)

urlFromIssue :: BI.Issue -> String
urlFromIssue = maybe "" BC.href . BC.html . BI.links

urlFromPullRequest :: BP.PullRequest -> String
urlFromPullRequest  = maybe "" BC.href . BC.html . BP.links

responseToIssue :: Issue -> I.Issue
responseToIssue i =
  I.Issue (Just . P.show $ BI.id i) (BI.title i) Nothing (Just $ urlFromIssue i)

responseToPullRequest :: PullRequest -> PR.PullRequest
responseToPullRequest pr =
  PR.PullRequest (Just . P.show $ BP.id pr) (BP.title pr)
                 "" "" Nothing (Just htmlLink)
                   where htmlLink = urlFromPullRequest pr

prToPullRequestPost :: PR.PullRequest -> PullRequestPost
prToPullRequestPost pr = PullRequestPost (PR.title pr) src dest (fromMaybe "" (PR.body pr))
  where src = PullRequestBranch $ BranchDetails (PR.srcBranch pr)
        dest = PullRequestBranch $ BranchDetails (PR.destBranch pr)

baseUrl :: String
baseUrl = "https://api.bitbucket.org/2.0"

reposPath :: RepoInfo -> String
reposPath ri = printf "/repositories/%s/%s" (organization ri) (repository ri)

bitbucketKey :: String -> String -> OAuth2
bitbucketKey clientId clientSecret =
  OAuth2 { oauth2ClientId = (TL.toStrict . TL.pack) clientId
    , oauth2ClientSecret = TL.toStrict . TL.pack $ clientSecret
    , oauth2RedirectUri = [uri|http://127.0.0.1:8080/bitbucketCallback|]
    , oauth2AuthorizeEndpoint = [uri|https://bitbucket.org/site/oauth2/authorize|]
    , oauth2TokenEndpoint = [uri|https://bitbucket.org/site/oauth2/access_token|]
         }

buildBitbucketKey :: IO OAuth2
buildBitbucketKey = do
  mClientId <- lookupEnv "BITBUCKET_CLIENT_ID"
  case mClientId of
    Nothing -> P.error "Missing Bitbucket Client ID"
    Just clientId -> do
      mClientSecret <- lookupEnv "BITBUCKET_CLIENT_SECRET"
      case mClientSecret of
        Nothing           -> P.error "Missing Bitbucket Client Secret"
        Just clientSecret -> return $ bitbucketKey clientId clientSecret

extractAuthCode :: [QueryItem] -> U8.ByteString
extractAuthCode queryItems = maybe errorMsg P.id $ lookup "code" queryItems >>= P.id
  where
    errorMsg = P.error "Failed to extract auth code."

authenticate :: IO Tokens
authenticate = do
  oauth2Key <- buildBitbucketKey
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

refreshAccessToken :: IO Token
refreshAccessToken = do
  filepath <- credFilePath
  mbCreds <- readCredential filepath
  let creds = fromMaybe (P.error "Missing credentials") mbCreds
  let bbRefreshToken = WebUtils.refreshToken . bitbucket $ creds
  oauth2 <- buildBitbucketKey
  tokens <- refreshOAuth2AccessToken oauth2 (convertString bbRefreshToken)
  let newCreds = Credentials {
      github = github creds
    , bitbucket = tokens
                             }
  writeCredential filepath newCreds
  return $ WebUtils.accessToken tokens

getBitbucket :: Token -> String -> IO (Response BL.ByteString)
getBitbucket token url = do
  response <- getWith (bearerAuthHeader token) url
  let code = response ^. (responseStatus . statusCode)
  case code of
    401 -> do
      newToken <- refreshAccessToken
      getWith (bearerAuthHeader newToken) url
    _   -> return response

getIssue :: Token -> String -> IO I.Issue
getIssue token itemId = responseToIssue <$> runItemQuery token path
  where path = "/issues/" ++ itemId

createIssue :: Token -> I.Issue -> IO I.Issue
createIssue token issue = responseToIssue <$> runCreate token "/issues" param
  where param = issueToIssuePost issue

issueToIssuePost :: I.Issue -> IssuePost
issueToIssuePost issue = IssuePost (I.title issue) (BI.IssueContent body "plaintext")
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
  let decoded = decodeResponseOrError resp :: BI.Issues
      issues = BI.values decoded
  nextIssues <- getIssuesFromUrl token (fromMaybe "" (BI.next decoded))
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
      items = BP.values decoded
  nextPullRequests <- getPullRequestsFromUrl token (fromMaybe "" (BP.next decoded))
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
    Nothing  -> P.error "Could not identify remote URL."

readIssueTemplate :: IO String
readIssueTemplate = return ""

readPRTemplate :: IO String
readPRTemplate = return ""

postBitbucket :: Postable a => Token -> String -> a -> IO (Response BL.ByteString)
postBitbucket token = postWith $ bearerAuthHeader token
