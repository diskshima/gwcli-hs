{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module BitbucketApi
  (
    authenticate
  , getIssue
  , listIssues
  ) where

import           Control.Lens.Operators       ((.~), (^.))
import           Data.Aeson                   (FromJSON (parseJSON), decode,
                                               genericParseJSON)
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
import           Network.HTTP.Types.URI       (QueryItem, renderQuery)
import           Network.OAuth.OAuth2         (OAuth2 (..), authorizationUrl)
import           Network.Wreq                 (Options, Response, defaults,
                                               getWith, header, responseBody)
import           Prelude                      as P
import           System.Environment           (lookupEnv)
import           Text.Printf                  (printf)
import qualified Types.Issue                  as I
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
  { linksHtml     :: Html
  , linksComments :: Html
  } deriving (Show, Generic)

instance FromJSON Links where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Issue = Issue
  { issueId    :: Integer
  , issueTitle :: String
  , issueLinks :: Links
  } deriving (Show, Generic)

instance FromJSON Issues where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Issues = Issues
  { issuesSize   :: Integer
  , issuesValues :: [Issue]
  , issuesNext   :: Maybe String
  } deriving (Show, Generic)

instance FromJSON Issue where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

urlFromIssue :: Issue -> String
urlFromIssue = htmlHref . linksHtml . issueLinks

responseToIssue :: Issue -> I.Issue
responseToIssue i =
   I.Issue (Just . show $ issueId i) (issueTitle i) Nothing (Just $ urlFromIssue i)

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
getIssue token itemId = do
  maybeUrl <- buildUrl ("/issues/" ++ itemId) Nothing
  case maybeUrl of
    Just url -> do
      resp <- getBitbucket token url
      let decoded = decodeResponse resp :: Issue
      return $ responseToIssue decoded
    Nothing  -> P.error "Could not build URL."

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
  let decoded = decodeResponse resp :: Issues
      issues = issuesValues decoded
  nextIssues <- getIssuesFromUrl token (fromMaybe "" (issuesNext decoded))
  return $ issues ++ nextIssues

decodeResponse :: FromJSON a => Response BL.ByteString -> a
decodeResponse resp = fromMaybe (error "Failed to parse response") decoded
  where decoded = decode (resp ^. responseBody)
