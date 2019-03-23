{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module BitbucketApi
  (
    listIssues
  ) where

import           Control.Lens.Operators ((.~), (^.))
import           Data.Aeson             (FromJSON (parseJSON), decode,
                                         genericParseJSON)
import           Data.Aeson.Casing      (aesonPrefix, snakeCase)
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.UTF8   as U8
import           Data.Function          ((&))
import           Data.Maybe             (fromMaybe)
import           GHC.Generics
import           GitUtils               (RepoInfo (..), repoInfoFromRepo)
import           Network.HTTP.Types.URI (renderQuery)
import           Network.Wreq           (Options, Response, defaults, getWith,
                                         header, responseBody)
import           Prelude                as P
import           Text.Printf            (printf)
import qualified Types.Issue            as I
import           WebUtils               (ParamList, Token)

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
