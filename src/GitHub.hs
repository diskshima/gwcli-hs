{-# LANGUAGE DeriveGeneric     #-}
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
import           Data.List              (intercalate)
import           Data.Maybe             (fromMaybe)
import           GHC.Generics
import           GitUtils               (RepoInfo (..), repoInfoFromRepo)
import           Network.Wreq           (Options, Response, defaults, getWith,
                                         header, linkURL, postWith,
                                         responseBody, responseLink)
import           Network.Wreq.Types     (Postable)
import           Opener                 (openUrl)
import           Text.Printf            (printf)
import           Types                  (IssueDetails (..), PRDetails (..))

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

data PRPost = PRPost {
  prpostTitle :: String,
  prpostHead  :: String,
  prpostBase  :: String,
  prpostBody  :: Maybe String
} deriving (Show, Generic)

instance ToJSON PRPost where
  toJSON = genericToJSON $ aesonPrefix snakeCase

data Pull = Pull {
  pullNumber  :: Integer,
  pullHtmlUrl :: String,
  pullTitle   :: String
} deriving (Show, Generic)

instance FromJSON Pull where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

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

formatIssue :: IssueGet -> String
formatIssue i = printf "#%d: %s\n%s" (issuegetNumber i) (issuegetTitle i) (issuegetHtmlUrl i)

formatPull :: Pull -> String
formatPull i = printf "#%d: %s\n%s" (pullNumber i) (pullTitle i) (pullHtmlUrl i)

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

runItemQuery :: FromJSON a => Maybe String -> String -> (a -> String) -> IO String
runItemQuery token suffix format = do
  maybeUrl <- buildUrl suffix
  case maybeUrl of
    Just url -> do
      item <- getGitHub token url
      return $ maybe "" format (readItem item)
    Nothing  -> error "Could not identify remote URL."

runListQuery :: FromJSON a => Maybe String -> String -> (a -> String) -> IO String
runListQuery token suffix format = do
  maybeUrl <- buildUrl suffix
  case maybeUrl of
    Just url -> do
      items <- getItemsFromUrl token url
      return $ intercalate "\n" (fmap format items)
    Nothing  -> error "Could not identify remote URL."

getIssue :: [String] -> Maybe String -> IO ()
getIssue sscmds token =
  runItemQuery token path formatIssue >>= putStrLn
    where path = "/issues/" ++ head sscmds

getPR :: [String] -> Maybe String -> IO ()
getPR sscmds token =
  runItemQuery token path formatPull >>= putStrLn
    where path = "/pulls/" ++ head sscmds

getIssues :: [String] -> Maybe String -> IO ()
getIssues _ token =
  runListQuery token "/issues" formatIssue >>= putStrLn

getPRs :: [String] -> Maybe String -> IO ()
getPRs _ token =
  runListQuery token "/pulls" formatPull >>= putStrLn

issueDetailsToIssuePost :: IssueDetails -> IssuePost
issueDetailsToIssuePost issueDetails =
  IssuePost (idTitle issueDetails) (idBody issueDetails)

prDetailsToPRPost :: PRDetails -> PRPost
prDetailsToPRPost prDetails =
  PRPost (prTitle prDetails) (prSrcBranch prDetails) (prDestBranch prDetails)
    (prBody prDetails)

runCreate :: (ToJSON a, FromJSON b) => Maybe String -> String -> a -> (b -> String) -> IO ()
runCreate token suffix param format = do
  maybeUrl <- buildUrl suffix
  case maybeUrl of
    Just url -> do
      resp <- postGitHub token url (toJSON param)
      putStrLn $ maybe "Failed to read response." format (readItem resp)
    Nothing -> error "Could not identify remote URL."

createIssue :: IssueDetails -> Maybe String -> IO ()
createIssue details token = runCreate token "/issues" param formatIssue
  where param = issueDetailsToIssuePost details

createPR :: PRDetails -> Maybe String -> IO ()
createPR details token = runCreate token "/pulls" param formatPull
  where param = prDetailsToPRPost details

open :: IO ()
open = do
  maybeRi <- repoInfoFromRepo
  case maybeRi of
    Just ri -> openUrl $ browserPath ri
    Nothing -> error "Could not identify repo info."
