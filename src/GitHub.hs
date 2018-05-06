{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub where

import           Control.Lens.Operators  ((.~), (^.))
import           Data.Aeson              (FromJSON (parseJSON), decode,
                                          defaultOptions, fieldLabelModifier,
                                          genericParseJSON)
import           Data.Aeson.Casing       (aesonPrefix, snakeCase)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.UTF8    as U8
import           Data.Function           ((&))
import           Data.List               (intercalate)
import           Data.Maybe              (fromMaybe)
import           Data.String.Conversions (convertString)
import           GHC.Generics
import           GitUtils                (RepoInfo (..), repoInfoFromRepo)
import           Network.Wreq            (Options, Response, defaults, getWith,
                                          header, linkURL, responseBody,
                                          responseLink)
import           Text.Printf             (printf)

data Issue = Issue {
  issueNumber  :: Integer,
  issueHtmlUrl :: String,
  issueTitle   :: String
} deriving (Show, Generic)

instance FromJSON Issue where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

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

gitHubHeader :: String -> Options
gitHubHeader token = defaults & header "Authorization" .~ [U8.fromString $ "token " ++ token]

getGitHub :: Maybe String -> String -> IO (Response BL.ByteString)
getGitHub token = getWith opt
  where opt = maybe defaults gitHubHeader token

formatIssue :: Issue -> String
formatIssue i = printf "#%d\n%s\n%s" (issueNumber i) (issueTitle i) (issueHtmlUrl i)

formatPull :: Pull -> String
formatPull i = printf "#%d\n%s\n%s" (pullNumber i) (pullTitle i) (pullHtmlUrl i)

readItems :: FromJSON a => Response BL.ByteString -> [a]
readItems resp = fromMaybe [] items
  where items = decode (resp ^. responseBody)

readNextLink :: Response BL.ByteString -> U8.ByteString
readNextLink resp = resp ^. responseLink "rel" "next" . linkURL

getItemsFromUrl :: FromJSON a => Maybe String -> String -> IO [a]
getItemsFromUrl token "" = return []
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

getIssues :: [String] -> Maybe String -> IO ()
getIssues sscmds token = do
  maybeUrl <- buildUrl "/issues"
  case maybeUrl of
    Just url -> do
      issues <- getItemsFromUrl token url :: IO [Issue]
      putStrLn $ intercalate "\n" (fmap formatIssue issues)
    Nothing -> error "Could not identify remote URL."

getPulls :: [String] -> Maybe String -> IO ()
getPulls sscmds token = do
  maybeUrl <- buildUrl "/pulls"
  case maybeUrl of
    Just url -> do
       pulls <- getItemsFromUrl token url :: IO [Pull]
       putStrLn $ intercalate "\n" (fmap formatPull pulls)
    Nothing -> error "Could not identify remote URL."

