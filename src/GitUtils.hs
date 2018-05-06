module GitUtils
  (
    RepoInfo(..)
  , repoInfoFromRepo
  , getRemoteUrl
  ) where

import           Data.Git.Repository (configGet)
import           Data.Git.Storage    (findRepoMaybe, openRepo)
import           Data.List           (isSuffixOf)
import           Network.URI         (parseURI, pathSegments)
import           Text.Printf         (printf)

data RepoInfo = RepoInfo {
  organization :: String,
  repository   :: String
} deriving (Show)

repoInfoFromRepo :: IO (Maybe RepoInfo)
repoInfoFromRepo = do
  maybeUrl <- getRemoteUrl
  return $ case maybeUrl of
             Just uri -> urlToRepoInfo uri
             Nothing  -> Nothing

getRemoteUrl :: IO (Maybe String)
getRemoteUrl = do
  path <- findRepoMaybe
  case path of
    Just path -> getRemoteUrlInner path
    Nothing   -> return Nothing

remoteSection :: String -> String
remoteSection = printf "remote \"%s\""

getRemoteUrlInner path = do
  repo <- openRepo path
  configGet repo (remoteSection "origin") "url"

dropDotGit :: String -> String
dropDotGit = reverse . drop 4 . reverse

segmentsToRepoInfo :: [String] -> RepoInfo
segmentsToRepoInfo segs = RepoInfo (head segs) repoName
  where second = segs !! 1
        repoName = if ".git" `isSuffixOf` second then dropDotGit second else second

urlToRepoInfo :: String -> Maybe RepoInfo
urlToRepoInfo url = do
  uri <- parseURI url
  return $ (segmentsToRepoInfo . pathSegments) uri
