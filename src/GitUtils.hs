module GitUtils
  (
    Branch
  , RepoInfo(..)
  , repoInfoFromRepo
  , getRemoteUrl
  , getCurrentBranch
  , listBranches
  , listRemotes
  ) where

import           Data.Git            (refNameRaw)
import           Data.Git.Named      (looseRemotesList)
import           Data.Git.Repository (branchList, configGet, headGet)
import           Data.Git.Storage    (findRepoMaybe, openRepo, withCurrentRepo)
import           Data.List           (isPrefixOf, isSuffixOf)
import           Data.String.Utils   (replace)
import           Network.URI         (parseURI, pathSegments)
import           Text.Printf         (printf)

data RepoInfo = RepoInfo {
  organization :: String,
  repository   :: String
} deriving (Show)

type Branch = String

repoInfoFromRepo :: IO (Maybe RepoInfo)
repoInfoFromRepo = do
  maybe Nothing urlToRepoInfo <$> getRemoteUrl

getCurrentBranch :: IO (Maybe Branch)
getCurrentBranch = either (const Nothing) (Just . refNameRaw) <$> withCurrentRepo headGet

getRemoteUrl :: IO (Maybe String)
getRemoteUrl = do
  maybePath <- findRepoMaybe
  case maybePath of
    Just path -> do
      repo <- openRepo path
      configGet repo (remoteSection "origin") "url"
    Nothing   -> return Nothing

remoteSection :: String -> String
remoteSection = printf "remote \"%s\""

dropDotGit :: String -> String
dropDotGit = reverse . drop 4 . reverse

segmentsToRepoInfo :: [String] -> RepoInfo
segmentsToRepoInfo segs = RepoInfo (head segs) repoName
  where second = segs !! 1
        repoName = if ".git" `isSuffixOf` second then dropDotGit second else second

toFullSshUrl :: String -> String
toFullSshUrl str =
  if "ssh://" `isPrefixOf` str
    then str
    else "ssh://" ++ replace ":" "/" str

urlToRepoInfo :: String -> Maybe RepoInfo
urlToRepoInfo url = do
  uri <- parseURI $ toFullSshUrl url
  return $ (segmentsToRepoInfo . pathSegments) uri

listBranches = do
  maybePath <- findRepoMaybe
  case maybePath of
    Just path -> do
      repo <- openRepo path
      branchList repo
    Nothing   -> Prelude.error "aaaa"

listRemotes = do
  maybePath <- findRepoMaybe
  case maybePath of
    Just path -> do
      looseRemotesList path
    Nothing   -> Prelude.error "aaaa"
