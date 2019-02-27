{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Data.List             (isInfixOf)
import           Data.Yaml             (FromJSON, decodeFileEither)
import           GHC.Generics
import           GitHub                (createIssue, createPullRequest,
                                        getIssue, getPullRequest, listIssues,
                                        listPullRequests, open)
import           GitUtils              (getCurrentBranch, getRemoteUrl)
import           ListUtils             (formatEachAndJoin, nthOrDefault,
                                        nthOrNothing)
import           Remote                (Remote (..), Token)
import           System.Console.GetOpt (ArgDescr (..), ArgOrder (RequireOrder),
                                        OptDescr (..), getOpt, usageInfo)
import           System.Directory      (getHomeDirectory)
import           System.Environment    (getArgs)
import           System.FilePath       (joinPath)
import           Text.RawString.QQ
import qualified Types.Issue           as I
import qualified Types.PullRequest     as PR

data Flag = Help | Verbose | Version

data Credentials = Credentials
  { github    :: Token
  , bitbucket :: Token
  } deriving (Show, Generic)

instance FromJSON Credentials

options :: [OptDescr Flag]
options =
  [ Option ['v']["verbose"] (NoArg Verbose) "Verbose output"
  , Option ['h']["help"] (NoArg Help) "Help"
  ]

newtype IssueOptions = IssueOptions { iOptAll :: Bool }
issueOptions :: [OptDescr (IssueOptions -> IssueOptions)]
issueOptions =
  [ Option ['a']["all"]
      (NoArg (\opts -> opts { iOptAll = True }))
       "show all issues"
  ]

defaultIssueOptions :: IssueOptions
defaultIssueOptions = IssueOptions { iOptAll = False }

newtype PullRequestOptions = PullRequestOptions { prOptAll :: Bool }
pullRequestOptions :: [OptDescr (PullRequestOptions -> PullRequestOptions)]
pullRequestOptions =
  [ Option ['a']["all"]
      (NoArg (\opts -> opts { prOptAll = True }))
       "show all pull requests"
  ]

defaultPullRequestOptions :: PullRequestOptions
defaultPullRequestOptions = PullRequestOptions { prOptAll = False }

printError :: String -> IO ()
printError = ioError . userError

readCredential :: FilePath -> IO (Maybe Credentials)
readCredential filepath = do
  file <- decodeFileEither filepath
  case file of
    Left err -> do
      print err
      return Nothing
    Right content -> return content

paramToIssue :: [String] -> I.Issue
paramToIssue params = I.Issue Nothing title body Nothing
  where title = head params
        body = nthOrNothing params 1

paramsToPullRequest :: [String] -> IO PR.PullRequest
paramsToPullRequest params = do
  maybeBranch <- getCurrentBranch
  case maybeBranch of
    Just src -> return $ PR.PullRequest Nothing title src dest body Nothing
    Nothing  -> error "Failed to retrieve source branch."
  where title = head params
        dest = nthOrDefault params "master" 1
        body = nthOrNothing params 2

handleIssue :: Remote -> [String] -> IO ()
handleIssue remote params =
  case subsubcommand of
    "show"   -> getIssue remote (head rest) >>= (putStrLn . I.formatIssue)
    "list"   -> do
      issues <- listIssues remote showAll
      putStrLn $ formatEachAndJoin issues I.formatIssue
        where (parsed, _, _) = getOpt RequireOrder issueOptions rest
              IssueOptions { iOptAll = showAll } = foldl (flip id) defaultIssueOptions parsed
    "create" -> createIssue remote (paramToIssue rest)
                  >>= (putStrLn . I.formatIssue)
    _      -> printError $ "Subcommand " ++ subsubcommand ++ " not supported"
    where subsubcommand = head params
          rest = tail params

handlePullRequest :: Remote -> [String] -> IO ()
handlePullRequest remote params =
  case subsubcommand of
    "show"   -> getPullRequest remote (head rest) >>= (putStrLn . PR.formatPullRequest)
    "list"   -> do
      prs <- listPullRequests remote showAll
      putStrLn $ formatEachAndJoin prs PR.formatPullRequest
        where (parsed, _, _) = getOpt RequireOrder pullRequestOptions rest
              PullRequestOptions { prOptAll = showAll } = foldl (flip id) defaultPullRequestOptions parsed
    "create" -> do
      pr <- paramsToPullRequest rest
      response <- createPullRequest remote pr
      putStrLn $ PR.formatPullRequest response
    _        -> printError $ "Command " ++ subsubcommand ++ " not supported"
    where subsubcommand = head params
          rest = tail params

remoteUrlToRemote :: String -> Credentials -> Remote
remoteUrlToRemote url cred
  | "bitbucket.org" `isInfixOf` url = Bitbucket (bitbucket cred)
  | "github.com"    `isInfixOf` url = GitHub (github cred)
  | otherwise = error "Could not determine remote URL"

chooseRemote :: Credentials -> IO Remote
chooseRemote c = do
  remoteUrl <- getRemoteUrl
  case remoteUrl of
    Nothing  -> error "Could not determine remote URL."
    Just url -> return $ remoteUrlToRemote url c

handleHelp :: IO ()
handleHelp = putStr [r|issue create|show|list
pullrequest create|show|list
browse
help
|]

main :: IO ()
main = do
  args <- getArgs
  homeDir <- getHomeDirectory
  cred <- readCredential $ joinPath [homeDir, ".gwcli.yaml"]
  case cred of
    Nothing -> printError "Failed to read credentials file."
    Just c -> do
      remote <- chooseRemote c
      case getOpt RequireOrder options args of
        (_, n, [])   ->
          case head n of
            "issue"       -> handleIssue remote (tail n)
            "pullrequest" -> handlePullRequest remote (tail n)
            "browse"      -> open remote
            "help"        -> handleHelp
            _             -> printError "Please specify subcommand"
        (_, _, errs) -> printError $ concat errs ++ usageInfo header options
      where header = "Usage: gwcli subcommand"
