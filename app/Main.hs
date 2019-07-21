{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           CredentialUtils       (Credentials (..), credFilePath,
                                        readCredential, writeCredential)
import           Data.List             (isInfixOf)
import           GitUtils              (getCurrentBranch, getRemoteUrl)
import           ListUtils             (formatEachAndJoin, nthOrDefault,
                                        nthOrNothing)
import           Remote                (authenticate, createIssue,
                                        createPullRequest, getIssue,
                                        getPullRequest, listIssues,
                                        listPullRequests, open)
import           RemoteTypes           (Remote (..))
import           System.Console.GetOpt (ArgDescr (..), ArgOrder (RequireOrder),
                                        OptDescr (..), getOpt, usageInfo)
import           System.Environment    (getArgs)
import           Text.RawString.QQ
import qualified Types.Issue           as I
import qualified Types.PullRequest     as PR
import           WebUtils              as WU

data Flag = Help | Verbose | Version

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

handleAuth :: Remote -> Credentials -> FilePath -> IO ()
handleAuth remote creds credFP = do
  tokens <- authenticate remote
  putStrLn "Fetched access token."
  let newCreds = Credentials { github = github creds, bitbucket = tokens }
  writeCredential credFP newCreds

remoteUrlToRemote :: String -> Credentials -> Remote
remoteUrlToRemote url cred
  | "bitbucket"  `isInfixOf` url = Bitbucket (WU.accessToken . bitbucket $ cred)
  | "github.com" `isInfixOf` url = GitHub (github cred)
  | otherwise = error "Could not determine remote URL"

chooseRemote :: Credentials -> IO Remote
chooseRemote c = do
  remoteUrl <- getRemoteUrl
  case remoteUrl of
    Nothing  -> error "Could not determine remote URL."
    Just url -> return $ remoteUrlToRemote url c

handleHelp :: IO ()
handleHelp = putStr [r|
auth
issue create|show|list
pullrequest create|show|list
browse
help
|]

main :: IO ()
main = do
  args <- getArgs
  credFP <- credFilePath
  cred <- readCredential credFP
  case cred of
    Nothing -> printError "Failed to read credentials file."
    Just c -> do
      remote <- chooseRemote c
      case getOpt RequireOrder options args of
        (_, n, [])   ->
          case head n of
            "auth"        -> handleAuth remote c credFP
            "issue"       -> handleIssue remote (tail n)
            "pullrequest" -> handlePullRequest remote (tail n)
            "browse"      -> open remote
            "help"        -> handleHelp
            _             -> printError "Please specify subcommand"
        (_, _, errs) -> printError $ concat errs ++ usageInfo header options
      where header = "Usage: gwcli subcommand"
