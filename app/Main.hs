{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           CredentialUtils       (Credentials (..), credFilePath,
                                        readCredential, writeCredential)
import           Data.List             (isInfixOf, isPrefixOf)
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

newtype IssueListOptions = IssueListOptions { iOptAll :: Bool }

defaultIssueListOptions :: IssueListOptions
defaultIssueListOptions = IssueListOptions { iOptAll = False }

issueListOptions :: [OptDescr (IssueListOptions -> IssueListOptions)]
issueListOptions =
  [ Option ['a']["all"]
      (NoArg (\opts -> opts { iOptAll = True }))
       "show all issues"
  ]

newtype PullRequestListOptions = PullRequestListOptions { prOptAll :: Bool }

defaultPullRequestListOptions :: PullRequestListOptions
defaultPullRequestListOptions = PullRequestListOptions { prOptAll = False }

pullRequestListOptions :: [OptDescr (PullRequestListOptions -> PullRequestListOptions)]
pullRequestListOptions =
  [ Option ['a']["all"]
      (NoArg (\opts -> opts { prOptAll = True }))
       "show all pull requests"
  ]

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
handleIssue remote params
  | ssc `isPrefixOf` "show" = getIssue remote (head rest) >>= (putStrLn . I.formatIssue)
  | ssc `isPrefixOf` "list" = do
    let (parsed, _, _) = getOpt RequireOrder issueListOptions rest
        IssueListOptions { iOptAll = showAll } = foldl (flip id) defaultIssueListOptions parsed
    issues <- listIssues remote showAll
    putStrLn $ formatEachAndJoin issues I.formatIssue
  | ssc `isPrefixOf` "create" = createIssue remote (paramToIssue rest)
                  >>= (putStrLn . I.formatIssue)
  | otherwise = printError $ "Subcommand " ++ ssc ++ " not supported"
    where ssc = head params
          rest = tail params

handlePullRequest :: Remote -> [String] -> IO ()
handlePullRequest remote params
  | ssc `isPrefixOf` "show" = getPullRequest remote (head rest) >>= (putStrLn . PR.formatPullRequest)
  | ssc `isPrefixOf` "list" = do
      let (parsed, _, _) = getOpt RequireOrder pullRequestListOptions rest
          PullRequestListOptions { prOptAll = showAll } =
            foldl (flip id) defaultPullRequestListOptions parsed
      prs <- listPullRequests remote showAll
      putStrLn $ formatEachAndJoin prs PR.formatPullRequest
  | ssc `isPrefixOf` "create" = do
      pr <- paramsToPullRequest rest
      response <- createPullRequest remote pr
      putStrLn $ PR.formatPullRequest response
  | otherwise = printError $ "Command " ++ ssc ++ " not supported"
    where ssc = head params
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

isPullRequestSubCommand :: String -> Bool
isPullRequestSubCommand cmd = isPrefixOf "pullrequest" cmd || cmd == "pr"

dispatchSubcommand :: [String] -> Remote -> Credentials -> FilePath -> IO ()
dispatchSubcommand opts remote c credFP
  | sc `isPrefixOf` "auth"     = handleAuth remote c credFP
  | sc `isPrefixOf` "issue"    = handleIssue remote rest
  | isPullRequestSubCommand sc = handlePullRequest remote rest
  | sc `isPrefixOf` "browse"   = open remote
  | sc `isPrefixOf` "help"     = handleHelp
  | otherwise                  = printError "Please specify subcommand"
    where (sc : rest) = opts

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
        (_, n, [])   -> dispatchSubcommand n remote c credFP
        (_, _, errs) -> printError $ concat errs ++ usageInfo header options
      where header = "Usage: gwcli subcommand"
