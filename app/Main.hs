{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           CredentialUtils       (Credentials (..), credFilePath,
                                        readCredential, writeCredential)
import           Data.List             (isInfixOf, isPrefixOf, uncons)
import           Data.Maybe            (fromMaybe, listToMaybe)
import           Data.Version          (showVersion)
import           GitUtils              (Branch, getCurrentBranch, getRemoteUrl,
                                        listRemoteBranches)
import           ListUtils             (firstMatching, formatEachAndJoin)
import           Opener                (openEditorWithTempFile)
import           Paths_gwcli           (version)
import           Remote                (authenticate, createIssue,
                                        createPullRequest, defaultBranch,
                                        getIssue, getPullRequest, listIssues,
                                        listPullRequests, open, parseMessage,
                                        readIssueTemplate, readPRTemplate)
import           RemoteTypes           (Remote (..))
import qualified RemoteTypes           as R
import           System.Console.GetOpt (ArgDescr (..), ArgOrder (RequireOrder),
                                        OptDescr (..), getOpt, usageInfo)
import           System.Directory      (removeFile)
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

data IssueCreateOptions =
  IssueCreateOptions { iscoTitle :: String, iscoBody :: String }

defaultIssueCreateOptions :: IssueCreateOptions
defaultIssueCreateOptions = IssueCreateOptions { iscoTitle = "", iscoBody = "" }

issueCreateOptions :: [OptDescr (IssueCreateOptions -> IssueCreateOptions)]
issueCreateOptions =
  [ Option ['t'] ["title"]
      (ReqArg (\title opts -> opts { iscoTitle = title }) "TITLE")
      "Issue title"
  , Option ['m'] ["message"]
      (ReqArg (\msg opts -> opts { iscoBody = msg }) "BODY")
      "Issue message (body)"
  ]

issueFromEditor :: String -> IO IssueCreateOptions
issueFromEditor template = do
  fp <- openEditorWithTempFile template
  content <- readFile fp
  removeFile fp
  let msg = parseMessage content
  return IssueCreateOptions { iscoTitle = R.title msg, iscoBody = R.body msg }

newtype PullRequestListOptions = PullRequestListOptions { prOptAll :: Bool }

defaultPullRequestListOptions :: PullRequestListOptions
defaultPullRequestListOptions = PullRequestListOptions { prOptAll = False }

pullRequestListOptions :: [OptDescr (PullRequestListOptions -> PullRequestListOptions)]
pullRequestListOptions =
  [ Option ['a']["all"]
      (NoArg (\opts -> opts { prOptAll = True }))
       "show all pull requests"
  ]

data PullRequestCreateOptions =
  PullRequestCreateOptions { prcoBase :: String , prcoTitle :: String, prcoBody :: String }

defaultPullRequestCreateOptions :: PullRequestCreateOptions
defaultPullRequestCreateOptions =
  PullRequestCreateOptions { prcoBase = "", prcoTitle = "", prcoBody = "" }

pullRequestCreateOptions :: [OptDescr (PullRequestCreateOptions -> PullRequestCreateOptions)]
pullRequestCreateOptions =
  [ Option ['t'] ["title"]
      (ReqArg (\title opts -> opts { prcoTitle = title }) "TITLE")
      "Pull request title"
  , Option ['b'] ["base"]
      (ReqArg (\base opts -> opts { prcoBase = base }) "BRANCH")
      "Base (destination) branch"
  , Option ['m'] ["message"]
      (ReqArg (\msg opts -> opts { prcoBody = msg }) "BODY")
      "Pull request message (body)"
  ]

candidateBaseBranches :: [Branch]
candidateBaseBranches = ["develop", "main", "master"]

printError :: String -> IO ()
printError = ioError . userError

paramsToIssue :: IssueCreateOptions -> I.Issue
paramsToIssue params = I.Issue Nothing title (Just body) Nothing
  where IssueCreateOptions { iscoTitle = title, iscoBody = body } = params

paramsToPullRequest :: PullRequestCreateOptions -> IO PR.PullRequest
paramsToPullRequest opts = do
  maybeBranch <- getCurrentBranch
  case maybeBranch of
    Just src -> return $ PR.PullRequest Nothing title src base (Just body) Nothing
    Nothing  -> error "Failed to retrieve source branch."
  where
    PullRequestCreateOptions { prcoTitle = title, prcoBase = base, prcoBody = body } = opts

handleIssue :: Remote -> [String] -> IO ()
handleIssue _ [] = printError "Please specify subcommand"
handleIssue remote (ssc:params)
  | ssc `isPrefixOf` "show" = case params of
      [] -> printError "Please specify issue number"
      (issueNum:_) -> getIssue remote issueNum >>= (putStrLn . I.formatIssue)
  | ssc `isPrefixOf` "list" = do
    let (parsed, _, _) = getOpt RequireOrder issueListOptions params
        IssueListOptions { iOptAll = showAll } = foldl (flip id) defaultIssueListOptions parsed
    issues <- listIssues remote showAll
    putStrLn $ formatEachAndJoin issues I.formatIssue
  | ssc `isPrefixOf` "create" = do
      let (parsed, _, _) = getOpt RequireOrder issueCreateOptions params
      template <- addEmptyTitle <$> readIssueTemplate remote
      cParams <- case parsed of
                   [] -> issueFromEditor template
                   _  -> return $ foldl (flip id) defaultIssueCreateOptions parsed
      response <- createIssue remote (paramsToIssue cParams)
      putStrLn $ I.formatIssue response
  | otherwise = printError $ "Subcommand " ++ ssc ++ " not supported"

populateMissingPrco :: PullRequestCreateOptions -> Remote -> IO PullRequestCreateOptions
populateMissingPrco PullRequestCreateOptions{ prcoBase=base, prcoTitle=title, prcoBody=body } remote = do
  newBase <- determineBaseBranch remote base
  R.Message{ R.title=newTitle, R.body=newBody } <- determinePRBody remote title body
  return $ PullRequestCreateOptions { prcoBase=newBase, prcoTitle=newTitle, prcoBody=newBody }

handlePullRequest :: Remote -> [String] -> IO ()
handlePullRequest _ [] = printError "Please specify subcommand"
handlePullRequest remote (ssc:params)
  | ssc `isPrefixOf` "show" = case params of
      [] -> printError "Please specify pull request number"
      (prNum:_) -> getPullRequest remote prNum >>= (putStrLn . PR.formatPullRequest)
  | ssc `isPrefixOf` "list" = do
      let (parsed, _, _) = getOpt RequireOrder pullRequestListOptions params
          PullRequestListOptions { prOptAll = showAll } =
            foldl (flip id) defaultPullRequestListOptions parsed
      prs <- listPullRequests remote showAll
      putStrLn $ formatEachAndJoin prs PR.formatPullRequest
  | ssc `isPrefixOf` "create" = do
      let (parsed, _, _) = getOpt RequireOrder pullRequestCreateOptions params
      let tmpPrco = foldl (flip id) defaultPullRequestCreateOptions parsed
      prco <- populateMissingPrco tmpPrco remote
      pr <- paramsToPullRequest prco
      response <- createPullRequest remote pr
      putStrLn $ PR.formatPullRequest response
  | otherwise = printError $ "Command " ++ ssc ++ " not supported"

determineBaseBranch :: Remote -> String -> IO Branch
determineBaseBranch remote "" = do
  remoteBase <- defaultBranch remote
  case remoteBase of
    Just base -> return base
    Nothing -> do
      remoteBranches <- listRemoteBranches
      return $ fromMaybe "master" (firstMatching remoteBranches candidateBaseBranches)
determineBaseBranch _ specifiedBranch = return specifiedBranch

determinePRBody :: Remote -> String -> String -> IO R.Message
determinePRBody remote "" body = do
  newBody <- case body of
               "" -> readPRTemplate remote
               b  -> return b
  fp <- openEditorWithTempFile (addEmptyTitle newBody)
  content <- readFile fp
  removeFile fp
  return $ parseMessage content
determinePRBody _ title body = return $ R.Message title body

addEmptyTitle :: String -> String
addEmptyTitle = (++) "\n\n"

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

newtype BrowseOptions = BrowseOptions { brOpenBrowser :: Bool }

defaultBrowseOptions :: BrowseOptions
defaultBrowseOptions = BrowseOptions { brOpenBrowser = True }

browseOptions :: [OptDescr (BrowseOptions -> BrowseOptions)]
browseOptions =
  [ Option ['p']["print"]
      (NoArg (\opts -> opts { brOpenBrowser = False }))
      "Only print the URL (instead of opening browser)." ]

handleBrowse :: Remote -> [FilePath] -> IO()
handleBrowse remote params =
  case getOpt RequireOrder browseOptions params of
    (opts, rest, []) ->
      open remote (listToMaybe rest) openBrowser
        where BrowseOptions { brOpenBrowser = openBrowser } =
                foldl (flip id) defaultBrowseOptions opts
    (_, _, errs)  -> printError $ concat errs ++ "Invalid option."

handleShowVersion :: IO ()
handleShowVersion = putStrLn ("gwcli " ++ showVersion version)

dispatchSubcommand :: [String] -> Remote -> Credentials -> FilePath -> IO ()
dispatchSubcommand opts remote c credFP =
  case uncons opts of
    Nothing         -> printError "Please specify subcommand"
    Just (sc, rest) -> handler
      where handler
              | sc `isPrefixOf` "auth"     = handleAuth remote c credFP
              | sc `isPrefixOf` "issue"    = handleIssue remote rest
              | isPullRequestSubCommand sc = handlePullRequest remote rest
              | sc `isPrefixOf` "browse"   = handleBrowse remote rest
              | sc `isPrefixOf` "help"     = handleHelp
              | sc `isPrefixOf` "version"  = handleShowVersion
              | otherwise                  = printError "Please specify subcommand"

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
