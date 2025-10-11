{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           CommandLineParser     (AuthOptions (..), BrowseOptionsCli (..),
                                        CliArguments (..), Command (..),
                                        GlobalOptions (..), IssueCommand (..),
                                        IssueCreateOptionsCli (..),
                                        IssueListOptionsCli (..),
                                        IssueShowOptions (..), PullRequestCommand (..),
                                        PullRequestCreateOptionsCli (..),
                                        PullRequestListOptionsCli (..),
                                        PullRequestShowOptions (..), parseCliArgs)
import           CredentialUtils       (Credentials (..), credFilePath,
                                        readCredential, writeCredential)
import           Data.List             (isInfixOf, isPrefixOf)
import           Data.Maybe            (fromMaybe)
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
                                        readPRTemplate)
import           RemoteTypes           (Remote (..))
import qualified RemoteTypes           as R
import           System.Directory      (removeFile)

import qualified Types.Issue           as I
import qualified Types.PullRequest     as PR
import           WebUtils              as WU

data IssueCreateOptions = IssueCreateOptions
    { iscoTitle    :: String
    , iscoBody     :: String
    , iscoShowHelp :: Bool
    }

issueFromEditor :: String -> IO IssueCreateOptions
issueFromEditor template = do
  fp <- openEditorWithTempFile template
  content <- readFile fp
  removeFile fp
  let msg = parseMessage content
  return IssueCreateOptions { iscoTitle = R.title msg, iscoBody = R.body msg, iscoShowHelp = False }

candidateBaseBranches :: [Branch]
candidateBaseBranches = ["develop", "main", "master"]

printError :: String -> IO ()
printError = ioError . userError

paramsToIssue :: IssueCreateOptionsCli -> I.Issue
paramsToIssue params = I.Issue Nothing (icoTitle params) (Just (icoBody params)) Nothing

paramsToPullRequest :: PullRequestCreateOptionsCli -> IO PR.PullRequest
paramsToPullRequest opts = do
    maybeBranch <- getCurrentBranch
    case maybeBranch of
        Just src -> return $ PR.PullRequest Nothing (prcoTitle opts) src (prcoBase opts) (Just (prcoBody opts)) Nothing
        Nothing  -> error "Failed to retrieve source branch."

handleIssue :: Bool -> Remote -> IssueCommand -> IO ()
handleIssue _verbose remote issueCmd = case issueCmd of
    IssueList opts -> do
        issues <- listIssues remote (iloAll opts)
        putStrLn $ formatEachAndJoin issues I.formatIssue
    IssueCreate opts -> do
        let newIssue = paramsToIssue opts
        response <- createIssue remote newIssue
        putStrLn $ I.formatIssue response
    IssueShow opts ->
        getIssue remote (isoIssueNumber opts) >>= (putStrLn . I.formatIssue)

handlePullRequest :: Bool -> Remote -> PullRequestCommand -> IO ()
handlePullRequest _verbose remote prCmd = case prCmd of
    PullRequestList opts -> do
        prs <- listPullRequests remote (prloAll opts)
        putStrLn $ formatEachAndJoin prs PR.formatPullRequest
    PullRequestCreate opts -> do
        pr <- paramsToPullRequest opts
        response <- createPullRequest remote pr
        putStrLn $ PR.formatPullRequest response
    PullRequestShow opts ->
        getPullRequest remote (prsoPrNumber opts) >>= (putStrLn . PR.formatPullRequest)

populateMissingPrco :: PullRequestCreateOptionsCli -> Remote -> IO PullRequestCreateOptionsCli
populateMissingPrco PullRequestCreateOptionsCli{prcoBase=base, prcoTitle=title, prcoBody=body} remote = do
    newBase <- determineBaseBranch remote base
    R.Message{R.title=newTitle, R.body=newBody} <- determinePRBody remote title body
    return $ PullRequestCreateOptionsCli {prcoBase=newBase, prcoTitle=newTitle, prcoBody=newBody}

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

handleAuth :: Bool -> Credentials -> FilePath -> Remote -> AuthOptions -> IO ()
handleAuth _verbose creds credFP remote _authOpts = do
    tokens <- authenticate remote
    putStrLn "Fetched access token."
    let newCreds = Credentials {github = github creds, bitbucket = tokens}
    writeCredential credFP newCreds

remoteUrlToRemote :: String -> Credentials -> Remote
remoteUrlToRemote url cred
    | "bitbucket"  `isInfixOf` url = Bitbucket (WU.accessToken . bitbucket $ cred)
    | "github.com" `isInfixOf` url = GitHub (github cred)
    | otherwise                    = error "Could not determine remote URL"

chooseRemote :: Credentials -> IO Remote
chooseRemote c = do
    remoteUrl <- getRemoteUrl
    case remoteUrl of
        Nothing  -> error "Could not determine remote URL."
        Just url -> return $ remoteUrlToRemote url c

isPullRequestSubCommand :: String -> Bool
isPullRequestSubCommand cmd = isPrefixOf "pullrequest" cmd || cmd == "pr"

handleBrowse :: Bool -> Remote -> BrowseOptionsCli -> IO ()
handleBrowse _verbose remote browseOpts =
    open remote (boUrl browseOpts) (not (boPrint browseOpts))

handleShowVersion :: IO ()
handleShowVersion = putStrLn ("gwcli " ++ showVersion version)

globalOptionsToVerboseOpt :: GlobalOptions -> Bool
globalOptionsToVerboseOpt = optVerbose

executeCommand :: CliArguments -> Credentials -> FilePath -> Remote -> IO ()
executeCommand (CliArguments globalOpts cmd) creds credFP remote =
    case cmd of
        AuthCmd authOpts     -> handleAuth (globalOptionsToVerboseOpt globalOpts) creds credFP remote authOpts
        BrowseCmd browseOpts -> handleBrowse (globalOptionsToVerboseOpt globalOpts) remote browseOpts
        IssueCmd issueCmd    -> handleIssue (globalOptionsToVerboseOpt globalOpts) remote issueCmd
        PullRequestCmd prCmd -> handlePullRequest (globalOptionsToVerboseOpt globalOpts) remote prCmd
        VersionCmd _         -> handleShowVersion

main :: IO ()
main = do
    let versionStr = "gwcli " ++ showVersion version
    parsedArgs <- parseCliArgs versionStr

    credFP <- credFilePath
    cred <- readCredential credFP
    case cred of
        Nothing -> printError "Failed to read credentials file. Please run 'gwcli auth' or check your .gwcli.yaml."
        Just c -> do
            remote <- chooseRemote c
            executeCommand parsedArgs c credFP remote

