{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Data.Yaml             (FromJSON, decodeFileEither)
import           GHC.Generics
import           GitHub                (GitHub (..))
import           GitUtils              (getCurrentBranch)
import           ListUtils             (formatEachAndJoin, nthOrDefault,
                                        nthOrNothing)
import           Remote                (Remote, Token, createIssue,
                                        createPullRequest, getIssue,
                                        getPullRequest, listIssues,
                                        listPullRequests, open)
import           System.Console.GetOpt (ArgDescr (..), ArgOrder (RequireOrder),
                                        OptDescr (..), getOpt, usageInfo)
import           System.Directory      (getHomeDirectory)
import           System.Environment    (getArgs)
import           System.FilePath       (joinPath)
import           Text.RawString.QQ
import qualified Types.Issue           as I
import qualified Types.PullRequest     as PR

data Flag =
  Help |
  Verbose |
  Version

data Credentials = Credentials {
  github :: Token,
  zenhub :: Token
} deriving (Show, Generic)

instance FromJSON Credentials

options :: [OptDescr Flag]
options = [
  Option ['v']["verbose"] (NoArg Verbose) "Verbose output",
  Option ['h']["help"] (NoArg Help) "Help"
          ]

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

handleIssue :: Remote a => a -> [String] -> IO ()
handleIssue remote params =
  case subsubcommand of
    "show"   -> getIssue remote (head rest) >>= (putStrLn . I.formatIssue)
    "list"   -> do
      issues <- listIssues remote
      putStrLn $ formatEachAndJoin issues I.formatIssue
    "create" -> createIssue remote (paramToIssue rest)
                  >>= (putStrLn . I.formatIssue)
    _      -> printError $ "Subcommand " ++ subsubcommand ++ " not supported"
    where subsubcommand = head params
          rest = tail params

handlePullRequest :: Remote a => a -> [String] -> IO ()
handlePullRequest remote params =
  case subsubcommand of
    "show"   -> getPullRequest remote (head rest) >>= (putStrLn . PR.formatPullRequest)
    "list"   -> do
      prs <- listPullRequests remote
      putStrLn $ formatEachAndJoin prs PR.formatPullRequest
    "create" -> do
      pr <- paramsToPullRequest rest
      response <- createPullRequest remote pr
      putStrLn $ PR.formatPullRequest response
    _        -> printError $ "Command " ++ subsubcommand ++ " not supported"
    where subsubcommand = head params
          rest = tail params

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
      let remote = GitHub $ github c
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
