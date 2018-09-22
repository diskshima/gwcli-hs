{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Data.Yaml             (FromJSON, decodeFileEither)
import           GHC.Generics
import           GitHub                (createIssue, createPR, getIssue,
                                        getIssues, getPR, getPRs, open)
import           ListUtils             (nthOrDefault, nthOrNothing)
import           System.Console.GetOpt (ArgDescr (..), ArgOrder (RequireOrder),
                                        OptDescr (..), getOpt, usageInfo)
import           System.Directory      (getHomeDirectory)
import           System.Environment    (getArgs)
import           System.FilePath       (joinPath)
import           Text.RawString.QQ
import           Types                 (IssueDetails (..), PRDetails (..))

data Flag =
  Help |
  Verbose |
  Version

data Credentials = Credentials {
  zenhub :: String,
  github :: String
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

paramToIssueDetails :: [String] -> IssueDetails
paramToIssueDetails params = IssueDetails title body
  where title = head params
        body = nthOrNothing params 1

paramsToPRDetails :: [String] -> PRDetails
paramsToPRDetails params = PRDetails title src dest body
  where title = head params
        src = params !! 1
        dest = nthOrDefault params "master" 2
        body = nthOrNothing params 3

handleIssue :: [String] -> Maybe String -> IO ()
handleIssue params token =
  case subsubcommand of
    "show"   -> getIssue rest token
    "list"   -> getIssues rest token
    "create" -> createIssue (paramToIssueDetails rest) token
    _      -> printError $ "Subcommand " ++ subsubcommand ++ " not supported"
    where subsubcommand = head params
          rest = tail params

handlePR :: [String] -> Maybe String -> IO ()
handlePR params token =
  case subsubcommand of
    "show"   -> getPR rest token
    "list"   -> getPRs rest token
    "create" -> createPR (paramsToPRDetails rest) token
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
  case getOpt RequireOrder options args of
    (_, n, [])   ->
      case head n of
        "issue"       -> handleIssue (tail n) (fmap github cred)
        "pullrequest" -> handlePR (tail n) (fmap github cred)
        "browse"      -> open
        "help"        -> handleHelp
        _             -> printError "Please specify subcommand"
    (_, _, errs) -> printError $ concat errs ++ usageInfo header options
  where header = "Usage: gwcli subcommand"
