{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens.Operators  ((.~), (^.))
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import           Data.Function           ((&))
import           Data.String.Conversions (convertString)
import           Data.Yaml               (FromJSON, decodeFile)
import           GHC.Generics
import           Lib
import           Network.Wreq            (Options, Response, defaults, getWith,
                                          header, responseBody)
import           Prelude
import           System.Console.GetOpt   (ArgDescr (..),
                                          ArgOrder (RequireOrder),
                                          OptDescr (..), getOpt, usageInfo)
import           System.Environment      (getArgs)

gitHubBaseUrl :: String
gitHubBaseUrl = "https://api.github.com"

data Flag =
  Help |
  Issues |
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

readCredential :: String -> IO (Maybe Credentials)
readCredential = decodeFile

gitHubHeader :: IO Options
gitHubHeader = do
  cred <- readCredential "credentials.yaml"
  case cred of
    Just c -> return (defaults & header "Authorization" .~ [BS.append "token " token])
      where token = convertString (github c)
    Nothing ->
      return defaults

getGitHub :: String -> IO (Response BL.ByteString)
getGitHub path = do
  opt <- gitHubHeader
  getWith opt (gitHubBaseUrl ++ path)

getIssues :: [String] -> [Flag] -> IO ()
getIssues sscmds o = do
  resp <- getGitHub "/repos/organization/repo/issues"
  let body = resp ^. responseBody in
    print body

getPullRequests :: [String] -> [Flag] -> IO ()
getPullRequests sscmds o = print sscmds

main :: IO ()
main = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (o, n, [])   ->
      case head n of
        "issues" -> getIssues (tail n) o
        "pulls"  -> getPullRequests (tail n) o
        _        -> printError "Please specify subcommand"
    (_, _, errs) -> printError $ concat errs ++ usageInfo header options
  where header = "Usage: gwcli subcommand"
