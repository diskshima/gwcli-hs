{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Yaml             (FromJSON, decodeFileEither)
import           GHC.Generics
import           GitHub                (getIssues)
import           System.Console.GetOpt (ArgDescr (..), ArgOrder (RequireOrder),
                                        OptDescr (..), getOpt, usageInfo)
import           System.Environment    (getArgs)

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

readCredential :: FilePath -> IO (Maybe Credentials)
readCredential filepath = do
  file <- decodeFileEither filepath
  case file of
    Left err -> do
      print err
      return Nothing
    Right content -> return content

main :: IO ()
main = do
  args <- getArgs
  cred <- readCredential "credentials.yaml"
  case getOpt RequireOrder options args of
    (o, n, [])   ->
      case head n of
        "issues" -> getIssues (tail n) (fmap github cred)
        _        -> printError "Please specify subcommand"
    (_, _, errs) -> printError $ concat errs ++ usageInfo header options
  where header = "Usage: gwcli subcommand"
