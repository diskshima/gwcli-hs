{-# LANGUAGE DeriveGeneric #-}

module CredentialUtils
  (
    Credentials(..)
  , readCredential
  , credFilePath
  , writeCredential
  ) where

import           Data.Yaml        (FromJSON, ToJSON, decodeFileEither,
                                   encodeFile)
import           GHC.Generics
import           System.Directory (getHomeDirectory)
import           System.FilePath  (joinPath)
import           WebUtils         (Token, Tokens)

data Credentials = Credentials
  { github    :: Token
  , bitbucket :: Tokens
  } deriving (Show, Generic)

instance FromJSON Credentials
instance ToJSON Credentials

credFilePath :: IO String
credFilePath = do
  homeDir <- getHomeDirectory
  return $ joinPath [homeDir, ".gwcli.yaml"]

readCredential :: FilePath -> IO (Maybe Credentials)
readCredential filepath = do
  file <- decodeFileEither filepath
  case file of
    Left err -> do
      print err
      return Nothing
    Right content -> return content

writeCredential :: FilePath -> Credentials -> IO ()
writeCredential = encodeFile
