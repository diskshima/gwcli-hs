{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub where

import           Control.Lens.Operators  ((.~), (^.))
import           Data.Aeson              (FromJSON, decode)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.UTF8    as U8
import           Data.Function           ((&))
import           Data.String.Conversions (convertString)
import           GHC.Generics
import           Network.Wreq            (Options, Response, defaults, getWith,
                                          header, responseBody)

data Issue = Issue {
  id       :: Integer,
  html_url :: String,
  title    :: String
} deriving (Show, Generic)

instance FromJSON Issue

gitHubBaseUrl :: String
gitHubBaseUrl = "https://api.github.com"

gitHubHeader :: String -> Options
gitHubHeader token = defaults & header "Authorization" .~ [U8.fromString $ "token " ++ token]

getGitHub :: Maybe String -> String -> IO (Response BL.ByteString)
getGitHub token path = getWith opt (gitHubBaseUrl ++ path)
  where opt = maybe defaults gitHubHeader token

getIssues :: [String] -> Maybe String -> IO ()
getIssues sscmds token = do
  resp <- getGitHub token "/repos/organization/repo/issues"
  let result = decode (resp ^. responseBody) :: Maybe [Issue] in
    print result
