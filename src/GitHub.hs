{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub where

import           Control.Lens.Operators  ((.~), (^.))
import           Data.Aeson              (FromJSON (parseJSON), decode,
                                          defaultOptions, fieldLabelModifier,
                                          genericParseJSON)
import           Data.Aeson.Casing       (snakeCase)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.UTF8    as U8
import           Data.Function           ((&))
import           Data.List               (intercalate)
import           Data.Maybe              (fromMaybe)
import           Data.String.Conversions (convertString)
import           GHC.Generics
import           Network.Wreq            (Options, Response, defaults, getWith,
                                          header, responseBody)
import           Text.Printf             (printf)

data Issue = Issue {
  id      :: Integer,
  htmlUrl :: String,
  title   :: String
} deriving (Show, Generic)

instance FromJSON Issue where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = snakeCase }

gitHubBaseUrl :: String
gitHubBaseUrl = "https://api.github.com"

gitHubHeader :: String -> Options
gitHubHeader token = defaults & header "Authorization" .~ [U8.fromString $ "token " ++ token]

getGitHub :: Maybe String -> String -> IO (Response BL.ByteString)
getGitHub token path = getWith opt (gitHubBaseUrl ++ path)
  where opt = maybe defaults gitHubHeader token

formatIssue :: Issue -> String
formatIssue i = printf "%s: %s" (title i) (htmlUrl i)

getIssues :: [String] -> Maybe String -> IO ()
getIssues sscmds token = do
  resp <- getGitHub token "/repos/organization/repo/issues"
  let result = decode (resp ^. responseBody) :: Maybe [Issue]
      issues = fromMaybe [] result in
    putStrLn $ intercalate "\n" (fmap formatIssue issues)
