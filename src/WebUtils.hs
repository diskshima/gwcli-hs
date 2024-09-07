#!/usr/bin/env stack
-- stack --resolver lts-13.8 --install-ghc runghc --package HTTP
-- Taken from: https://stackoverflow.com/a/37824454
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module WebUtils
  (
    ParamList
  , Token
  , Tokens(..)
  , fetchOAuth2AccessToken
  , receiveWebRequest
  , refreshOAuth2AccessToken
  , toParamList
  ) where

import           Control.Monad.Except              (runExceptT)
import qualified Data.ByteString.UTF8              as U8
import           Data.String.Conversions           (convertString)
import           Data.Yaml                         (FromJSON, ToJSON)
import           GHC.Generics                      (Generic)
import           Network.HTTP                      (Request (..), Response (..),
                                                    close, receiveHTTP,
                                                    respondHTTP,
                                                    socketConnection)
import           Network.HTTP.Conduit              (newManager,
                                                    tlsManagerSettings)
import           Network.HTTP.Types.URI            (QueryItem, parseQuery)
import           Network.OAuth.OAuth2              (ExchangeToken (..),
                                                    OAuth2 (..), OAuth2Token,
                                                    RefreshToken (..),
                                                    TokenResponseError,
                                                    accessToken, atoken,
                                                    fetchAccessToken,
                                                    refreshAccessToken,
                                                    refreshToken, rtoken)
import           Network.Socket
import           Network.URI
import           Prelude                           as P

type Token = String

data Tokens = Tokens
  { accessToken  :: Token
  , refreshToken :: Token
  } deriving (Show, Generic)

instance FromJSON Tokens
instance ToJSON Tokens

type ParamList = [(U8.ByteString, Maybe U8.ByteString)]

type AccessTokenRespE = Either TokenResponseError OAuth2Token

toParamList :: [(String, String)] -> ParamList
toParamList = P.map (\(k, v) -> (U8.fromString k, Just $ U8.fromString v))

receiveWebRequest :: Int -> IO [QueryItem]
receiveWebRequest portNum = do
  lsock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints { addrFlags = [AI_PASSIVE] , addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show portNum))
  bind lsock (addrAddress addr)
  listen lsock 1
  (csock, _) <- accept lsock
  hs <- socketConnection "" portNum csock
  req <- receiveHTTP hs
  case req of
    Left _ -> error "Receiving request failed"
    Right Request {..} -> do
      let queryItems = parseQuery $ U8.fromString $ uriQuery rqURI
      respondHTTP hs $ Response (2,0,0) "OK" [] "OK"
      Network.HTTP.close hs
      return queryItems

fetchOAuth2AccessToken :: OAuth2 -> U8.ByteString -> IO Tokens
fetchOAuth2AccessToken oauth2 authCode = do
  let textAuthCode = convertString authCode
  manager <- newManager tlsManagerSettings
  resp <- runExceptT (fetchAccessToken manager oauth2 ExchangeToken { extoken = textAuthCode })
  return $ responseToTokens resp

refreshOAuth2AccessToken :: OAuth2 -> U8.ByteString -> IO Tokens
refreshOAuth2AccessToken oauth2 refreshToken = do
  let textRefreshToken = convertString refreshToken
  manager <- newManager tlsManagerSettings
  resp <- runExceptT (refreshAccessToken manager oauth2 RefreshToken { rtoken = textRefreshToken })
  return $ responseToTokens resp

responseToTokens :: AccessTokenRespE -> Tokens
responseToTokens resp =
  Tokens { accessToken = newAccessToken, refreshToken = newRefreshToken }
    where newAccessToken = extractAccessToken resp
          newRefreshToken = extractRefreshToken resp

extractAccessToken :: AccessTokenRespE -> String
extractAccessToken (Left err) = P.error $ show err
extractAccessToken (Right token) = (convertString . atoken . Network.OAuth.OAuth2.accessToken) token

extractRefreshToken :: AccessTokenRespE -> String
extractRefreshToken (Left err) = P.error $ show err
extractRefreshToken (Right token) =
  maybe (P.error "No refresh token") (convertString . rtoken) (Network.OAuth.OAuth2.refreshToken token)

-- main :: IO ()
-- main = do
--   queryItems <- receiveWebRequest 8080
--   print queryItems
