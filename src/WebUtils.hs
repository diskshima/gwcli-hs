#!/usr/bin/env stack
-- stack --resolver lts-13.8 --install-ghc runghc --package HTTP
-- Taken from: https://stackoverflow.com/a/37824454
{-# LANGUAGE RecordWildCards #-}

module WebUtils
  (
    ParamList
  , Token
  , receiveWebRequest
  , toParamList
  ) where

import           Data.ByteString.UTF8   (fromString)
import qualified Data.ByteString.UTF8   as U8
import           Network.HTTP
import           Network.HTTP.Types.URI (QueryItem, parseQuery)
import           Network.Socket
import           Network.URI
import           Prelude                as P

type Token = String
type ParamList = [(U8.ByteString, Maybe U8.ByteString)]

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
      let queryItems = parseQuery $ fromString $ uriQuery rqURI
      respondHTTP hs $ Response (2,0,0) "OK" [] "OK"
      Network.HTTP.close hs
      return queryItems

-- main :: IO ()
-- main = do
--   queryItems <- receiveWebRequest 8080
--   print queryItems
