#!/usr/bin/env stack
-- stack --resolver lts-13.8 --install-ghc runghc --package HTTP
-- Taken from: https://stackoverflow.com/a/37824454
{-# LANGUAGE RecordWildCards #-}

import           Data.ByteString.UTF8   (fromString)
import           Network.HTTP
import           Network.HTTP.Types.URI (parseQuery)
import           Network.Socket
import           Network.URI

main = do
  lsock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints { addrFlags = [AI_PASSIVE] , addrSocketType = Stream }
  let portnum = 8080
  addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show portnum))
  bind lsock (addrAddress addr)
  listen lsock 1
  (csock, _) <- accept lsock
  hs <- socketConnection "" portnum csock
  req <- receiveHTTP hs
  case req of
    Left _ -> error "Receiving request failed"
    Right Request {..} -> if uriPath rqURI == "/"
                          then do
                            respondHTTP hs $ Response (2,0,0) "OK" [] "Hello HTTP"
                            Network.HTTP.close hs
                          else do
                            print $ uriPath rqURI
                            print $ parseQuery $ fromString $ uriQuery rqURI
                            respondHTTP hs $ Response (4,0,4) "Not found" [] "Nothing here"
                            Network.HTTP.close hs
