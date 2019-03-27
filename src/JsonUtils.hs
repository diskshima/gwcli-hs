module JsonUtils
  (
    decodeResponse
  , decodeResponseAsList
  , decodeResponseOrError
  ) where

import           Control.Lens.Operators ((^.))
import           Data.Aeson             (FromJSON, decode)
import qualified Data.ByteString.Lazy   as BL
import           Data.Maybe             (fromMaybe)
import           Network.Wreq           (Response, responseBody)

decodeResponse :: FromJSON a => Response BL.ByteString -> Maybe a
decodeResponse resp = decode (resp ^. responseBody)

decodeResponseAsList :: FromJSON a => Response BL.ByteString -> [a]
decodeResponseAsList resp = fromMaybe [] items
  where items = decode (resp ^. responseBody)

decodeResponseOrError :: FromJSON a => Response BL.ByteString -> a
decodeResponseOrError resp = fromMaybe (error "Failed to parse response") $ decodeResponse resp
