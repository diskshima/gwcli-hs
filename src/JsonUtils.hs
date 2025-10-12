module JsonUtils
  (
    decodeResponse
  , decodeResponseAsList
  , decodeResponseOrError
  ) where

import           Control.Lens.Operators ((^.))
import           Data.Aeson             (FromJSON, decode)
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Maybe             (fromMaybe)
import           Network.Wreq           (Response, responseBody, responseStatus, statusCode)

decodeResponse :: FromJSON a => Response BL.ByteString -> Maybe a
decodeResponse resp = decode (resp ^. responseBody)

decodeResponseAsList :: FromJSON a => Response BL.ByteString -> [a]
decodeResponseAsList resp = fromMaybe [] items
  where items = decode (resp ^. responseBody)

decodeResponseOrError :: FromJSON a => Response BL.ByteString -> a
decodeResponseOrError resp = fromMaybe (error errorMsg) $ decodeResponse resp
  where
    code = resp ^. (responseStatus . statusCode)
    body = BL8.unpack $ resp ^. responseBody
    bodyPreview = if length body > 200 then take 200 body ++ "..." else body
    errorMsg = "Failed to parse response.\nHTTP Status: " ++ show code ++ "\nResponse body: " ++ bodyPreview
