module Opener where

import           System.Info    (os)
import           System.Process (callCommand)

openCmd :: String
openCmd =
  case os of
    "darwin"  -> "open"
    "linux"   -> "xdg-open"
    "windows" -> "start launchy /b"
    _         -> error $ "Unknown os type: " ++ os

openUrl :: String -> IO ()
openUrl url = callCommand $ openCmd ++ " " ++ url
