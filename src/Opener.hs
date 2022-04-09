module Opener where

import           Data.Maybe           (fromMaybe)
import           System.Environment   (lookupEnv)
import           System.IO.Temp       (writeTempFile)
import           System.Info          (os)
import           System.Process       (callCommand)
import           System.Process.Typed

openCmd :: String
openCmd =
  case os of
    "darwin"  -> "open"
    "linux"   -> "xdg-open"
    "windows" -> "start launchy /b"
    _         -> error $ "Unknown os type: " ++ os

openUrl :: String -> IO ()
openUrl url = callCommand $ openCmd ++ " " ++ url

openEditorWithTempFile :: String -> IO FilePath
openEditorWithTempFile content = do
  fp <- writeTempFile "." "tmp" content
  mbEditor <- lookupEnv "EDITOR"
  let editor = fromMaybe (error "Missing credentials") mbEditor
  _ <- runProcess (proc editor [fp])
  return fp
