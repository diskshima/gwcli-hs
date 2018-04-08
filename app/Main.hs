module Main where

import           Lib
import           System.Console.GetOpt (ArgDescr (..), ArgOrder (RequireOrder),
                                        OptDescr (..), getOpt, usageInfo)
import           System.Environment    (getArgs)

data Flag =
  Verbose |
  Version |
  Issues

options :: [OptDescr Flag]
options = [
  Option ['v']["verbose"] (NoArg Verbose) "Verbose output"
          ]

main :: IO ()
main = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (o, n, [])   -> print n
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: gwcli subcommand"
