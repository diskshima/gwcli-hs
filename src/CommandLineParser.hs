module CommandLineParser
  ( parseCommandLine
  , Flag(..)
  , IssueListOptions(..)
  , IssueCreateOptions(..)
  , PullRequestListOptions(..)
  , PullRequestCreateOptions(..)
  , defaultIssueListOptions
  , defaultIssueCreateOptions
  , defaultPullRequestListOptions
  , defaultPullRequestCreateOptions
  , issueListOptions
  , issueCreateOptions
  , pullRequestListOptions
  , pullRequestCreateOptions
  ) where

import           System.Console.GetOpt (ArgDescr (..), ArgOrder (RequireOrder),
                                        OptDescr (..), getOpt, usageInfo)

data Flag = Help | Verbose | Version

options :: [OptDescr Flag]
options =
  [ Option ['v']["verbose"] (NoArg Verbose) "Verbose output"
  , Option ['h']["help"] (NoArg Help) "Help"
  ]

newtype IssueListOptions = IssueListOptions { iOptAll :: Bool }

defaultIssueListOptions :: IssueListOptions
defaultIssueListOptions = IssueListOptions { iOptAll = False }

issueListOptions :: [OptDescr (IssueListOptions -> IssueListOptions)]
issueListOptions =
  [ Option ['a']["all"]
      (NoArg (\opts -> opts { iOptAll = True }))
       "show all issues"
  ]

data IssueCreateOptions =
  IssueCreateOptions { iscoTitle :: String, iscoBody :: String }

defaultIssueCreateOptions :: IssueCreateOptions
defaultIssueCreateOptions = IssueCreateOptions { iscoTitle = "", iscoBody = "" }

issueCreateOptions :: [OptDescr (IssueCreateOptions -> IssueCreateOptions)]
issueCreateOptions =
  [ Option ['t'] ["title"]
      (ReqArg (\title opts -> opts { iscoTitle = title }) "TITLE")
      "Issue title"
  , Option ['m'] ["message"]
      (ReqArg (\msg opts -> opts { iscoBody = msg }) "BODY")
      "Issue message (body)"
  ]

newtype PullRequestListOptions = PullRequestListOptions { prOptAll :: Bool }

defaultPullRequestListOptions :: PullRequestListOptions
defaultPullRequestListOptions = PullRequestListOptions { prOptAll = False }

pullRequestListOptions :: [OptDescr (PullRequestListOptions -> PullRequestListOptions)]
pullRequestListOptions =
  [ Option ['a']["all"]
      (NoArg (\opts -> opts { prOptAll = True }))
       "show all pull requests"
  ]

data PullRequestCreateOptions =
  PullRequestCreateOptions { prcoBase :: String , prcoTitle :: String, prcoBody :: String }

defaultPullRequestCreateOptions :: PullRequestCreateOptions
defaultPullRequestCreateOptions =
  PullRequestCreateOptions { prcoBase = "", prcoTitle = "", prcoBody = "" }

pullRequestCreateOptions :: [OptDescr (PullRequestCreateOptions -> PullRequestCreateOptions)]
pullRequestCreateOptions =
  [ Option ['t'] ["title"]
      (ReqArg (\title opts -> opts { prcoTitle = title }) "TITLE")
      "Pull request title"
  , Option ['b'] ["base"]
      (ReqArg (\base opts -> opts { prcoBase = base }) "BRANCH")
      "Base (destination) branch"
  , Option ['m'] ["message"]
      (ReqArg (\msg opts -> opts { prcoBody = msg }) "BODY")
      "Pull request message (body)"
  ]

parseCommandLine :: [String] -> ([Flag], [String])
parseCommandLine args =
  case getOpt RequireOrder options args of
    (opts, n, []) -> (opts, n)
    (_, _, errs)  -> error $ concat errs ++ usageInfo header options
  where header = "Usage: gwcli subcommand"
