module CommandLineParser
  ( parseCommandLine
  , Flag(..)
  , IssueListOptions(..)
  , IssueCreateOptions(..)
  , PullRequestListOptions(..)
  , PullRequestCreateOptions(..)
  , BrowseOptions(..) -- Added BrowseOptions
  , defaultIssueListOptions
  , defaultIssueCreateOptions
  , defaultPullRequestListOptions
  , defaultPullRequestCreateOptions
  , defaultBrowseOptions -- Added defaultBrowseOptions
  , issueListOptions
  , issueCreateOptions
  , pullRequestListOptions
  , pullRequestCreateOptions
  , browseOptions -- Added browseOptions
  , globalUsageInfo
  , issueUsageHeader
  , issueListUsageInfo
  , issueCreateUsageInfo
  , prUsageHeader
  , prListUsageInfo
  , prCreateUsageInfo
  , browseUsageInfo
  ) where

import           System.Console.GetOpt (ArgDescr (..), ArgOrder (RequireOrder),
                                        OptDescr (..), getOpt, usageInfo)
-- import           Debug.Trace (trace) -- Removed debugging import

data Flag = Help | Verbose | Version deriving (Show, Eq) -- Added deriving Eq for Flag

options :: [OptDescr Flag]
options =
  [ Option ['v']["verbose"] (NoArg Verbose) "Verbose output"
  , Option ['h']["help"] (NoArg Help) "Help"
  ]

-- Issue List Options
data IssueListOptions = IssueListOptions { iOptAll :: Bool, ilOptShowHelp :: Bool } -- Changed newtype to data

defaultIssueListOptions :: IssueListOptions
defaultIssueListOptions = IssueListOptions { iOptAll = False, ilOptShowHelp = False }

issueListOptions :: [OptDescr (IssueListOptions -> IssueListOptions)]
issueListOptions =
  [ Option ['a']["all"]
      (NoArg (\opts -> opts { iOptAll = True }))
       "show all issues"
  , Option ['h'] ["help"] (NoArg (\opts -> opts { ilOptShowHelp = True })) "Show help for list issues"
  ]

-- Issue Create Options
data IssueCreateOptions =
  IssueCreateOptions { iscoTitle :: String, iscoBody :: String, iscoShowHelp :: Bool }

defaultIssueCreateOptions :: IssueCreateOptions
defaultIssueCreateOptions = IssueCreateOptions { iscoTitle = "", iscoBody = "", iscoShowHelp = False }

issueCreateOptions :: [OptDescr (IssueCreateOptions -> IssueCreateOptions)]
issueCreateOptions =
  [ Option ['t'] ["title"]
      (ReqArg (\title opts -> opts { iscoTitle = title }) "TITLE")
      "Issue title"
  , Option ['m'] ["message"]
      (ReqArg (\msg opts -> opts { iscoBody = msg }) "BODY")
      "Issue message (body)"
  , Option ['h'] ["help"] (NoArg (\opts -> opts { iscoShowHelp = True })) "Show help for create issue"
  ]

-- Pull Request List Options
data PullRequestListOptions = PullRequestListOptions { prOptAll :: Bool, prOptShowHelp :: Bool } -- Changed newtype to data

defaultPullRequestListOptions :: PullRequestListOptions
defaultPullRequestListOptions = PullRequestListOptions { prOptAll = False, prOptShowHelp = False }

pullRequestListOptions :: [OptDescr (PullRequestListOptions -> PullRequestListOptions)]
pullRequestListOptions =
  [ Option ['a']["all"]
      (NoArg (\opts -> opts { prOptAll = True }))
       "show all pull requests"
  , Option ['h'] ["help"] (NoArg (\opts -> opts { prOptShowHelp = True })) "Show help for list pull requests"
  ]

-- Pull Request Create Options
data PullRequestCreateOptions =
  PullRequestCreateOptions { prcoBase :: String , prcoTitle :: String, prcoBody :: String, prcoShowHelp :: Bool }

defaultPullRequestCreateOptions :: PullRequestCreateOptions
defaultPullRequestCreateOptions =
  PullRequestCreateOptions { prcoBase = "", prcoTitle = "", prcoBody = "", prcoShowHelp = False }

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
  , Option ['h'] ["help"] (NoArg (\opts -> opts { prcoShowHelp = True })) "Show help for create pull request"
  ]

-- Browse Options
data BrowseOptions = BrowseOptions { brOpenBrowser :: Bool, brShowHelp :: Bool } -- Changed newtype to data

defaultBrowseOptions :: BrowseOptions
defaultBrowseOptions = BrowseOptions { brOpenBrowser = True, brShowHelp = False }

browseOptions :: [OptDescr (BrowseOptions -> BrowseOptions)]
browseOptions =
  [ Option ['p']["print"]
      (NoArg (\opts -> opts { brOpenBrowser = False }))
      "Only print the URL (instead of opening browser)."
  , Option ['h'] ["help"] (NoArg (\opts -> opts { brShowHelp = True })) "Show help for browse"
  ]

parseCommandLine :: [String] -> ([Flag], [String])
parseCommandLine args =
  case getOpt RequireOrder options args of
    (o, n, []) -> (o, n)
    (_, _, errs) -> error $ concat errs ++ usageInfo header options
  where header = "Usage: gwcli [GLOBAL OPTIONS] <subcommand> [SUBCOMMAND OPTIONS]"

-- Global options help
globalUsageInfo :: String
globalUsageInfo = usageInfo header options
  where header = "Usage: gwcli [GLOBAL OPTIONS] <subcommand> [SUBCOMMAND OPTIONS]"

-- Issue subcommand help
issueUsageHeader :: String
issueUsageHeader = "Usage: gwcli issue <create|list|show> [OPTIONS]"

issueListUsageInfo :: String
issueListUsageInfo = usageInfo ("Usage: gwcli issue list [OPTIONS]") issueListOptions

issueCreateUsageInfo :: String
issueCreateUsageInfo = usageInfo ("Usage: gwcli issue create [OPTIONS]") issueCreateOptions

-- Pull Request subcommand help
prUsageHeader :: String
prUsageHeader = "Usage: gwcli pullrequest <create|list|show> [OPTIONS]"

prListUsageInfo :: String
prListUsageInfo = usageInfo ("Usage: gwcli pullrequest list [OPTIONS]") pullRequestListOptions

prCreateUsageInfo :: String
prCreateUsageInfo = usageInfo ("Usage: gwcli pullrequest create [OPTIONS]") pullRequestCreateOptions

-- Browse subcommand help
browseUsageInfo :: String
browseUsageInfo = usageInfo ("Usage: gwcli browse [OPTIONS] [page]") browseOptions -- Now uses the real browseOptions
