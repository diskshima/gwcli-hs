module CommandLineParser (
    Command(..),
    AuthOptions(..),
    BrowseOptionsCli(..),
    IssueCommand(..),
    PullRequestCommand(..),
    VersionOptions(..),
    GlobalOptions(..),
    CliArguments(..),
    parseCliArgs,
    IssueListOptionsCli(..),
    IssueCreateOptionsCli(..),
    IssueShowOptions(..),
    -- Export new types for PullRequest subcommand
    PullRequestListOptionsCli(..),
    PullRequestCreateOptionsCli(..),
    PullRequestShowOptions(..)
) where

import Options.Applicative

-- Global options
data GlobalOptions = GlobalOptions
    { optVerbose :: Bool
    } deriving (Show, Eq)

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = GlobalOptions
    <$> switch
        ( long "verbose"
       <> short 'v'
       <> help "Enable verbose output" )

-- Define option types for each command
data AuthOptions = AuthOptions deriving (Show, Eq)
data BrowseOptionsCli = BrowseOptionsCli
    { boUrl :: Maybe String
    , boPrint :: Bool
    } deriving (Show, Eq)

-- IssueCommand and its options
data IssueShowOptions = IssueShowOptions
    { isoIssueNumber :: String
    } deriving (Show, Eq)

data IssueListOptionsCli = IssueListOptionsCli
    { iloAll :: Bool
    } deriving (Show, Eq)

data IssueCreateOptionsCli = IssueCreateOptionsCli
    { icoTitle :: String
    , icoBody  :: String
    } deriving (Show, Eq)

data IssueCommand
    = IssueList IssueListOptionsCli
    | IssueCreate IssueCreateOptionsCli
    | IssueShow IssueShowOptions
    deriving (Show, Eq)

-- PullRequestCommand and its options
data PullRequestShowOptions = PullRequestShowOptions
    { prsoPrNumber :: String
    } deriving (Show, Eq)

data PullRequestListOptionsCli = PullRequestListOptionsCli
    { prloAll :: Bool
    } deriving (Show, Eq)

data PullRequestCreateOptionsCli = PullRequestCreateOptionsCli
    { prcoTitle :: String
    , prcoBody  :: String
    , prcoBase  :: String  -- Base branch
    } deriving (Show, Eq)

data PullRequestCommand
    = PullRequestList PullRequestListOptionsCli
    | PullRequestCreate PullRequestCreateOptionsCli
    | PullRequestShow PullRequestShowOptions
    deriving (Show, Eq)

data VersionOptions = VersionOptions deriving (Show, Eq)


-- Top-level command sum type
data Command
    = AuthCmd AuthOptions
    | BrowseCmd BrowseOptionsCli
    | IssueCmd IssueCommand
    | PullRequestCmd PullRequestCommand -- Updated
    | VersionCmd VersionOptions
    deriving (Show, Eq)

-- Parsers for individual command options (auth, browse, version, issue actions already exist)
authOptionsParser :: Parser AuthOptions
authOptionsParser = pure AuthOptions

browseOptionsCliParser :: Parser BrowseOptionsCli
browseOptionsCliParser = BrowseOptionsCli
    <$> optional (argument str (metavar "PAGE" <> help "Page identifier (e.g., issue/PR number, specific path like 'pulls')"))
    <*> switch
        ( long "print"
       <> short 'p'
       <> help "Only print the URL, don't open in browser" )

versionOptionsParser :: Parser VersionOptions
versionOptionsParser = pure VersionOptions

-- Parsers for issue actions
issueShowOptionsParser :: Parser IssueShowOptions
issueShowOptionsParser = IssueShowOptions
    <$> argument str (metavar "ISSUE_NUMBER" <> help "Issue number to show")

issueListOptionsParser :: Parser IssueListOptionsCli
issueListOptionsParser = IssueListOptionsCli
    <$> switch
        ( long "all"
       <> short 'a'
       <> help "Show all issues (including closed/resolved)" )

issueCreateOptionsParser :: Parser IssueCreateOptionsCli
issueCreateOptionsParser = IssueCreateOptionsCli
    <$> strOption
        ( long "title"
       <> short 't'
       <> metavar "TITLE"
       <> help "Title of the issue" )
    <*> strOption
        ( long "message"
       <> short 'm'
       <> metavar "BODY"
       <> help "Body/description of the issue" )

-- Parser for the issue subcommand and its actions
issueCommandParser :: Parser IssueCommand
issueCommandParser = subparser
    ( command "list" (info (IssueList <$> issueListOptionsParser <**> helper)
        (progDesc "List issues. Use --all to show all issues."))
   <> command "l" (info (IssueList <$> issueListOptionsParser <**> helper)
        (progDesc "List issues (short form)"))
   <> command "create" (info (IssueCreate <$> issueCreateOptionsParser <**> helper)
        (progDesc "Create a new issue."))
   <> command "c" (info (IssueCreate <$> issueCreateOptionsParser <**> helper)
        (progDesc "Create a new issue (short form)"))
   <> command "show" (info (IssueShow <$> issueShowOptionsParser <**> helper)
        (progDesc "Show a specific issue by its number."))
   <> command "s" (info (IssueShow <$> issueShowOptionsParser <**> helper)
        (progDesc "Show a specific issue (short form)"))
    )

-- Parsers for pull request actions
pullRequestShowOptionsParser :: Parser PullRequestShowOptions
pullRequestShowOptionsParser = PullRequestShowOptions
    <$> argument str (metavar "PR_NUMBER" <> help "Pull request number to show")

pullRequestListOptionsParser :: Parser PullRequestListOptionsCli
pullRequestListOptionsParser = PullRequestListOptionsCli
    <$> switch
        ( long "all"
       <> short 'a'
       <> help "Show all pull requests (including merged/closed)" )

pullRequestCreateOptionsParser :: Parser PullRequestCreateOptionsCli
pullRequestCreateOptionsParser = PullRequestCreateOptionsCli
    <$> strOption
        ( long "title"
       <> short 't'
       <> metavar "TITLE"
       <> help "Title of the pull request" )
    <*> strOption
        ( long "message" -- Or "body"
       <> short 'm' -- Or 'd' for description
       <> metavar "BODY"
       <> help "Body/description of the pull request" )
    <*> strOption
        ( long "base"
       <> short 'b'
       <> metavar "TARGET_BRANCH"
       <> help "Base (target) branch for the pull request" )

-- Parser for the pullrequest subcommand and its actions
pullRequestCommandParser :: Parser PullRequestCommand
pullRequestCommandParser = subparser
    ( command "list" (info (PullRequestList <$> pullRequestListOptionsParser <**> helper)
        (progDesc "List pull requests. Use --all to show all PRs."))
   <> command "l" (info (PullRequestList <$> pullRequestListOptionsParser <**> helper)
        (progDesc "List pull requests (short form)"))
   <> command "create" (info (PullRequestCreate <$> pullRequestCreateOptionsParser <**> helper)
        (progDesc "Create a new pull request."))
   <> command "c" (info (PullRequestCreate <$> pullRequestCreateOptionsParser <**> helper)
        (progDesc "Create a new pull request (short form)"))
   <> command "show" (info (PullRequestShow <$> pullRequestShowOptionsParser <**> helper)
        (progDesc "Show a specific pull request by its number."))
   <> command "s" (info (PullRequestShow <$> pullRequestShowOptionsParser <**> helper)
        (progDesc "Show a specific pull request (short form)"))
    )

-- Combined command parser using subcommands
commandParser :: Parser Command
commandParser = subparser
    ( command "auth" (info (AuthCmd <$> authOptionsParser <**> helper) (progDesc "Authenticate with services"))
   <> command "a" (info (AuthCmd <$> authOptionsParser <**> helper) (progDesc "Authenticate with services (short form)"))
   <> command "browse" (info (BrowseCmd <$> browseOptionsCliParser <**> helper) (progDesc "Open repository page in browser"))
   <> command "b" (info (BrowseCmd <$> browseOptionsCliParser <**> helper) (progDesc "Open repository page in browser (short form)"))
   <> command "issue" (info (IssueCmd <$> issueCommandParser) (progDesc "Manage issues (list, create, show)"))
   <> command "i" (info (IssueCmd <$> issueCommandParser) (progDesc "Manage issues (short form)"))
   <> command "pullrequest" (info (PullRequestCmd <$> pullRequestCommandParser) (progDesc "Manage pull requests (list, create, show)"))
   <> command "pr" (info (PullRequestCmd <$> pullRequestCommandParser) (progDesc "Manage pull requests (short form)"))
   <> command "version" (info (VersionCmd <$> versionOptionsParser <**> helper) (progDesc "Show version (same as --version flag)"))
   <> command "v" (info (VersionCmd <$> versionOptionsParser <**> helper) (progDesc "Show version (short form)"))
    )

-- Top-level parser for all arguments (GlobalOptions + Command)
data CliArguments = CliArguments GlobalOptions Command deriving (Show, Eq)

cliArgumentsParser :: Parser CliArguments
cliArgumentsParser = CliArguments <$> globalOptionsParser <*> commandParser

-- Version option for --version flag (version string provided externally)
versionOption :: String -> Parser (a -> a)
versionOption versionStr = infoOption versionStr
    ( long "version"
   <> short 'V' -- Different from verbose 'v'
   <> help "Show version information"
   <> hidden ) -- Hidden because we also have a 'version' subcommand

-- ParserInfo for the entire application
optsParserInfo :: String -> ParserInfo CliArguments
optsParserInfo versionStr = info (cliArgumentsParser <**> helper <**> versionOption versionStr)
    ( fullDesc
   <> progDesc (
        "CLI tool for interacting with Git services. " ++
        "Try 'gwcli <command> --help' for command-specific help.\n\n" ++ -- Added newline for better formatting
        "To generate completion scripts (redirect output to a file):\n" ++
        "  gwcli --bash-completion-script gwcli\n" ++
        "  gwcli --fish-completion-script gwcli\n" ++
        "  gwcli --zsh-completion-script gwcli"
        )
   <> header "gwcli - Git Workflow CLI" )

-- Main function to be called from Main.hs
parseCliArgs :: String -> IO CliArguments
parseCliArgs versionStr = execParser (optsParserInfo versionStr)
