# Changelog for gwcli-hs

## 0.10.0.0 -- 2026-01-09

* Added short command aliases for improved CLI usability:
  - Top-level: `a` (auth), `b` (browse), `i` (issue), `pr` (pullrequest), `v` (version)
  - Subcommands: `l` (list), `c` (create), `s` (show)
* Restored convenience of short commands from the old GetOpt-based parser

## 0.9.9.0 -- 2025-10-12

* Improved error messages when API calls fail with HTTP status codes and response details (#95)
* Fixed GHC optimization flag: corrected invalid -O4 to proper -O2 (#96)

## 0.9.8.0 -- 2025-10-11

* Refactored module naming: renamed NewCommandLineParser to CommandLineParser (#94)
* Removed old GetOpt-based command-line parser
* Code cleanup and formatting improvements in Main.hs
* Improved code maintainability after optparse-applicative migration

## 0.9.7.0 -- 2025-01-27

* Migrated command-line parsing to optparse-applicative with shell completions (#90)
* Updated Cabal version to 3.14

## 0.9.6.0 -- 2025-03-09

* Upgraded to GHC 9.12.1
* Fixed ambiguous 'show' function references for GHC 9.12.1 compatibility
