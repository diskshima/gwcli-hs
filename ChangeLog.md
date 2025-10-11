# Changelog for gwcli-hs

## Unreleased changes

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
