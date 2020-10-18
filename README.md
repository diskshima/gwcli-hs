# gwcli-hs

A command line tool for Git repository services like:
- GitHub
- BitBucket (not yet supported)
- ZenHub (not yet supported)

## Setup
1. Install `stack`
    ```bash
     brew install stack  # macOS + brew
        or
     curl -sSL https://get.haskellstack.org/ | sh
    ```

## Build
```bash
stack build
```

## Run

### GitHub

1. Create a credentials file named `.gwcli.yaml` in your home directory.
    ```yaml
    github: GITHUB_PERSONAL_TOKEN
    ```
1. Running it via `stack exec`
    ```bash
    gwcli
    ```

### Bitbucket

TBD...

## haskell-language-server Support

`haskell-language-server` requires `hie.yaml` to be up-to-date to work, we recommend installing [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) and updating it from the stack configuration.

### Installation

```bash
stack install implicit-hie
```

### Running

```bash
gen-hie > hie.yaml
```
