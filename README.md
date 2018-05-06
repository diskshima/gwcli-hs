# gwcli-hs

A command line tool for Git sites (GitHub, ZenHub).

## Setup
1. Install `stack`
    ```bash
    $ brew install stack  # macOS + brew
        or
    $ curl -sSL https://get.haskellstack.org/ | sh
    ```
## Build
```bash
stack build
```

## Run
1. Create a credentials file named `.gwcli.yaml` in your home directory.
    ```yaml
    zenhub: ZENHUB_API_KEY
    github: GITHUB_PERSONAL_TOKEN
    ```
1. Running it via `stack exec`
    ```bash
    stack exec gwcli
    ```
