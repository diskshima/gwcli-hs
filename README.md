# gwcli-hs

A command line tool for Git repository services like:
- GitHub
- Bitbucket

## Using gwcli-hs

1. Copy the `dot_gwcli.yaml.example` to your home directory and rename it `.gwcli.yaml`.
    ```bash
    cp dot_gwcli.yaml.example ~/gwcli.yaml
    ```
1. (For GitHub) [Obtain a Personal Token](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token) and add it to the `github` entry in the above YAML file.
    ```yaml
    github: GITHUB_PERSONAL_TOKEN
    ```
1. (For Bitbucket) Run `gwcli auth` in your repository. It will run an OAuth2 flow and open your browser. Login to Bitbucket.
    ```bash
    gwcli auth
    ```

You're good to go :smile:


## Developing gwcli-hs

### Setup

1. Install LLVM.
    - macOS
        ```sh
        brew install llvm
         # Make sure the brew installed version of llvm is used.
        PATH="$(brew --prefix)/opt/llvm/bin:$PATH"
        ```

### Build

1. Run Cabal
    ```sh
    cabal build
    ```
