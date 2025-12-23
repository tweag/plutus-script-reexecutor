# Plutus Script Re-Executor

A tool that follows a Cardano node, looks for user-specified script hashes and executes user-provided scripts instead of the original ones. This enables running debugging versions of Plutus Core scripts - with traces - against the live ledger state provided by the node, simplifying script development and debugging.

## Motivation 

Plutus scripts can be difficult to debug and test, particularly against real world transactions. Generating inputs that are identical to what the Cardano nodes will pass to the script can be difficult, and because scripts are optimised to save execution costs, they usually have all tracing removed when used in production. This means that scripts which behave unexpectedly in the real world are often a black box when trying to determine what went wrong.

Plutus Script Re-Executor (PSR) is a tool designed to allow developers to test alternative versions of Plutus scripts on real transactions from the Cardano blockchain. It does this be connecting to a cardano-node to monitor for new blocks, and then when transactions using matched scripts are found, it will substitute one (or more) scripts the user has provided in place of the real script. This means users can use unoptimised scripts with tracing enabled to see intermediate values, or test alternative implementations against known buggy transactions to ensure bug have been fixed.

PSR also stores all data needed to re-execute transactions of interest from the past against new scripts, allowing developers to iterate application development and then go back and test after the fact.

To ensure that all relevant transactions are observed, PSR is able to control a cardano-node in a "leashed" mode - the cardano node will not proceed fetching new blocks until each has been processed by PSR. This means that every single execution of all user specified scripts can be stored for later use.

## How to Run

### Get a binary:

With [flake-enabled](https://nixos.wiki/wiki/Flakes) Nix, run Plutus Script Re-Executor directly

```bash
nix run github:tweag/plutus-reexec
```

Please note, that [our binary cache](http://plutus-script-reexecutor.cachix.org) provides only `aarch64-macos` packages, meaning you might need to build a lot of packages yourself.

You can install also install the binary in your profile with 

```bash
nix profile install github:tweag/plutus-reexec
```

The `plutus-script-reexecutor` command is then in your `$PATH` and is available anywhere.

Or you can build it directly, see [development](#Development) section.

### Usage 

The re-executor takes a configuration file with scripts to substitute:

```bash
plutus-script-reexecutor --script-yaml scripts.yaml
```

The example file can be generated with

```bash
plutus-script-reexecutor generate-scripts-example > scripts.yaml
```

Please note that `plutus-script-reexecutor` uses `CARDANO_NODE_SOCKET_PATH` and `CARDANO_NODE_NETWORK_ID` environment variables by default, which can be overriden with cli arguments `--node-socket PATH (--mainnet | --testnet TESTNET_MAGIC)`.

After starting the `plutus-script-reexecutor` you can look for events with

```bash
websocat ws://localhost:8080/events-ws | jq
```

or

```bash
curl -v http://localhost:8080/events | jq
``` 

## Documentation

See the [docs](./docs) directory for detailed documentation.

## Development 

To build and run the project manually:

```bash
nix develop
cabal build
cabal run plutus-script-reexecutor
```

We use `process-compose` to run the local cluster with cardano-node, testing scripts and re-executor. Use `./dev-local/main` to start it.

