# Plutus Script Re-Executor

A tool that follows a Cardano node, looks for user-specified script hashes and shadows them - executes user-provided scripts instead of the original ones. This enables running debugging versions of Plutus Core scripts - with traces - against the live ledger state provided by the node, enabling detailed on-chain monitoring. 

## Motivation 

Developers of a decentralized application (dApp) would ideally have the option to easily analyze the successful executions of their scripts on the Cardano chain. To reduce load on the network, the fee formula incentivizes script authors to minimize the script’s logic and the size of the state it maintains. Unfortunately, this means state that does not influence the script’s validity judgment but is otherwise useful to the dapp developer (e.g. an individual user’s history of usage, statistics about general usage, etc) is not automatically available on-chain. Dapp developers must use auxiliary tooling in order to maintain that state off-chain.

Plutus Script Re-Executor (PSR) is a monitoring tool designed to allow developers to run alternative Plutus scripts - for example, with tracing enabled - on real transactions from the Cardano blockchain locally, without paying extra fees. It does this by connecting to a cardano-node to monitor new blocks, and then when transactions using specified scripts are found, it will substitute one (or more) user provided scripts in place of the real script. This means users can use unoptimised scripts with tracing enabled to see intermediate values, or test alternative implementations against known buggy transactions to ensure bug have been fixed.

PSR also provides an option to store all data needed to re-execute transactions of interest from the past against new scripts, allowing developers to iterate application development and then go back and test after the fact.

To ensure that all relevant transactions are observed, PSR is able to control a cardano-node in a "leashed" mode - the cardano node will not proceed fetching new blocks until each has been processed by PSR. This means that every single execution of all user specified scripts can be stored for later use.

## How to Run

### Get a binary:

With [flake-enabled](https://nixos.wiki/wiki/Flakes) Nix, run Plutus Script Re-Executor directly

```bash
nix run github:tweag/plutus-script-reexecutor
```

Please note, that [our binary cache](http://plutus-script-reexecutor.cachix.org) provides only `aarch64-macos` packages, meaning you might need to build a lot of packages yourself.

You can install also install the binary in your profile with 

```bash
nix profile install github:tweag/plutus-script-reexecutor
```

The `plutus-script-reexecutor` command is then in your `$PATH` and is available anywhere.

Or you can build it directly, see [development](#Development) section.

### Usage 

The re-executor takes a configuration file with scripts to shadow:

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

By default, there is no logging or any recording to storage. If you want to write the events to file, use `--logs-path events.log` and explore them with

```bash
tail -f events.log
# Or, if you have jq installed
tail -f events.log | jq
```

or enable SQLite storage with `--sqlite-path plutus-script-reexecutor.db` which will enable API endpoints to explore historical data.

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

