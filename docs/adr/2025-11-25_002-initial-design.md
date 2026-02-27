---
slug: 2
title: | 
  2. Initial design 
authors: [tweag-ev-ak]
tags: [Accepted]
---

## Status

Accepted

## Context

- We are starting to work on Plutus Script Re-Executor (PSR) proposed in [the proposal](../proposal.md).

- One of the proposal's deliverables is a PSR's design and its interface to the node. 

## Decision

- This ADR describes our initial steps in implementing the PSR and proposes [the following specification](../specification.md) for implementation. 

- The specification should contain the up-to-date state and will be updated with each ADR.

- The PSR will be a multi-thread Haskell application that connects to the cardano-node, queries blocks, and runs the specified scripts.

- We are going to start with a basic chain-indexer approach inspiring by https://github.com/IntersectMBO/plutus-script-evaluation.

- There are several components that might be done in parallel:

    - cardano-node "leash" mode connection support

    - PlutusCore executor in PSR (indexes, building of ScriptContext, etc)

    - Configuration map support

    - Events emittance support

- At this point the PSR's interace to the node for us is a ChainSync mini-protocol which we are planning to replace later.

## Consequences

- A basic chain-indexer will allow us to start with a small core and to iterate and improve each component independently later.

- Separation on compontents will allow us to work in different directions simultaneously as a team.

- The "leash" mechanism will be covered in a separate ADR as a standalone component. 

