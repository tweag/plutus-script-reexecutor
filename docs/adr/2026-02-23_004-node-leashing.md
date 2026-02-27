---
slug: 4
title: | 
  4. Node Leashing
authors: [tweag-ev-ak]
tags: [Accepted]
---

## Status

Accepted

## Context

The re-executor can be too slow to follow the node's ledger state. It should be able to leash the node in such a way that the node won't proceed until the re-executor has caught up. The specification describes three strategies for leashing implementation.

1. Pausing the `BlockFetch` client. 
2. Removal of the connected peers.
3. Use the LoE mechanism.

## Decision

We decided that the first two strategies are too invasive and the third strategy described in the specification seems to be the most viable and correct one, increasing our chances to get it accepted by the cardano-node developers, since we are utilising the already existing and well-tested mechanism.

## Consequences

We get leashing working and have more chances to get it merged to the upstream cardano-node implementation.
