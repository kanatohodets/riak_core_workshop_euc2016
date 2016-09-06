Base project for EUC 2016 Riak Core workshop

# Building a Riak Core Cluster

A riak core cluster member has several distinct modes. Transitions between
these modes are performed by humans, just like in normal Riak. The goal of this
round is to just get a few Riak Core nodes connected to each other.

## Exercise

Start up at least two VMs with Riak Core running. Join them together. Once
joined, use `riak_core_console:member_status([])` to verify that the ring is
transferring.

See the language-specific README for commands to run.

## Background
### Forever alone
When we first turn on the cluster, the cluster members are isolated -- they're
each in clusters of size 1.

This is fine for playing around, and for quick development, but the whole point
of Riak Core is to help us manage clusters, so using it on a single node
doesn't help very much.

### Finding friends

`riak_core join/1` is the key to forming a cluster. If you've already started
your first node, start a second one and run `riak_core:join('friend@<ip>').`

This will cluster the nodes both in the Distributed Erlang sense, and in the
Riak sense. This is worth keeping in mind -- Riak clusters are just Distributed
Erlang clusters at heart: they have no magic that lets them scale beyond the
~40-50 node cluster size limit that is typically in effect for Distributed
Erlang.

### Leaving the party
`riak_core leave/1` will cause a node to leave the cluster and exit.

This can be helpful for testing handoff code.

### The cluster is persistent

The ring data is stored on disk, so you don't need to re-join the cluster
members to each other every time you start them up.
