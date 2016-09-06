# Coordinated reads/writes

So far we have a fairly terrible service: a single node failure will result in
permanently lost data. In order to avoid that, we need to write our data to
more places. In terms of Riak Core, "multiple places" means "multiple vnodes".

## Does that help protect from a hardware failure?
"basically yes" -- Riak Core takes care to put sequential vnodes on different
physical nodes when possible, so writing a piece of data to 2+ vnodes results
in fault tolerance (at the expense of cluster capacity). Reading from multiple
sources allows us to tolerate node failures on read as well.

## How does it work?
Coordinated reads/writes are implemented using a simple finite state machine.
This is not shipped with Riak Core, but it is a fairly straightforward pattern
that can be found in Riak and other users of Riak Core. The service API, rather
than hashing and fetching a preflist directly, spawns a request to be handled
by the operation FSM.

The Op FSM gets a preflist of N members long and send the command to all of them, waiting
or W or R to respond.

## Exercise

Study the op FSM and the vnode, then implement the Service code to use these coordinators.

If you're building up YakDB, bring the FSM code into YakDB.

See git tag coordinated-kv-complete if you'd like a reference implementation.
