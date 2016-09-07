# Handoff
What if we want to grow or shrink our cluster? in that case, we need to be able
to reallocate vnodes to other locations permanently. This is what handoff does.

This is mostly a matter of serialization, with one interesting bit in the fold
request.

## Exercise
Check out the fold request code. Are the various `handoff_*` commands
implemented in a way that makes sense for the data in question?

Fire up a cluster with one node, store some data there, then add another node
to the cluster and watch handoff take place. While that's happening, run pings
or KV fetches to see which node handles them.

For example:

Before joining, on node A:

    > riak_core_console:member_status([]).
    > handoff_kv_service:store(foo, {bar, 42}, 3, 2).
    > handoff_kv_service:ping(3).
    %% look at which vnodes handle this command (they log)
  
Now, start node B, and join it to Node A:

    > riak_core:join('handoff_kv1@127.0.0.1').
    > riak_core_console:member_status([]).
    %% now, start running fetches to see who handles the commands:
    > handoff_kv_service:fetch(foo, 3, 1). %% repeat periodically as handoff progresses
