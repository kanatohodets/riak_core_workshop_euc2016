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
