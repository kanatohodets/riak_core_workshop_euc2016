# Handoff
What if we want to grow or shrink our cluster? in that case, we need to be able
to reallocate vnodes to other locations permanently. This is what handoff does.

This is mostly a matter of serialization, with one interesting bit: the fold
request.
