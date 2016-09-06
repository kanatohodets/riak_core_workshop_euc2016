# Building a cluster

Sketch of a `riak_core` application in Elixir.

First demo app for "Stateful Services with Riak Core". See README one level up
for details about the exercise.

To run:

    mix deps.get
    MIX_ENV=dev_a iex --name dev_a@127.0.0.1 -S mix
    iex> :riak_core.member_status([])

To build a cluster, start another member and join it to the cluster:

    # In another terminal
    MIX_ENV=dev_b iex --name dev_b@127.0.0.1 -S mix
    iex> :riak_core.join('dev_a@127.0.0.1')
    iex> :riak_core.member_status([])

Now you have a cluster!

If you want to see transitions back, try:

    iex> :riak_core.leave()
    
