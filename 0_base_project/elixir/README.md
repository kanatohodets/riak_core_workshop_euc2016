# YakDB

Sketch of a `riak_core` application in Elixir.

First demo app for "Stateful Services with Riak Core". See README one level up
for details about the exercise.

To run:

    mix deps.get
    MIX_ENV=dev_a iex --name dev_a@127.0.0.1 -S mix

In another terminal


    MIX_ENV=dev_b iex --name dev_b@127.0.0.1 -S mix
    iex> :riak_core.join('dev_a@127.0.0.1')
    iex> YakDB.Service.ping()

Now you have a cluster!
