# SimpleHash

Sketch of a `riak_core` application in Elixir.

First demo app for "Stateful Services with Riak Core". See README one level up
for details about the exercise.

To run:

    mix deps.get
    MIX_ENV=dev_a iex --name dev_a@127.0.0.1 -S mix

Potentially also a terminal for `dev_b`, and `dev_c` if you want a cluster, plus

    :riak_core.join('dev_b@127.0.0.1')
