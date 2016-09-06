# SimpleHash

Sketch of a `riak_core` application in Erlang.

First demo app for "Stateful Services with Riak Core". See README one level up
for details about the exercise.

To run:

    make release
    make console

If you want a cluster, start up the other nodes, and then ask this one to join
one of the others (where '$ip' is substituted for the real IP of that node):

    erl>riak_core:join('dev_b@127.0.0.1'). 
