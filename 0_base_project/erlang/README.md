# Building a cluster

Sketch of a `riak_core` application in Erlang.

Demo app for "Stateful Services with Riak Core". See README one level up
for details about the exercise.

To run:

    make devrel1
    make dev1-console
    erl> riak_core_console:member_status([]).

To build a cluster, compile and run another profile:

    make devrel2
    make dev2-console

Then, in the second console:

    erl>riak_core:join('yak_db1@127.0.0.1').
    erl>riak_core_console:member_status([]).

Now you have a cluster!

If you want to see transitions back, try:

    erl>riak_core:leave().
    
