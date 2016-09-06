# YakDB

Sketch of a `riak_core` application in Erlang.

Demo app for "Stateful Services with Riak Core". See README one level up
for details about the exercise.

To run:

    make devrel1
    make dev1-console
    erl>yak_db_service:ping().

If you want a cluster, compile and run another profile:

    make devrel2
    make dev2-console
    
Then join it to the cluster:

    erl>riak_core:join('yak_db1@127.0.0.1'). 
