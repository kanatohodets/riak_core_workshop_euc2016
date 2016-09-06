-module(coordinated_kv_service).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/1,
         store/4,
         fetch/3
        ]).

-ignore_xref([
              ping/1,
              store/4,
              fetch/3
             ]).

%% Public API
ping(N) ->
	W = N,
	RingKey = {<<"ping">>, term_to_binary(os:timestamp())},
    {ok, ReqID} = coordinated_kv_opfsm:op(ping, RingKey, N, W),
    wait_for_reqid(ReqID, 5000).

store(Key, Data, N, W) when N >= W ->
    RingKey = {<<"store">>, term_to_binary(Key)},
    {ok, ReqID} = coordinated_kv_opfsm:op({store, Key, Data}, RingKey, N, W),
    wait_for_reqid(ReqID, 5000).

fetch(Key, N, W) when N >= W ->
    RingKey = {<<"store">>, term_to_binary(Key)},
    {ok, ReqID} = coordinated_kv_opfsm:op({fetch, Key}, RingKey, N, W),
    wait_for_reqid(ReqID, 5000).

wait_for_reqid(ReqID, Timeout) ->
    receive
        {ReqID, Value} ->
            {ok, Value}
    after Timeout ->
        {error, timeout}
    end.
