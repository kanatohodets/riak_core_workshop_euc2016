-module(zap_core_service).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/1,
         join/2,
         say/2
        ]).

-ignore_xref([
              ping/1,
              join/2,
              say/2
             ]).

%% Public API
ping(N) ->
	W = N,
	RingKey = {<<"ping">>, term_to_binary(os:timestamp())},
    {ok, ReqID} = zap_core_opfsm:op(ping, RingKey, N, W),
    wait_for_reqid(ReqID, 5000).

join(Room, Client) ->
    RingKey = {<<"chat">>, Room},
    {ok, ReqID} = zap_core_opfsm:op({join, Room, Client}, RingKey, 2, 1),
    {ok, _Res} = wait_for_reqid(ReqID, 5000),
    ok.

say(Room, Msg) ->
    RingKey = {<<"chat">>, Room},
    {ok, ReqID} = zap_core_opfsm:op({say, Room, Msg}, RingKey, 1, 1),
    {ok, _Res} = wait_for_reqid(ReqID, 5000),
    ok.

wait_for_reqid(ReqID, Timeout) ->
    receive
        {ReqID, Value} ->
            {ok, Value}
    after Timeout ->
        {error, timeout}
    end.
