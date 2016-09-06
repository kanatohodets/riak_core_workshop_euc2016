-module(zap_core_service).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         join/2,
         say/2
        ]).

-ignore_xref([
              join/2,
              say/2
             ]).

%% Public API

join(_Room, _Client) ->
    io:fwrite("zap_core_service:join not yet implemented!\n"),
    ok.

say(_Room, _Msg) ->
    io:fwrite("zap_core_service:say not yet implemented!\n"),
    ok.
