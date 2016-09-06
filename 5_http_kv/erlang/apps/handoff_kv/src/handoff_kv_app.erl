-module(handoff_kv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case handoff_kv_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, handoff_kv_vnode}]),
            ok = riak_core_node_watcher:service_up(handoff_kv_service, self()),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
