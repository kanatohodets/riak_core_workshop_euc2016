-module(zap_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case zap_core_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, zap_core_vnode}]),
            ok = riak_core_node_watcher:service_up(zap_core_service, self()),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
