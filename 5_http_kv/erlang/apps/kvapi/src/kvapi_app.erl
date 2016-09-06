%%%-------------------------------------------------------------------
%% @doc kvapi public API
%% @end
%%%-------------------------------------------------------------------

-module(kvapi_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/api/store/:key", kvapi_handler, []}]}
    ]),
    cowboy:start_http(kvapi_http_listener, 100, [{port, 4000}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    kvapi_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
