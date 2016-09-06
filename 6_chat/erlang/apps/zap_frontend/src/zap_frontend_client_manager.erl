-module(zap_frontend_client_manager).

-behaviour(supervisor).

%% API
-export([start_link/0, handle/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

handle(Client) ->
    supervisor:start_child(?MODULE, [Client]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    Client = { client,
                {zap_frontend_client, start_link, []},
                temporary, 5000, worker, [zap_frontend_client]},

    { ok,
        { {simple_one_for_one, 5, 10},
          [Client]}}.
