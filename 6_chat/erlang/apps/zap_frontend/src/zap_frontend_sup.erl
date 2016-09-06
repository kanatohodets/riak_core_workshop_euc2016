%%%-------------------------------------------------------------------
%% @doc zap_frontend top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(zap_frontend_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, accept/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ClientManager = { zap_frontend_client_manager, {zap_frontend_client_manager,
                                              start_link, []},
                      permanent, 5000, supervisor, dynamic},

    {ok, Port} = application:get_env(chat_port),
    %% apologies, didn't want to fiddle with proc_lib. should be stable (famous last words...)
    spawn(?MODULE, accept, [Port]),
    
    {ok, { {one_for_one, 5, 10}, [ClientManager]} }.

%%====================================================================
%% Internal functions
%%====================================================================

accept(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [binary, {packet, line}, {active,
                                                                   false},
                                          {reuseaddr, true}]),
    io:fwrite("zap frontend now listening on ~p\n", [Port]),
    loop_acceptor(Socket).

loop_acceptor(Socket) ->
    {ok, Client} = gen_tcp:accept(Socket),
    {ok, Pid} = zap_frontend_client_manager:handle(Client),
    ok = gen_tcp:controlling_process(Client, Pid),
    loop_acceptor(Socket).
