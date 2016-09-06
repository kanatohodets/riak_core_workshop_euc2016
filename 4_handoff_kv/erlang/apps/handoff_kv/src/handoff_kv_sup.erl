-module(handoff_kv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { handoff_kv_vnode_master,
                {riak_core_vnode_master, start_link, [handoff_kv_vnode]},
                permanent, 5000, worker, [riak_core_vnode_master]},

    OpFSMs = { handoff_kv_opfsm_sup,
                {handoff_kv_opfsm_sup, start_link, []},
                permanent, infinity, supervisor, [handoff_kv_opfsm_sup]},

    { ok,
        { {one_for_one, 5, 10},
          [VMaster, OpFSMs]}}.
