-module(handoff_kv_opfsm_sup).
-behavior(supervisor).

-export([start_op_fsm/1,
         start_link/0]).
-export([init/1]).

start_op_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    OpFSM = {undefined,
                {handoff_kv_opfsm, start_link, []},
                temporary, 5000, worker, [handoff_kv_opfsm]},

    {ok, {{simple_one_for_one, 10, 10}, [OpFSM]}}.
