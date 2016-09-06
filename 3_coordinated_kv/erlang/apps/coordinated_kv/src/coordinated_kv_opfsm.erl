-module(coordinated_kv_opfsm).
-behavior(gen_fsm).

%% API
-export([start_link/6, op/4]).

%% Callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).

%% States
-export([prepare/2, execute/2, waiting/2]).

-record(state, {req_id,
                from,
                n,
                w,
                op,
                key,
                accum,
                preflist,
                num_w = 0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReqID, From, Op, Key, N, W) ->
    gen_fsm:start_link(?MODULE, [ReqID, From, Op, Key, N, W], []).

op(Op, Key, N, W) ->
    ReqID = req_id(),
    coordinated_kv_opfsm_sup:start_op_fsm([ReqID, self(), Op, Key, N, W]),
    {ok, ReqID}.

%%%===================================================================
%%% States
%%%===================================================================

init([ReqID, From, Op, Key, N, W]) ->
    lager:info("FSM ~p (~p) spawning: ~p target vnodes (n), of which ~p must reply for success (r/w)",
               [ReqID, Op, N, W]),

    SD = #state{req_id=ReqID, from=From, n=N, w=W, op=Op, key=Key, accum=[]},
    {ok, prepare, SD, 0}.

%% @doc Prepare the write by calculating the _preference list_.
prepare(timeout, SD0=#state{op=Op, req_id=ReqID, n=N, key=Key}) ->
    lager:info("FSM ~p (~p) hashing ring key ~p and asking for preference list with ~p members",
               [ReqID, Op, Key, N]),

    DocIdx = riak_core_util:chash_key(Key),
    Preflist = riak_core_apl:get_apl(DocIdx, N, coordinated_kv_service),

    SD = SD0#state{preflist=Preflist},
    {next_state, execute, SD, 0}.

%% @doc Execute the write request and then go into waiting state to
%% verify it has meets consistency requirements.
execute(timeout, SD0=#state{req_id=ReqID, op=Op, preflist=Preflist}) ->
    lager:info("FSM ~p (~p) sending command to preflist members", [ReqID, Op]),

    Command = {ReqID, Op},
    riak_core_vnode_master:command(Preflist, Command, {fsm, undefined, self()},
                                   coordinated_kv_vnode_master),
    {next_state, waiting, SD0}.

%% @doc Wait for W write reqs to respond.
waiting({ReqID, Resp}, SD0=#state{op=Op, from=From, num_w=NumW0, w=W, accum=Accum}) ->
    NumW = NumW0 + 1,
    lager:info("FSM ~p (~p) received response number ~p of ~p", [ReqID, Op, NumW, W]),
    NewAccum = [Resp|Accum],
    SD = SD0#state{num_w=NumW, accum=NewAccum},
    if
        NumW =:= W ->
            From ! {ReqID, NewAccum},
            {stop, normal, SD};
        true -> {next_state, waiting, SD}
    end.

handle_info(Info, _StateName, StateData) ->
    lager:warning("got unexpected info ~p", [Info]),
    {stop,badmsg,StateData}.

handle_event(Event, _StateName, StateData) ->
    lager:warning("got unexpected event ~p", [Event]),
    {stop,badmsg,StateData}.

handle_sync_event(Event, _From, _StateName, StateData) ->
    lager:warning("got unexpected sync event ~p", [Event]),
    {stop,badmsg,StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    ok.

%% Private API

req_id() -> erlang:unique_integer().

