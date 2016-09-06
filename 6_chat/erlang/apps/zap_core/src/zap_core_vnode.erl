-module(zap_core_vnode).
-behaviour(riak_core_vnode).

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([
             start_vnode/1
             ]).

-record(state, {partition, db}).

-include_lib("riak_core/include/riak_core_vnode.hrl").

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    EtsHandle = ets:new(nil, [bag]),
    {ok, #state { partition=Partition, db=EtsHandle }}.

handle_command({ReqID, ping}, _Sender, State=#state{partition=Partition}) ->
    {reply, {ReqID, {pong, Partition}}, State};

%% TODO: join a room
%% TODO: broadcast

handle_handoff_command(?FOLD_REQ{foldfun=VisitFun, acc0=Acc0}, _Sender, State=#state{db=Db, partition=Partition}) ->
    lager:info("~p received handoff command for vnode ~p", [node(), Partition]),
    FoldFun = fun(Object, AccIn) ->
        AccOut = VisitFun({unused_bucket_name, Object}, Object, AccIn),
        AccOut
    end,
    Final = ets:foldl(FoldFun, Acc0, Db),
    {reply, Final, State}.

handoff_starting(TargetNode, State=#state{partition=Partition}) ->
    lager:info("~p starting handoff of ~p to ~p", [node(), Partition, TargetNode]),
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(TargetNode, State=#state{partition=Partition}) ->
    lager:info("~p has finished transferring vnode ~p to ~p", [node(), Partition, TargetNode]),
    {ok, State}.

handle_handoff_data(BinData, State=#state{db=Db}) ->
    {_Bucket, {Key, Value}} = binary_to_term(BinData),
    ets:insert(Db, {Key, Value}),
    {reply, ok, State}.

encode_handoff_item(ObjectName, ObjectValue) ->
    term_to_binary({ObjectName, ObjectValue}).

is_empty(State=#state{db=Db}) ->
    {'$end_of_table' =:= ets:first(Db), State}.

delete(State=#state{db=Db, partition=Partition}) ->
    lager:info("deleting vnode ~p", [Partition]),
    ets:delete(Db),
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
