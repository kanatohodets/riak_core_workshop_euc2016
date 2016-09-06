-module(silly_kv_service).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         store/2,
         fetch/1
        ]).

-ignore_xref([
              ping/0
             ]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    %% hash the current time (<<"ping">> is our bucket type, artifact of riak)
    %% to get a "document index"
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),
    %% use that hash (document index) to get a 1-item "preference list" -- a list of
    %% vnodes who can handle this index for the given service (the silly_kv_service)
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, silly_kv_service),
    [{IndexVnode, _Type}] = PrefList,
    %% ..then actually send the request to that vnode
    %% (riak core appends "_master" to silly_kv_vnode for a process name)
    riak_core_vnode_master:sync_spawn_command(IndexVnode, ping, silly_kv_vnode_master).

%% @doc Store some erlang terms into the ring under a key
store(Key, Data) ->
    DocIdx = riak_core_util:chash_key({<<"store">>, term_to_binary(Key)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, silly_kv_service),
    [{IndexVnode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexVnode, {store, Key, Data}, silly_kv_vnode_master).

%% @doc Fetch some previously stored erlang terms from the ring
fetch(Key) ->
    DocIdx = riak_core_util:chash_key({<<"store">>, term_to_binary(Key)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, silly_kv_service),
    [{IndexVnode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexVnode, {fetch, Key}, silly_kv_vnode_master).
