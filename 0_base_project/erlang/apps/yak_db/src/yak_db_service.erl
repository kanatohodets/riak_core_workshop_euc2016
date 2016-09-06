-module(yak_db_service).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0
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
    %% vnodes who can handle this index for the given service (the yak_db_service)
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, yak_db_service),
    [{IndexVnode, _Type}] = PrefList,
    %% ..then actually send the request to that vnode
    %% (riak core appends "_master" to yak_db_vnode for a process name)
    riak_core_vnode_master:sync_spawn_command(IndexVnode, ping, yak_db_vnode_master).
