-module(coordinated_kv_service).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/1,
         store/4,
         fetch/3
        ]).

-ignore_xref([
              ping/1,
              store/4,
              fetch/3
             ]).

%% Public API
ping(N) -> todo.

store(Key, Data, N, W) when N >= W -> todo.

fetch(Key, N, W) when N >= W -> todo.
