-module(kvapi_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
  n,
  key,
  method
}).

init(_, Req, _Opts) ->
    {Method, Req1} = method(Req),
    {Key, Req2} = cowboy_req:binding(key, Req1),
    {N, Req3} = cowboy_req:qs_val(<<"n">>, Req2),
    N1 = binary_to_int(N),
    io:fwrite("got an N val: ~p", [N1]),
    {ok, Req3, #state{method=Method, key=Key, n=N1}}.

%% TODO: handle fetch

handle(Req, State=#state{method=put, key=Key, n=N}) ->
    {W, Req1} = cowboy_req:qs_val(<<"w">>, Req),
    W1 = binary_to_int(W),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    Term = jsx:decode(Body),

    io:fwrite("storing ~p (~p) with N ~p and W ~p", [Key, Term, N, W1]),
    {ok, _Res} = handoff_kv_service:store(Key, Term, N, W1),
    Encoded = jsx:encode(#{outcome => <<"success">>}),

    {ok, Req3} = cowboy_req:reply(200, json_header(), Encoded, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

json_header() -> [{ <<"content-type">>, <<"application/json">>}].

method(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    AMethod = case Method of
                  <<"GET">> -> get;
                  <<"PUT">> -> put
              end,
	{AMethod, Req1}.

binary_to_int(String) ->
    L = binary_to_list(String),
    list_to_integer(L).
