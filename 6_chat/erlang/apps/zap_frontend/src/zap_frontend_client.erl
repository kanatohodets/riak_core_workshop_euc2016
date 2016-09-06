-module(zap_frontend_client).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {name, sock}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    inet:setopts(Socket, [{active, true}]),
    respond(Socket, "connected! 'set-name <name>' and 'join <chat name>' to get started!"),
    {ok, #state{name=undefined, sock=Socket}}.

handle_info({tcp, _Socket, Msg}, State) ->
    Msg1 = binary:bin_to_list(Msg),
    Stripped = string:strip(Msg1, right, $\n),
    Pieces = string:tokens(Stripped, " "),
    command(Pieces, State);
handle_info({tcp_closed, _port}, State) ->
    {stop, {shutdown, tcp_closed}, State};
handle_info(Msg, #state{sock=Socket}=State) ->
    respond(Socket, Msg),
    {noreply, State}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _S) -> ok.

%% private

command(["ping", _N], #state{sock=Socket}=State) ->
    Ping = zap_core_service:ping(2),
    respond(Socket, io_lib:format("~p\n", [Ping])),
    {noreply, State};

command(["set-name", Name], #state{sock=Socket}=State) ->
    respond(Socket, io_lib:format("set-name to ~p", [Name])),
    {noreply, State#state{name=Name}};

command(_Command, #state{name=undefined, sock=Socket}=State) ->
    respond(Socket, "must set a name before joining/chatting"),
    {noreply, State};

command(["say", Room | MsgPieces], #state{name=Name}=State) ->
    Msg = string:join(MsgPieces, " "),
    Msg1 = io_lib:format("~p: ~p", [Name, Msg]),
    ok = zap_core_service:say(Room, Msg1),
    {noreply, State};

command(["join", Room], #state{sock=Socket}=State) ->
    ok = zap_core_service:join(Room, self()),
    respond(Socket, io_lib:format("joined ~p!", [Room])),
    {noreply, State};

command([], #state{sock=Socket}=State) ->
    respond(Socket, ""),
    {noreply, State};

command(Msg, #state{sock=Socket}=State)->
    respond(Socket, io_lib:format("sorry, I don't understand '~p'", [Msg])),
    {noreply, State}.

respond(Socket, Msg) ->
    gen_tcp:send(Socket, Msg++"\n"),
    gen_tcp:send(Socket, "> ").
