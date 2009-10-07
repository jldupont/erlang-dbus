%%% -------------------------------------------------------------------
%%% Author  : jldupont
%%% Description :
%%%
%%% Created : 2009-10-07
%%% -------------------------------------------------------------------
-module('erlang-dbus-ping_pong').

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% DEFINES
%% --------------------------------------------------------------------
-define(EDBUS,         'erlang-dbus-client').
-define(PING_INTERVAL, 1*1000). %% in ms.

%% --------------------------------------------------------------------
%% External exports
-export([
		 start/0
		,start/1
		 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
				type
			   ,uname
			   ,st
			   ,counter
				}).

%%
%% ===================================================================== MANAGEMENT
%%

start() ->
	start(ping).

start([ping]) ->
	do_start(ping);

start([pong]) ->
	do_start(pong);

start(["ping"]) ->
	do_start(ping);

start(["pong"]) ->
	do_start(pong).

do_start(Server=Type) ->
	gen_server:start_link({local, Server}, ?MODULE, [{type, Type}], []),
	gen_server:cast(Server, start).


%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([{type, Type}]) ->
    {ok, #state{type=Type, st=start, counter=0}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(start, State) ->
	Type=State#state.type,
	io:format("Init for Type: ~p~n", [Type]),
	?EDBUS:init(),
    {noreply, State};


handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({edbus, {ready, UName}}, State) ->
	io:format("Unique Name: ~p~n", [UName]),
	Type=erlang:atom_to_list(State#state.type),
	?EDBUS:register_name("com.jldupont.edbus."++Type),
	case Type of
		"ping" ->
			timer:send_interval(?PING_INTERVAL, do_ping);
		"pong" ->
			?EDBUS:subscribe_signals(["com.jldupont.edbus"])
	end,
    {noreply, State#state{uname=UName, st=main} };


handle_info(do_ping, State) ->
	Counter=State#state.counter,
	io:format("Sending Ping, counter(~p) ", [Counter]),
	?EDBUS:send_signal({0, "com.jldupont.edbus.pong", "/com/jldupont/edbus", "com.jldupont.edbus", "ping", {ui32, Counter}}),
    {noreply, State#state{counter=Counter+1}};


handle_info({edbus, [s, _Serial, {_Sender}, {_Dest}, {_Path}, {"com.jldupont.edbus"}, {"ping"}, Message]}, State) ->
	io:format("Ping received: ~p~n", [Message]),
    {noreply, State};



handle_info(Info, State) ->
	io:format("Info: ~p~n", [Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% ================================================= Internal functions
%% --------------------------------------------------------------------

