%%% -------------------------------------------------------------------
%%% Author  : jldupont
%%% Description :
%%%
%%% Created : 2009-10-06
%%% -------------------------------------------------------------------
-module('erlang-dbus-client-server').

-behaviour(gen_server).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {drvpath 
				,drvport
			    ,debug
			   }).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([{drv, Drv}, {debug, Debug}]) ->
	io:format("server: drv: ~p", [Drv]),
    {ok, #state{drvpath=Drv, debug=Debug}}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_cast(stop, State) ->
	dmsg(State, "! Stopping"),
    {stop, normal, State};


handle_cast(Msg, State) when State#state.drvport == undefined ->
	dmsg(State, "> Starting Port"),
	Drv=State#state.drvpath,
	Port = open_port({spawn, Drv++" type=\'signal\' type=\'method_return\' type=\'error\'"}, [{packet, 4}, binary, exit_status]),
    {noreply, State#state{drvport=Port}};


handle_cast(Msg, State) when State#state.drvport =/= undefined ->
	Port=State#state.drvport,
	dmsg(State, "> Msg: ~p ... Port: ~p~n", [Msg, Port]),
    {noreply, State}.





%% @private 
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
%%% Internal functions
%% --------------------------------------------------------------------

dmsg(State, Msg) when State#state.debug==true ->
	io:format(Msg);

dmsg(_State, _Msg) ->
	ok.

dmsg(State, Msg, Params) when State#state.debug==true ->
	io:format(Msg, Params);

dmsg(_State, _Msg, _Params) ->
	ok.
