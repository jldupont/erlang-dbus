%%% -------------------------------------------------------------------
%%% Author  : Jean-Lou Dupont
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
			   ,name
			   }).

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


handle_cast({api, Msg}, State) when State#state.drvport == undefined ->
	dmsg(State, "> Starting Port"),
	Drv=State#state.drvpath,
	Port = open_port({spawn, Drv++" type=\'signal\' type=\'method_return\' type=\'error\'"}, [{packet, 4}, binary, exit_status]),
	api(Port, Msg),
    {noreply, State#state{drvport=Port}};


handle_cast({api, Msg}, State) when State#state.drvport =/= undefined ->
	Port=State#state.drvport,
	dmsg(State, "> Msg: ~p ... Port: ~p~n", [Msg, Port]),
	api(Port, Msg),
    {noreply, State}.


api(Port, {register, Name}) ->
	Uncoded=[m, 0],
	Coded=erlang:term_to_binary(Uncoded),
	%% @TODO make safe...
	erlang:port_command(Port, Coded);
	

api(_Port, Msg) ->
	dmsg("api: unsupported msg: ~p", [Msg]),
	ok.


	




%% @doc Port Data Reception
%%
%% @private
handle_info({_Port, {data, Data}}, State) ->
	Msg=erlang:binary_to_term(Data),
	NewState=hmsg(State, Msg),
    {noreply, NewState};

%% @doc Port driver crashed 
%%
%% @private
handle_info({_Port, {exit_status, Reason}}, State) ->
	dmsg(State, "Driver crashed, Reason: ~p", [Reason]),
    {noreply, State#state{drvport=undefined}};

%% @doc Catch-all
%%
%% @private
handle_info(Info, State) ->
	dmsg(State, "Info: ~p", [Info]),
    {noreply, State}.



hmsg(State, {unique_name, Name}) ->
	dmsg(State, "** Name: ~p", [Name]),
	State#state{name=Name};


hmsg(State, Msg) ->
	dmsg(State, "Msg: ~p", [Msg]),
	State.






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
	io:format(Msg++"~n");

dmsg(_State, _Msg) ->
	ok.

dmsg(State, Msg, Params) when State#state.debug==true ->
	io:format(Msg++"~n", Params);

dmsg(_State, _Msg, _Params) ->
	ok.
