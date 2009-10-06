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
			   ,uname
			   }).

%% NOTE: MUST BE IN-SYNC with 'dbus-shared.h'
%% *****
-define(DBUS_NAME_FLAG_DO_NOT_QUEUE, 4).

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
	UName=State#state.uname,
	Port = open_port({spawn, Drv}, [{packet, 4}, binary, exit_status]),
	api(Port, UName, Msg),
    {noreply, State#state{drvport=Port}};


handle_cast({api, Msg}, State) when State#state.drvport =/= undefined ->
	Port=State#state.drvport,
	UName=State#state.uname,
	dmsg(State, "> Msg: ~p ... Port: ~p~n", [Msg, Port]),
	api(Port, UName, Msg),
    {noreply, State}.


api(Port, UName, init) ->
	io:format("* Init ~n");

api(Port, UName, {subscribe_signals, List}) ->
	do_subscribe_signals(Port, UName, List);
  

api(Port, UName, {register, Name}) ->
	%io:format("* Register: UName: ~p  Name: ~p", [UName, Name]),
	Uncoded=[m, 0, {UName}, {"org.freedesktop.DBus"}, 
			 {"/org/freedesktop/DBus"}, 
			 {"org.freedesktop.DBus"}, 
			 {"RequestName"}, 
			 {str, Name}, 
			 {ui32, ?DBUS_NAME_FLAG_DO_NOT_QUEUE}],
	Coded=erlang:term_to_binary(Uncoded),
	%% @TODO make safe...
	erlang:port_command(Port, Coded);
	

api(_Port, UName, Msg) ->
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


%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%
%% ============================================= Helpers
%%


hmsg(State, {unique_name, Name}) ->
	dmsg(State, "** Name: ~p", [Name]),
	State#state{uname=Name};


hmsg(State, Msg) ->
	dmsg(State, "Msg: ~p", [Msg]),
	State.



do_subscribe_signals(Port, UName, []) ->
	finished;

do_subscribe_signals(Port, UName, [Signal|Rest]) ->
	Coded=prepDbusMethod(UName, "AddMatch", []),
	erlang:port_command(Port, Coded),
	do_subscribe_signals(Port, UName, Rest).	



	
prepDbusMethod(UName, Member, Params) ->
	Uncoded=[m, 0, {UName}, {"org.freedesktop.DBus"}, 
			 {"/org/freedesktop/DBus"}, 
			 {"org.freedesktop.DBus"}, 
			 {Member},
			 Params], 
	erlang:term_to_binary(Uncoded).
	



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


%% @doc Translate port driver exit status code
%%		to human readable format 
%%		NOTE: should be in-sync with 'erlang_dbus_driver.h'
%%
code(0)  -> "EDBUS_OK";
code(1)  -> "EDBUS_CONN_ERROR";
code(2)  -> "EDBUS_DISCONNECTED";
code(3)  -> "EDBUS_ADD_MATCH_ERROR";
code(4)  -> "EDBUS_ADD_FILTER_ERROR";
code(5)  -> "EDBUS_INIT_MESSAGE";
code(6)  -> "EDBUS_SEND_ERROR";
code(7)  -> "EDBUS_UNSUPPORTED_TYPE";
code(8)  -> "EDBUS_UNRECOVERABLE_ERROR";
code(9)  -> "EDBUS_INVALID_UNIQUE_NAME";
code(10) -> "EDBUS_ERROR_SENDING_UNIQ";
code(11) -> "EDBUS_REGISTRATION_FAILED";
code(12) -> "EDBUS_RECEIVE_ERROR";
code(13) -> "EDBUS_DECODE_HEADER_ERROR";
code(14) -> "EDBUS_MALLOC_ERROR";
code(15) -> "EDBUS_CREATE_DBUSMSG_ERROR";
code(16) -> "EDBUS_UNKNOWN_EGRESS_STATE";
code(17) -> "EDBUS_DECODE_ERROR";
code(_)  -> "UNKNOWN".
