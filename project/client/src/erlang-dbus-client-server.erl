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
			   ,'client.pid'
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
init([{client, ClientPid}, {drv, Drv}, {debug, Debug}]) ->
	io:format("server: drv: ~p", [Drv]),
    {ok, #state{drvpath=Drv, debug=Debug, 'client.pid'=ClientPid}}.



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

%% @doc Start the Driver before dispatching the API request
%%		Restarting the driver is, most probably, as a result
%%		of the driver crashing due to spurious disconnect events
%%		from DBus (exit_code==2)... or a bug in the driver ;-)
%%
%% @private
handle_cast({From, api, Msg}, State) when State#state.drvport == undefined ->
	dmsg(State, "> Starting Port"),
	Drv=State#state.drvpath,

	UName=State#state.uname,
	Port = open_port({spawn, Drv}, [{packet, 4}, binary, exit_status]),
	api(From, Port, UName, Msg),
    {noreply, State#state{drvport=Port}};

%% @doc Dispath API request
%%
%% @private
handle_cast({From, api, Msg}, State) when State#state.drvport =/= undefined ->
	Port=State#state.drvport,
	UName=State#state.uname,
	api(From, Port, UName, Msg),
    {noreply, State}.



%% @private
api(_From, _Port, _UName, init) ->
	io:format("* Init ~n");

api(From, _Port, undefined, _Msg) ->
	safe_reply(From, {edbus, {error, 'interface.not.ready'}});

api(From, Port, UName, {subscribe_signals, List}) ->
	do_subscribe_signals(From, Port, UName, List);
  
api(From, Port, UName, {register, Name}) ->
	do_register_name(From, Port, UName, Name);

api(From, Port, UName, {method, Serial, Destination, Path, Interface, Member, Message}) ->
	do_send(From, m, Port, UName, Serial, Destination, Path, Interface, Member, Message);

api(From, Port, UName, {signal, Serial, Destination, Path, Interface, Member, Message}) ->
	do_send(From, s, Port, UName, Serial, Destination, Path, Interface, Member, Message);

api(From, Port, UName, {return, Serial, Destination, Message}) ->
	do_send(From, r, Port, UName, Serial, Destination, Message);

api(From, Port, UName, {error, Serial, Destination, Name, Message}) ->
	do_send(From, e, Port, UName, Serial, Destination, Name, Message);

%%% CATCH-ALL %%%
api(_From, _Port, _UName, Msg) ->
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
	dmsg(State, "Unsupported Info: ~p", [Info]),
    {noreply, State}.


%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%
%% ===================================================================== Helpers
%%


%% @doc Receives the 'unique-name' assigned by DBus
%%		to our connection. Communicate this information
%%		back to the Client signaling that the interface
%%		can safely be used.
%% 
%% @private
hmsg(State, {unique_name, Name}) ->
	ClientPid=State#state.'client.pid',
	safe_reply(ClientPid, {edbus, {ready, Name}}),
	dmsg(State, "** Name: ~p", [Name]),
	State#state{uname=Name};


hmsg(State, Msg) ->
	dmsg(State, "Msg: ~p", [Msg]),
	State.


%% @doc Ask DBus to register a Name
%%
%% @private
do_register_name(From, Port, UName, Name) ->
	RawMsg=prep_dbus_method(UName, "RequestName", 
					[{str, Name}, 
			 		{ui32, ?DBUS_NAME_FLAG_DO_NOT_QUEUE}]),
	port_send(From, Port, RawMsg).

%% @doc Subscribe the Client to specific Signals
%%		based on the 'interface' level filter.
%%
%% @private
do_subscribe_signals(_From, _Port, _UName, []) ->
	finished;

do_subscribe_signals(From, Port, UName, [Signal|Rest]) ->
	RawMsg=prep_dbus_method(UName, "AddMatch", ["type=\'signal\' interface=\'"++Signal++"\'"]),
	port_send(From, Port, RawMsg),
	do_subscribe_signals(From, Port, UName, Rest).	

%% @doc Sends a Method_call / Signal message on DBus
%%
%% @private
do_send(From, Type, Port, UName, Serial, Destination, Path, Interface, Member, Message) ->
	RawMsg=[Type, Serial, {UName}, {Destination}, 
		 {Path}, {Interface}, {Member},
		 Message],
	port_send(From, Port, RawMsg).

%% @doc Sends a "Method_Return" message on DBus
%%
%% @private
do_send(From, r, Port, UName, Serial, Destination, Message) ->
	RawMsg=[r, Serial, {UName}, {Destination}, Message],
	port_send(From, Port, RawMsg).
	
%% @doc Sends an "Error" message on DBus
%%
%% @private
do_send(From, e, Port, UName, Serial, Destination, Name, Message) ->
	RawMsg=[e, Serial, {UName}, {Destination}, {Name}, Message],
	port_send(From, Port, RawMsg).

%% @doc Prepares a "Method_call" message
%%
%% @private	
prep_dbus_method(UName, Member, Params) ->
	[m, 0, {UName}, {"org.freedesktop.DBus"}, 
			 {"/org/freedesktop/DBus"}, 
			 {"org.freedesktop.DBus"}, 
			 {Member}]++Params.

%% @doc Performs the actual transmission
%%		of the message destined to DBus
%%		via the Port Driver.
%%
%% @private
port_send(From, Port, RawMsg) ->
	try
		Coded=erlang:term_to_binary(RawMsg),
		erlang:port_command(Port, Coded)
	catch
		_:_ ->
			safe_reply(From, {edbus, {error, send.to.driver}})
	end.

%% @doc Provide message feedback back to the Client
%%		Can't do much if the Client can't be reached...
%%		If this situation persists, this server will most
%%		probably get garbage collected anyways.
%%
%% @private
safe_reply(To, Msg) ->
	try
		To ! Msg
	catch
		_:_ ->
			io:format("safe_reply exception")
	end.


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
%% ------------------------------------------------- Internal functions
%% --------------------------------------------------------------------


%% @doc Prints a message when in 'Debug' mode
%%
%%@private
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
