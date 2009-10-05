%% Author: jldupont
%% Created: 2009-10-05
%% Description: 
-module('erlang-dbus-driver-test').

-define(TIMEOUT, 2000).
-define(DRV, "/usr/bin/erlang-dbus-driver_debug").

%%
%% Exported Functions
%%
-export([
		 start/0
		 ]).

start() ->
	start_drv(),
	loop().


loop() ->
	receive
		{port, Port} ->
			io:format("Port: ~p~n", [Port]),
			put(port, Port);
		
		{_Port, {exit_status, Reason}} ->
			io:format("EXIT REASON: ~p~n", [Reason]),
			exit(Reason);
		
		{_Port, {data, Data}} ->
			Term=binary_to_term(Data),
			handle_term(Term);
			
		Other ->
			io:format("Other: ~p~n", [Other])
	after ?TIMEOUT ->
			
			send_ping()
		  
	end,
	loop().

send_ping() ->
	Port=get(port),
	Name=get(name),
	send_ping(Port, Name).

send_ping(_, undefined) ->
	pass;
	
send_ping(Port, Name) ->
	Msg=[s, 666, {Name}, {""}, {"org.freedesktop.DBus"}, {"org.freedesktop.DBus.Peer"},  {"Ping"}, {str, "Ping"}],
	EMsg=erlang:term_to_binary(Msg),
	erlang:port_command(Port, EMsg).


handle_term({unique_name, Name}) ->
	io:format("Name: ~p~n", [Name]),
	put(name, Name);

handle_term(Term) ->
	%io:format("Term: ~p~n", [Term]).
	pass.



start_drv() ->
	Port = open_port({spawn, ?DRV++" type=\'signal\' type=\'error\'"}, [{packet, 4}, binary, exit_status]),
	self() ! {port, Port}.	


