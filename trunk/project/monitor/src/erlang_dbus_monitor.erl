%% Author: jldupont
%% Created: 2009-10-01
%% Description: DBus Monitor
%
%  Filter Match Rule
%  =================
% 
%  sender=
%  interface=
%  member=
%  path=
%  destination
%  type=
%  
-module(erlang_dbus_monitor).

-define(TIMEOUT, 100).
-define(TOOLS,   'erlang-dbus-client-tools').
-define(DRV,     "erlang-dbus-driver_debug").

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
			io:format("data: ~p~n", [Term]);
			
		Other ->
			io:format("Other: ~p~n", [Other])
		  
	end,
	loop().
	

start_drv() ->
	Result=?TOOLS:find_driver(?DRV),
	case Result of
		{ok, Filename} ->
			Port = open_port({spawn, Filename++" type=\'method_call\' type=\'signal\' type=\'method_return\' type=\'error\'"}, [{packet, 4}, binary, exit_status]),
			self() ! {port, Port},
			Result;
		_ ->
			{error, driver_not_found}
	end.
		   


