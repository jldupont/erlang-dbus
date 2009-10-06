%% Author: jldupont
%% Created: 2009-10-06
%% Description: Erlang Client Interface to DBus
%%
%% @doc
%% == Messages Generated ==
%%
%%  ```{edbus, driver.crashed}'''
%%
%%
-module('erlang-dbus-client').

%%
%% ============================================= DEFINES
%%
-define(DRV,       "erlang-dbus-driver").
-define(DRV_DEBUG, "erlang-dbus-driver_debug").


%%
%% ============================================= EXPORTED API
%%
-export([
		 init/0
		,init/1
		,subscribe_signals/1
		,register_name/1
		,send_method/1
		,send_signal/1
		,send_return/1
		,send_error/1
		 ]).

%%
%% ============================================= API Functions
%%

%% @doc Initializes the Client Interface
%%
init() ->
	do_init(nodebug).

%% @doc Initializes the Client Interface in debug mode
%%
init(debug) ->
	do_init(debug).


%% @doc Subscribe a Client to a list of Signals
%%
%% @spec subscribe_signals(List) -> ok
%% where 
%%	List=[string()]
%%
subscribe_signals(List) when is_list(List) ->
	ok.


%% @doc Registers a "Name" with DBus
%%
%% @spec register_name(Name) -> ok
%% where
%%	Name=string()
%%
register_name(Name) when is_list(Name) ->
	ok.

%% @doc Sends a "Method Call" message
%%
%% @spec send_method({Serial, Destination, Path, Interface, Member, Message}) -> ok
%% where
%%	Serial=integer()
%%	Destination=string()
%%	Path=string()
%%	Interface=string()
%%	Member=string()
%%	Message=term()
%%
send_method({Serial, Destination, Path, Interface, Member, Message}) ->
	ok.

%% @doc Sends a "Signal" message
%%
%% @spec send_signal({Serial, Destination, Path, Interface, Member, Message}) -> ok
%% where
%%	Serial=integer()
%%	Destination=string()
%%	Path=string()
%%	Interface=string()
%%	Member=string()
%%	Message=term()
%%
send_signal({Serial, Destination, Path, Interface, Member, Message}) ->
	ok.

%% @doc Sends a "Method Return" message
%%
%% @spec send_return({Serial, Destination, Message}) -> ok
%% where
%%	Serial=integer()
%%	Destination=string()
%%	Message=term()
%%
send_return({Serial, Destination, Message}) ->
	ok.

%% @doc Sends a "Error" message
%%
%% @spec send_error({Serial, Destination, Name, Message}) -> ok
%% where
%%	Serial=integer()
%%	Destination=string()
%%	Name=string()
%%	Message=term()
%%
send_error({Serial, Destination, Name, Message}) ->
	ok.


%%
%% ------------------------------------------------- Local Functions
%%
do_init(Debug) ->
	maybe_init(Debug).

maybe_init(debug) ->
	real_init(?DRV_DEBUG);

maybe_init(_) ->
	real_init(?DRV).

real_init(Drv) ->
	ok.




