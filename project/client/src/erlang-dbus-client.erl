%% Author: jldupont
%% Created: 2009-10-06
%% Description: Erlang Client Interface to DBus
%%
%% @doc
%% == Messages Generated ==
%%
%% The following messages are generated back to the Client.
%%
%%	An error occured:
%%
%%	```{edbus, {error, Reason}}'''
%%
%%	The Client interface is ready to be used:
%%
%%	```{edbus, {ready, UName}}''' 
%%
%%  The Client attempted to use the interface prematurely:
%%
%%	```{edbus, {error, 'interface.not.ready'}}'''
%%
%%
-module('erlang-dbus-client').

%%
%% ============================================= DEFINES
%%
-define(DRV,        "erlang-dbus-driver").
-define(DRV_DEBUG,  "erlang-dbus-driver_debug").
-define(SERVER,     edbus).
-define(SERVER_MOD, 'erlang-dbus-client-server').
-define(TOOLS,      'erlang-dbus-client-tools').

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

-export([
		 t1/0, t1/1
		 ]).

%%
%% ============================================= API Functions
%%

%% @doc Initializes the Client Interface
%%
%% @spec init() -> ok | {error, Reason}
init() ->
	do_init(nodebug).

%% @doc Initializes the Client Interface in debug mode
%%
%% @spec init(debug) -> ok | {error, Reason}
init(debug) ->
	do_init(debug).


%% @doc Subscribe a Client to a list of Signals
%%
%%		The subscription is based on the match rule
%%		on the 'interface' parameter i.e. the Client
%%		is subscribed to all signals sent on the
%%		list of supplied interfaces.
%%
%% @spec subscribe_signals(List) -> ok | {error, Reason}
%% where 
%%	List=[string()]
%%
subscribe_signals(List) when is_list(List) ->
	safe_send_to_server({subscribe_signals, List});

subscribe_signals(_) ->
	{error, invalid.parameter}.


%% @doc Registers a "Name" with DBus
%%
%% @spec register_name(Name) -> ok | {error, Reason}
%% where
%%	Name=string()
%%
register_name(Name) when is_list(Name) ->
	safe_send_to_server({register, Name});

register_name(_) ->
	{error, invalid.parameter}.


%% @doc Sends a "Method Call" message
%%
%%	Note that the 'Serial' parameter is unused at present.
%%
%% @spec send_method({Serial, Destination, Path, Interface, Member, Message}) -> ok | {error, Reason}
%% where
%%	Serial=integer()
%%	Destination=string()
%%	Path=string()
%%	Interface=string()
%%	Member=string()
%%	Message=term()
%%
send_method({Serial, Destination, Path, Interface, Member, Message}) ->
	safe_send_to_server({method, Serial, Destination, Path, Interface, Member, Message});

send_method(_) ->
	{error, invalid.parameter}.


%% @doc Sends a "Signal" message
%%
%%	Note that the 'Serial' parameter is unused at present.
%%
%% @spec send_signal({Serial, Destination, Path, Interface, Member, Message}) -> ok | {error, Reason}
%% where
%%	Serial=integer()
%%	Destination=string()
%%	Path=string()
%%	Interface=string()
%%	Member=string()
%%	Message=term()
%%
send_signal({Serial, Destination, Path, Interface, Member, Message}) ->
	safe_send_to_server({signal, Serial, Destination, Path, Interface, Member, Message});

send_signal(_) ->
	{error, invalid.parameter}.


%% @doc Sends a "Method Return" message
%%
%%	Note that the 'Serial' parameter is unused at present.
%%
%% @spec send_return({Serial, Destination, Message}) -> ok | {error, Reason}
%% where
%%	Serial=integer()
%%	Destination=string()
%%	Message=term()
%%
send_return({Serial, Destination, Message}) ->
	safe_send_to_server({return, Serial, Destination, Message});

send_return(_) ->
	{error, invalid.parameter}.

%% @doc Sends a "Error" message
%%
%%	Note that the 'Serial' parameter is unused at present.
%%
%% @spec send_error({Serial, Destination, Name, Message}) -> ok | {error, Reason}
%% where
%%	Serial=integer()
%%	Destination=string()
%%	Name=string()
%%	Message=term()
%%
send_error({Serial, Destination, Name, Message}) ->
	safe_send_to_server({error, Serial, Destination, Name, Message});

send_error(_) ->
	{error, invalid.parameter}.

%%
%% ----------------------------------------------------------------------- Local Functions
%%

%% @private
safe_send_to_server(Message) ->
	try
		gen_server:cast(?SERVER, {self(), api, Message})
	catch
		_:_ -> {error, server.unreachable}
	end.
	
%% @private
do_init(Debug) ->
	maybe_init(Debug).

%% @private
maybe_init(debug) ->
	real_init(?DRV_DEBUG, true);

%% @private
maybe_init(_) ->
	real_init(?DRV, false).

%% @private
real_init(Drv, Debug) ->
	R=?TOOLS:find_driver(Drv),
	case R of
		{ok, Filename} ->
			Result=gen_server:start_link({local, ?SERVER}, ?SERVER_MOD, [{client, self()}, {drv, Filename}, {debug, Debug}], []),
			gen_server:cast(?SERVER, {self(), api, init}),
			Result;
		_ ->
			{error, driver_not_found}
	end.




%%
%% ------------------------------------------------- Test Functions
%%

%% @private
t1() ->
	?MODULE:init(debug).

%% @private
t1(stop) ->
	gen_server:cast(?SERVER, stop);

%% @private
t1(reg) ->
	?MODULE:register_name("com.jldupont.erlang_dbus_client").



