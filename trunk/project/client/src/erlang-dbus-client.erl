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
	safe_send_to_server({subscribe_signals, List}).


%% @doc Registers a "Name" with DBus
%%
%% @spec register_name(Name) -> ok
%% where
%%	Name=string()
%%
register_name(Name) when is_list(Name) ->
	safe_send_to_server({register, Name}).

%% @doc Sends a "Method Call" message
%%
%% @spec send_method({Serial, Destination, Path, Interface, Member, Message}) -> ok | error
%% where
%%	Serial=integer()
%%	Destination=string()
%%	Path=string()
%%	Interface=string()
%%	Member=string()
%%	Message=term()
%%
send_method({Serial, Destination, Path, Interface, Member, Message}) ->
	safe_send_to_server({method, Serial, Destination, Path, Interface, Member, Message}).

%% @doc Sends a "Signal" message
%%
%% @spec send_signal({Serial, Destination, Path, Interface, Member, Message}) -> ok | error
%% where
%%	Serial=integer()
%%	Destination=string()
%%	Path=string()
%%	Interface=string()
%%	Member=string()
%%	Message=term()
%%
send_signal({Serial, Destination, Path, Interface, Member, Message}) ->
	safe_send_to_server({signal, Serial, Destination, Path, Interface, Member, Message}).

%% @doc Sends a "Method Return" message
%%
%% @spec send_return({Serial, Destination, Message}) -> ok | error
%% where
%%	Serial=integer()
%%	Destination=string()
%%	Message=term()
%%
send_return({Serial, Destination, Message}) ->
	safe_send_to_server({return, Serial, Destination, Message}).

%% @doc Sends a "Error" message
%%
%% @spec send_error({Serial, Destination, Name, Message}) -> ok | error
%% where
%%	Serial=integer()
%%	Destination=string()
%%	Name=string()
%%	Message=term()
%%
send_error({Serial, Destination, Name, Message}) ->
	safe_send_to_server({error, Serial, Destination, Name, Message}).


%%
%% ------------------------------------------------- Local Functions
%%

%% @private
safe_send_to_server(Message) ->
	try
		gen_server:cast(?SERVER, {api, Message})
	catch
		_:_ -> error
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
	Result=?TOOLS:find_driver(Drv),
	case Result of
		{ok, Filename} ->
			gen_server:start_link({local, ?SERVER}, ?SERVER_MOD, [{drv, Filename}, {debug, Debug}], []),
			gen_server:cast(?SERVER, {api, init});
		_ ->
			{error, driver_not_found}
	end.




%%
%% ------------------------------------------------- Test Functions
%%


t1() ->
	?MODULE:init(debug).

t1(stop) ->
	gen_server:cast(?SERVER, stop);

t1(reg) ->
	?MODULE:register_name("erlang_dbus_client").



