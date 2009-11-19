%% Author: jldupont
%% Created: 2009-10-06
%% Description: Tools
-module('erlang-dbus-client-tools').
-include_lib("kernel/include/file.hrl").
-compile(export_all).



%% @doc Finds the specified driver file in a restricted
%%		set of paths
%%
%%		Starts by 
%%
find_driver(Name) ->
	Filename=code:which(?MODULE),
	ModuleDirname=filename:dirname(Filename),
	DriverPath1=filename:absname_join(ModuleDirname, "../driver"),
	DriverPath2=filename:absname_join(ModuleDirname, "../../driver/Debug"),
	DriverPath3=filename:absname_join(ModuleDirname, "../../driver/Release"),
	find_driver([DriverPath1, DriverPath2, DriverPath3], Name).

find_driver([], _Name) ->
	{error, not_found};

find_driver([Path|Rest], Name) ->
	FilePath=Path++"/"++Name,
	Result=file:read_file_info(FilePath),
	case Result of
		{ok, _FileInfo} ->
			{ok, FilePath};
		
		_ ->
			find_driver(Rest, Name)
	end.

	



%% @doc Very permissive pattern based filter
%%
pattern_filter(Pattern, List) ->
	pattern_filter(Pattern, List, []).

pattern_filter(_Pattern, [], Acc) ->
	Acc;

pattern_filter(Pattern, [H|T], Acc) when is_list(Pattern) ->
	
	case string:str(H, Pattern) of
		0 -> pattern_filter(Pattern, T, Acc);
		_ -> pattern_filter(Pattern, T, Acc++[H])
	end;
	
pattern_filter(_, _, Acc) ->
	Acc.
	



extract_version(File) when is_list(File) ->
	Tokens=string:tokens(File, "-"),
	maybe_extract_version(Tokens).


maybe_extract_version([_FileName, Version]) ->
	Version;
	
maybe_extract_version(_) ->
	undefined.
	


%% @doc Compare version information
%%
compare_version(Min, Version) ->
	MinF=tofloat(Min),
	VersionF=tofloat(Version),
	cv(MinF, VersionF).


cv(error, _) -> error;
cv(_, error) -> error;

cv(MinF, VersionF) ->
	VersionF > MinF.
	

tofloat(N) when is_list(N) ->
	case string:to_float(N) of
		{error, _} -> 
			case string:to_integer(N) of
				{error, _} -> error;
				{Int, _}   -> 1.0*Int
			end;
		{FN, _}    -> FN
	end;
tofloat(N) when is_float(N)   -> N;
tofloat(N) when is_integer(N) -> 1.0*N;
tofloat(_) -> error.



%% @doc Finds a file of BaseName in the directory BasePath 
%%		having a version number of at least MinVersion.
%%		If the file Default is found, return it regardeless.
%% 
%% @spec find_file(BasePath, BaseName, Default, MinVersion) -> {ok, FilePath} | {error, Reason}
%% where
%%	BasePath = string()
%%	BaseName = string()
%%	Default  = string()
%%	MinVersion = string() | float() | integer()
%%	Reason = term()
%%
find_file(BasePath, BaseName, Default, MinVersion) ->
	Ret=file:list_dir(BasePath),
	maybe_find_file(BasePath, BaseName, Default, MinVersion, Ret).


maybe_find_file(BasePath, BaseName, Default, MinVersion, {ok, FileList}) ->
	FilteredList=pattern_filter(BaseName, FileList),
	
	case lists:member(Default, FilteredList) of
		true -> {ok, BasePath++"/"++Default};
		_    ->	estimate_file(MinVersion, FilteredList)
	end;

maybe_find_file(_, _, _, _, _) ->
	{error, 'file.not.found'}.



estimate_file(_, []) ->
	{error, 'file.not.found'};

%% @doc Finds the highest version number
%%		that is *at least* greater than the minimum required
%%
estimate_file(MinVersion, [File|Rest]=_FileList) ->
	Version=extract_version(File),
	case compare_version(MinVersion, Version) of
		true -> {ok, File};
		false-> estimate_file(MinVersion, Rest)
	end;

estimate_file(_, _) ->
	{error, 'file.not.found'}.



%% @doc Adds 1 to the current value of Var
%%
addvar(Var) when is_atom(Var) -> addvar(Var, 1);
addvar(_) -> error.


%% @doc Adds Count to the current value of Var
%%
addvar(Var, Count) when is_atom(Var) and is_integer(Count) ->
	addvar(Var, get(Var), Count);

addvar(_,_) ->
	error.


addvar(Var, undefined, Count) -> put(Var, Count);
addvar(Var, Value, Count)     -> put(Var, Value+Count).


%% ------------------------------------------------------------------- TESTS

t1() ->
	Result=find_driver("erlang-dbus-driver_debug"),
	io:format("~p", [Result]).



