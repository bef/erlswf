-module(ssamod_version).
-export([run/1]).

-commands([
	{"version", [
		{usage, "version <swf>"},
		{shorthelp, "determine SWF version"},
		{longhelp, "lirum larum"}]}
	]).


run(["version", Filename]) ->
	{ok, Io} = file:open(Filename, [read]),
	{ok, Start} = file:read(Io, 4),
	file:close(Io),
	PrintVersion =
		fun(<<_, "WS", Version>>) ->
			io:format("~p~n", [Version]);
		(_) ->
			io:format("undef~n",[])
		end,
	PrintVersion(list_to_binary(Start)),
	ok;

run(_) -> invalid_usage.