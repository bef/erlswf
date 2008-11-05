-module(ssamod_abcdump).
-export([run/1]).

-commands([
	{"abcdump", [
		{usage, "abcdump <swf|abc> [version|cpool|metadata|scripts|methods|instances|classes]..."},
		{shorthelp, "dump swf's abc-parts in human readable format"}]},
	{"abcdump-raw", [
		{usage, "abcdump-raw <abc>"},
		{shorthelp, "raw abc dump"}]}
	]).


run(["abcdump", Filename|Parts]) ->
	Bin = swf:readfile(Filename),
	case swfmime:getmime(Bin) of
		swf ->
			RawSwf = swf:parsetorawtags(Bin),
			AbcList = swfutils:abcdata(RawSwf),
			lists:foreach(fun(AbcBinary) -> abcdump(AbcBinary, Parts) end, AbcList);
		abc ->
			abcdump(swf:readfile(Filename), Parts);
		_ ->
			io:format("unknown fileformat~n")
	end;

run(["abcdump-raw", Filename]) ->
	{X,_} = swfabc:abc(swf:readfile(Filename)),
	io:format("~p~n", [X]);


run(_) -> invalid_usage.

abcdump(AbcBinary, Parts) ->
	io:format("~n----- ABCDUMP -----~n", []),
	{ABC,_} = swfabc:abc(AbcBinary),
	P = case Parts of
		[] -> all;
		_ -> Parts
	end,
	swfabcformat:abc(standard_io, ABC, P).
