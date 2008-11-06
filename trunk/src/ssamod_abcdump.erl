-module(ssamod_abcdump).
-export([run/1]).

-commands([
	{"abcdump", [
		{usage, "abcdump <swf|abc> [version|cpool|metadata|scripts|methods|instances|classes]..."},
		{shorthelp, "dump SWF's ABC-parts in human readable format"}]},
	{"abcdump-raw", [
		{usage, "abcdump-raw <abc>"},
		{shorthelp, "raw ABC dump"}]},
	{"jsonabc", [
		{usage, "jsonabc <swf|abc>"},
		{shorthelp, "dump ABC as JSON (incomplete implementation / work in progress)"},
		{longhelp, "	(1) when supplied with an ABC file -> dump JSON object {...}
	(2) otherwise dump Array of JSON objects [{...},...], since one SWF may contain more than one doABC tag"}]}
	]).


run(["abcdump", Filename|Parts]) ->
	Bin = swf:readfile(Filename),
	case swfmime:getmime(Bin) of
		swf ->
			RawSwf = swf:parsetorawtags(Bin),
			AbcList = swfutils:abcdata(RawSwf),
			lists:foreach(fun(AbcBinary) -> abcdump(AbcBinary, Parts) end, AbcList);
		abc ->
			abcdump(Bin, Parts);
		_ ->
			io:format("unknown fileformat~n")
	end;

run(["abcdump-raw", Filename]) ->
	{X,_} = swfabc:abc(swf:readfile(Filename)),
	io:format("~p~n", [X]);

run(["jsonabc", Filename]) ->
	Bin = swf:readfile(Filename),
	Obj = case swfmime:getmime(Bin) of
		swf ->
			RawSwf = swf:parsetorawtags(Bin),
			AbcList = swfutils:abcdata(RawSwf),
			lists:map(fun(AbcBinary) ->
				{Abc,_} = swfabc:abc(AbcBinary),
				swfjsonabc:obj(Abc)
			end, AbcList);
		abc ->
			{Abc,_} = swfabc:abc(Bin),
			swfjsonabc:obj(Abc);
		_ ->
			io:format("unknown fileformat~n")
	end,
	JsonStr = rfc4627:encode(Obj),
	io:format("~s~n", [JsonStr]);

run(_) -> invalid_usage.

abcdump(AbcBinary, Parts) ->
	io:format("~n----- ABCDUMP -----~n", []),
	{ABC,_} = swfabc:abc(AbcBinary),
	P = case Parts of
		[] -> all;
		_ -> Parts
	end,
	swfabcformat:abc(standard_io, ABC, P).
