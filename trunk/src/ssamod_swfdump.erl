-module(ssamod_swfdump).
-export([run/1]).

-commands([
	{"dump", [
		{usage, "version <swf>"},
		{shorthelp, "dump whole SWF"}]},
	{"rawdump", [
		{usage, "rawdump <swf>"},
		{shorthelp, "debug dump"}]},
	{"filedump", [
		{usage, "filedump <swf> <saveprefix>"},
		{shorthelp, "dump whole SWF to file structure"}]},
	{"dumptags", [
		{usage, "dumptags <swf> <tagnames>..."},
		{shorthelp, "dump specified tags only"}]},
	{"test", [
		{usage, "test <swfs>..."},
		{shorthelp, "test library (read swf w/o dump)"}]},
	{"jsondump", [
		{usage, "jsondump <swf>"}]}
	]).


run(["dump", Filename]) ->
	swfutils:dumpswf(swf:swffile(Filename)), ok;

run(["rawdump", Filename]) ->
	io:format("~p~n", [swf:swffile(Filename)]);

run(["filedump", Filename, Prefix]) ->
	swfutils:filedumpswf(swf:swffile(Filename), Prefix), ok;

run(["dumptags", Filename | Tagnames]) ->
	swfutils:dumptags(swf:parsetorawtags(swf:readfile(Filename)), [list_to_atom(Tagname) || Tagname <- Tagnames]), ok;

run(["test" | Filenames]) ->
	ssamod:traverse_files(fun(Filename) ->
		statistics(runtime), statistics(wall_clock), %% start the clock
	
		%% decode swf
		{swf, _H, _Tags} = swf:swffile(Filename),
		
		%% stop the clock
		{_, Time1} = statistics(runtime),
		{_, Time2} = statistics(wall_clock),
		io:format("	runtime: ~p~n	realtime:~p~n", [Time1, Time2])
	end, Filenames);

run(["jsondump", Filename]) ->
	Swf = swf:swffile(Filename),
	Obj = swfjson:obj(Swf),
	JsonStr = rfc4627:encode(Obj),
	io:format("~s~n", [JsonStr]);

run(_) -> invalid_usage.