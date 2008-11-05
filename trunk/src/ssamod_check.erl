-module(ssamod_check).
-export([run/1]).

-commands([
	{"check", [
		{usage, "check <swfs>..."},
		{shorthelp, "check file for security issues (experimental/incomplete)"},
		{longhelp, "checks SWF for
	(1) unknown tags and
	(2) branch offset obfuscation (applicable for AS2/AVM1 only)"}]}
	]).

run(["check" | Filenames]) ->
	ssamod:traverse_files(fun(Filename) ->
		try swfutils:dumpsecuritycheck(swf:swffile(Filename)) of
			_ -> ok
		catch
			error:X ->
				io:format("ERROR: ~p stacktrace: ~p~n", [X, erlang:get_stacktrace()])
		end
	end, Filenames);

run(_) -> invalid_usage.
