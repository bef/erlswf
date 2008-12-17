-module(ssamod_api).
-export([run/1]).
-include("swf.hrl").

-commands([
	{"api", [
		{usage, "api"},
		{shorthelp, "experimental api"}
		]}
	]).


run(["api"]) ->
	Commands = ssamod:commands(),
	
	api_loop(Commands),
	ok;

run(_) -> invalid_usage.

api_loop(Commands) ->
	% io:format("~p~n", [Commands]),
	case io:get_line("100 ready.\n") of
		eof ->
			ok;
		{error, Err} ->
			io:format("500 api error: ~p~n", [Err]);
		Input ->
			case string:tokens(string:strip(Input, right, $\n), " \t") of
				[] -> io:format("501 empty command~n");
				[Cmd|Args] ->
					case lists:keysearch(Cmd, 1, Commands) of
						{value, {_, Mod, _}} ->
							{Code, Msg} = run(Mod, [Cmd|Args]),
							io:format("~p ~s~n", [Code, Msg]);
						_ ->
							io:format("404 invalid command~n")
					end
			end,
			
			api_loop(Commands)
	end.

run(Mod, Args) ->
	try Mod:run(Args) of
		ok -> {200, "ok"};
		invalid_usage -> {405, "invalid usage"};
		Err -> {201, lists:flatten(io_lib:format("~p", [Err]))}
	catch
		error:Err -> {502, lists:flatten(io_lib:format("~p, ~p", [Err, erlang:get_stacktrace()]))}
	end.
