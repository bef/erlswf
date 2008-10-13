#!/usr/bin/env escript

main([Filename|_]) ->
	{ok, Io} = file:open(Filename, [read]),
	{ok, Start} = file:read(Io, 4),
	file:close(Io),
	print_version(list_to_binary(Start));
main(_) ->
	io:format("version.erl Filename~n",[]).

print_version(<<_, "WS", Version>>) ->
	io:format("~p~n", [Version]);
print_version(_) ->
	io:format("undef~n",[]).

