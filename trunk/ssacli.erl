#!/usr/bin/env escript
-mode(compile).
%%
%% Command Line Interface for the simple swf analyzer
%%   author: Ben Fuhrmannek <bef@pentaphase.de>
%%   license: GPLv3 - http://www.gnu.org/licenses/licenses.html#GPL
%%

addlibpath() ->
	%% search for erlswf base directory
	SN = escript:script_name(),
	ScriptName = case file:read_link(SN) of
		{ok, X} -> X;
		_ -> SN
	end,
	SwfBaseDir = filename:dirname(ScriptName),

	code:add_patha(filename:join(SwfBaseDir, "ebin")),
	
	%% search for json library
	case code:which(rfc4627) of
		non_existing ->
			code:add_patha(filename:join([SwfBaseDir, "lib", "erlang-rfc4627", "ebin"]));
		_ -> ok
	end.

error_msg(Msg, Args) ->
	io:format("error: "++Msg++"~n", Args),
	halt(1).

main(Args) ->
	%% include path
	addlibpath(),
	
	%% log errors to console
	error_logger:tty(true),

	Commands = ssamod:commands(),
	
	case Args of
		[] -> %% show help
			credits(),
			usage(Commands);
		["help", CmdName] ->
			case lists:keysearch(CmdName, 1, Commands) of
				{value, {_, _, CmdDetails}} -> longhelp(CmdName, CmdDetails);
				false -> error_msg("no such command: \"~s\"", [CmdName])
			end;
		[Cmd|CArgs] ->
			case lists:keysearch(Cmd, 1, Commands) of
				{value, {_, Mod, _}} ->
					run(Mod, [Cmd|CArgs]);
				false ->
					error_msg("invalid cmd: \"~s\"", [Cmd])
			end
	end,

	%% give io some time to complete
	timer:sleep(500).

run(Mod, Args) ->
	try Mod:run(Args) of
		ok -> ok;
		invalid_usage ->
			error_msg("invalid usage. see help", []);
		Err ->
			error_msg("something went wrong: ~p", [Err])
	catch
		error:Err -> error_msg("~p: ~p", [Err, erlang:get_stacktrace()])
	end.

usage(Cmds) ->
	io:format("usage: ~s [cmd] [args]~n", [escript:script_name()]),
	io:format("  where [cmd] is one of~n~n"),
	shorthelp(Cmds),
	io:format("detailed help: help [cmd]~n~n").

shorthelp(Cmds) ->
	lists:foreach(fun({CmdName, Mod, CmdDetails}) ->
		Usage = proplists:get_value(usage, CmdDetails, "(no usage / do not use)"),
		ShortHelp = proplists:get_value(shorthelp, CmdDetails, "(no short help available)"),
		io:format("  -> ~s (module: ~s)~n", [CmdName, atom_to_list(Mod)]),
		io:format("  |  ~s~n", [Usage]),
		io:format("  |  ~s~n~n", [ShortHelp])
	end, lists:keysort(1, Cmds)).

longhelp(CmdName, CmdDetails) ->
	Usage = proplists:get_value(usage, CmdDetails, "(no usage / do not use)"),
	ShortHelp = proplists:get_value(shorthelp, CmdDetails, "(no short help available)"),
	LongHelp = proplists:get_value(longhelp, CmdDetails, "(no detailed help available)"),
	io:format("*** help for '~s' ***~n", [CmdName]),
	io:format("~s~n~n~s~n~n~s~n~n", [Usage, ShortHelp, LongHelp]).

credits() ->
	io:format(" ____ ____    _    ~n/ ___/ ___|  / \\   ~n\\___ \\___ \\ / _ \\  ~n ___) |__) / ___ \\ ~n|____/____/_/   \\_\\  (cl) Ben Fuhrmannek <bef@pentaphase.de>~n~n", []).
