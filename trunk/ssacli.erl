#!/usr/bin/env escript
%%
%% Command Line Interface for the simple swf analyzer
%%   author: Ben Fuhrmannek <bef@pentaphase.de>
%%   license: GPLv3 - http://www.gnu.org/licenses/licenses.html#GPL
%%

libdir() ->
	SN = escript:script_name(),
	ScriptName = case file:read_link(SN) of
		{ok, X} -> X;
		_ -> SN
	end,
	filename:join(filename:dirname(ScriptName), "ebin").

main(Args) ->
	%% include path
	code:add_patha(libdir()),
	
	%% log errors to console
	error_logger:tty(true),
	
	case Args of
		["dump", Filename] ->
			swfutils:dumpswf(swf:swffile(Filename));
		["rawdump", Filename] ->
			io:format("~p~n", [swf:swffile(Filename)]);
		["filedump", Filename, Prefix] ->
			swfutils:filedumpswf(swf:swffile(Filename), Prefix);
		["dumptags", Filename | Tagnames] ->
			swfutils:dumptags(swf:parsetorawtags(swf:readfile(Filename)), [list_to_atom(Tagname) || Tagname <- Tagnames]);
		["check" | Filenames] ->
			traverse_files(fun(Filename) ->
				try swfutils:dumpsecuritycheck(swf:swffile(Filename)) of
					_ -> ok
				catch
					error:X ->
						io:format("ERROR: ~p stacktrace: ~p~n", [X, erlang:get_stacktrace()])
				end
			end, Filenames);
		%["tag21", Filename, Prefix] ->
		%	swf:savetag21(swf:swffile(Filename), Prefix);
		%["rmut", Infile, Outfile] ->
		%	swf:rmut(swf:swffile(Infile), Outfile);
		["test" | Filenames] ->
			traverse_files(fun(Filename) ->
				statistics(runtime), statistics(wall_clock), %% start the clock
			
				%% decode swf
				{swf, _H, _Tags} = swf:swffile(Filename),
				%lists:foreach(fun({tag, Code, Name, Pos, Raw, DecodedTag}) ->
				%	io:format("~p~n", [Name])
				%	end, Tags),
				
				%% stop the clock
				{_, Time1} = statistics(runtime),
				{_, Time2} = statistics(wall_clock),
				io:format("	runtime: ~p~n	realtime:~p~n", [Time1, Time2])
			end, Filenames);
		_ ->
			credits(),
			usage()
	end,
	
	%% give io some time to complete
	timer:sleep(500).
	

usage() ->
	Format = "	~s~n	### ~s ###~n~n",
	io:format("usage: foo [cmd] [args]~n", []),
	io:format(Format, ["dump <swf>", "dump whole swf"]),
	io:format(Format,["rawdump <swf>", "debug dump"]),
	%io:format("	tag21 <swf> <saveprefix>~n",[]),
	io:format(Format, ["filedump <swf> <saveprefix>", "dump to file structure"]),
	%io:format("	rmut <in_swf> <out_swf>~n",[]),
	io:format(Format, ["dumptags <swf> <tagname> ...", "dump specified tags only"]),
	io:format(Format, ["test <swf1> ...", "test library (read swf w/o dump)"]),
	io:format(Format, ["check <swf>", "check file for security issues (experimental/incomplete)"]),
	halt(1).

credits() ->
	io:format(" ____ ____    _    ~n/ ___/ ___|  / \\   ~n\\___ \\___ \\ / _ \\  ~n ___) |__) / ___ \\ ~n|____/____/_/   \\_\\  (cl) Ben Fuhrmannek <bef@pentaphase.de>~n~n", []).

traverse_files(Fun, Filenames) ->
	lists:foreach(fun(F) ->
		io:format("processing file ~p~n", [F]),
		try Fun(F) of
			_ -> ok
		catch
			throw:no_swf ->
				io:format("	no swf~n", []);
			error:data_error ->
				io:format("	data error. unable to uncompress file~n", [])
		end
	end, Filenames).
