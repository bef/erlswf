#!/usr/bin/env escript
-mode(compile).

%%
%% squid url rewriter
%%

%% squid.conf:
%% acl flashfiles urlpath_regex -i \.swf$
%% acl redirector browser -i eswfurlrewrite
%%
%% url_rewrite_program /Users/BeF/devel/stuff/trunk/swf/url_rewrite.erl
%% url_rewrite_children 1
%% url_rewrite_concurrency 1
%% url_rewrite_access deny to_localhost
%% url_rewrite_access deny redirector
%% url_rewrite_access allow flashfiles
%% url_rewrite_access deny all
%% 


main(_) ->
	code:add_pathsz([
		"/Users/BeF/Desktop/software/jungerl/lib/ibrowse/ebin", %% ibrowse
		"/Users/BeF/devel/erlswf/trunk/ebin" %% swf analyzer
		]),
	
	error_logger:tty(false),
	ibrowse:start(),
	Sender = spawn(fun sender/0),
	receiver(Sender).

sender() ->
	receive
		quit -> ok;
		{put, S} ->
			io:format("~s", [S]),
			sender();
		{log, S} ->
			file:write_file("/tmp/urlrewritedump", list_to_binary(S), [append]),
			sender()
	end.

receiver(Sender) ->
	case io:get_line('') of
		eof ->
			Sender ! quit,
			halt(0);
		String ->
			spawn(fun() -> worker(Sender, String) end),
			receiver(Sender)
	end.

worker(Sender, String) ->
	[ID, URL, _Client, _User, _Method, _URLGroup|_] = string:tokens(String, " "),
	NewURL = case check_url(URL) of
		true -> URL;
		{false, Reason} -> "302:about:" ++ lists:flatten(io_lib:format("~p", [Reason]))
	end,
	Sender ! {log, lists:flatten(io_lib:format("~s ~s -> ~s~n", [ID, URL, NewURL]))},
	Sender ! {put, lists:flatten(io_lib:format("~s ~s~n", [ID, NewURL]))}.

check_url(URL) ->
	case ibrowse:send_req(URL, [{"User-Agent", "eswfurlrewrite/0.1"}], get, [], [{proxy_host, "localhost"}, {proxy_port, 3128}]) of
		{error, X} -> {false, X};
		{ok, _Code, _Header, Body} ->
			B = list_to_binary(Body),
			try swfutils:silentsecuritycheck(swf:swf(B)) of
				ok -> true
			catch
				throw:{not_ok, Reason} -> {false, Reason};
				throw:no_swf -> {false, no_swf};
				throw:X -> {false, X};
				error:X -> {false, X}
			end
	end.
