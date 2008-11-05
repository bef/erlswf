-module(ssamod).
-compile(export_all).

modlist() ->
	Dir = filename:dirname(code:which(swf)),
	{ok, DirList} = file:list_dir(Dir),
	ModList = lists:filter(fun("ssamod_"++_) -> true; (_) -> false end, [filename:basename(N, code:objfile_extension()) || N <- DirList]),
	[list_to_atom(X) || X <- ModList].
	%lists:keysort(2, lists:map(fun("ssamod_"++ShortName = Mod) -> {list_to_atom(Mod), ShortName} end, ModList)).

commands(Mods) ->
	Cmds = lists:map(fun(Mod) ->
		Attrs = Mod:module_info(attributes),
		C1 = proplists:get_value(commands, Attrs, []),
		lists:map(fun({CmdName, CmdDetails}) ->
			{CmdName, Mod, CmdDetails}
		end, C1)
	end, Mods),
	lists:append(Cmds).

commands() ->
	commands(modlist()).


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
