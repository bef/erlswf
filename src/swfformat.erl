-module(swfformat).
-export([
	tagformat/1,
	tagformat/2,
	formatactions/1
	]).

-include("swf.hrl").

%%
%% format tags
%%

%% @doc format tags for display/storage; 
%%    ABC can be decoded by abcdump.exe (must be in path)
%% @spec tagformat(tag()) -> [{postfix(), mimetype(), binary()}]
tagformat(#tag{code=Code, name=Name, pos=Pos, raw=Raw, contents=Contents}) ->
	[{".meta.txt", "text/plain", list_to_binary(lists:flatten(io_lib:format("code: ~3.10.0B~nname: ~s~npos: ~.10B/0x~.16B~n", [Code, atom_to_list(Name), Pos, Pos])))},
	{".raw", "application/octet-stream", Raw},
	{".contents.erl", "text/plain", list_to_binary(io_lib:format("~p~n", [Contents]))}
	| tagformat(Name, Contents)];
tagformat(_) -> %% no tag
	[].

tagformat(_, none) ->
	[];

tagformat('defineBitsJPEG2', Contents) ->
	{value, {jpegData, Data}} = lists:keysearch(jpegData, 1, Contents),
	[{".jpg", "image/jpeg", Data}];

tagformat('doABC', Contents) ->
	{value, {data, Data}} = lists:keysearch(data, 1, Contents),
	Dump = case os:find_executable("abcdump.exe") of
		false ->
			[];
		AbcDump ->
			TmpFilename = "/tmp/ssa-tmp." ++ os:getpid(),
			file:write_file(TmpFilename, Data),
			Out = os:cmd(AbcDump ++ " " ++ TmpFilename),
			file:delete(TmpFilename),
			[{".abc.asm", "text/plain", Out}]
	end,
	[{".abc", "application/octet-stream", Data}|Dump];

tagformat('doABC1', Contents) ->
	tagformat('doABC', Contents);

tagformat('doInitAction', Contents) ->
	{value, {actions, Data}} = lists:keysearch(actions, 1, Contents),
	[{".pseudo.asm", "text/plain", formatactions(Data)}];

tagformat('doAction', Contents) ->
	tagformat('doInitAction', Contents);
	
%tagformat('defineBitsJPEG3', Contents) -> %% broken jpegs??
%	tagformat('defineBitsJPEG2', Contents);

	
tagformat(_OtherName, _Contents) ->
	[].

%%
%% format helpers
%%

formatactions(Data) ->
	OutList = lists:foldr(fun({Name, Content}, Acc) ->
			{value, {code, Code}} = lists:keysearch(code, 1, Content),
			{value, {pos, Pos}} = lists:keysearch(pos, 1, Content),
			Line = lists:flatten(io_lib:format("<~8.10.0B> [~2.16.0B] ~s", [Pos, Code, Name])),
			Params = lists:keydelete(pos, 1, lists:keydelete(code, 1, Content)),
			Combine = fun(L, []) -> L; %% no params
					(L, E) -> %% one or more params
						lists:flatten(io_lib:format("~-31.. s~p", [L, E]))
					end,
			[Combine(Line, Params)|Acc]
		end, [], Data),
	lists:flatmap(fun(L) -> L ++ "\n"
		end, OutList).
	
