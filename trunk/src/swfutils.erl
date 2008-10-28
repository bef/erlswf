-module(swfutils).
-export([
	dumpswf/1,
	dumpswftags/1,
	filedumpswf/2,
	dumptags/2,
	dumpsecuritycheck/1,
	silentsecuritycheck/1,
	filtertags/2,
	savetofile/3,
	tagencode/2,
	abcdata/1]).

-include("swf.hrl").


%%
%% utils
%%

dumpswf({swf, Header, Tags}) ->
	io:format("~n-----------------------------------~n", []),
	io:format("** header: ~p~n", [Header]),
	dumpswftags(Tags).

dumpswftags([]) -> done;
dumpswftags([{tag, Code, Name, Pos, _Raw, Contents}|Rest]) ->
	io:format("** <~8.10.0B> [~3.10.0B] ~p : ~p~n", [Pos, Code, Name, Contents]),
	dumpswftags(Rest);
dumpswftags([{rawtag, Code, Name, Pos, _Raw, _}|Rest]) ->
	io:format("** <~8.10.0B> [~3.10.0B] ~p : (not decoded)~n", [Pos, Code, Name]),
	dumpswftags(Rest).


%savetag21({swf, _Header, Tags}, Prefix) ->
%	JpegTags = findtags([defineBitsJPEG2], Tags),
%	savetag21tofile(JpegTags, Prefix).
%savetag21tofile([], _Prefix) -> done;
%savetag21tofile([{_Name, Contents, _Raw}|Rest], Prefix) ->
%	{value, {characterID, CharacterID}} = lists:keysearch(characterID, 1, Contents),
%	{value, {jpegData, Data}} = lists:keysearch(jpegData, 1, Contents),
%	
%	savetofile("~s~p.jpg", [Prefix, CharacterID], Data),
%	
%	savetag21tofile(Rest, Prefix).


filedumpswf({swf, Header, Tags}, Prefix) ->
	%% dump header
	savetofile("~s-header.erl", [Prefix], list_to_binary(io_lib:format("~p~n", [Header]))),
	
	%% dump tags
	lists:foldl(fun(Tag, Acc) ->
		lists:foreach(fun({Postfix, _MimeType, B}) ->
			savetofile("~s-tag~5.10.0B~s", [Prefix, Acc, Postfix], B)
			end, swfformat:tagformat(Tag)),
		Acc + 1 end, 1, Tags).

%rmut({swf, Header, Tags}, Outfile) -> %% remove unknown tags
%	AllKnownTags = lists:foldl(fun({Name, Contents, Raw}, Acc) ->
%			KnownTag = case Name of
%				unknownTag -> <<>>;
%				_ ->
%					{value, {code, Code}} = lists:keysearch(code, 1, Contents),
%					tagencode(Code, Raw)
%			end,
%			<<Acc/binary, KnownTag/binary>>
%		end, <<>>, Tags),
%		
%	{value, {rawheader, RawHeader}} = lists:keysearch(rawheader, 1, Header),
%	{value, {version, Version}} = lists:keysearch(version, 1, Header),
%	
%	Body = zlib:compress(<<RawHeader/binary, AllKnownTags/binary>>),
%	%Body = <<RawHeader/binary, AllKnownTags/binary>>,
%	FileLength = size(Body),
%	savetofile("~s", [Outfile], <<"CWS", Version, FileLength:32/unsigned-integer-little, Body/binary>>).

dumptags({swf, _Header, RawTags}, Tagnames) ->
	dumpswftags([swf:tagdecode(Tag) || Tag <- filtertags(Tagnames, RawTags)]).



getactions(doAction, TagContents) ->
	{value, {actions, Actions}} = lists:keysearch(actions, 1, TagContents),
	Actions;
getactions(doInitAction, TC) ->
	getactions(doAction, TC).

checkactions([]) -> ok;
checkactions(Actions) -> %% >= 1 element
	[{_, LastActionArgs}|_] = lists:reverse(Actions), %% assumption: min 1 element available
	{value, {pos, LastActionPos}} = lists:keysearch(pos, 1, LastActionArgs),
	
	case lists:keysearch(unknownAction, 1, Actions) of
		false -> ok;
		{value, X} -> throw(X)
	end,
	
	Branches = lists:filter(fun({Op, _Args}) -> lists:member(Op, ['jump', 'if']) end, Actions),
	lists:foreach(fun({_, Args}) ->
		{value, {branchOffset, BO}} = lists:keysearch(branchOffset, 1, Args),
		{value, {pos, Pos}} = lists:keysearch(pos, 1, Args),
		Report = fun(Cond) ->
			case Cond of
				true -> throw({branchOOB, Pos});
				false -> ok
			end
		end,
		Report(BO + Pos < 0),
		Report(BO + Pos > LastActionPos) 
		end, Branches),
	ok.
	
has_invalid_actions_t(Tags) ->
	ActiveTags = filtertags(['doInitAction', 'doAction'], Tags),

	lists:foreach(fun({tag, _Code, Name, _Pos, _Raw, Contents}) ->
		Actions = getactions(Name, Contents),
		checkactions(Actions)
		end, ActiveTags),
	ok.

has_invalid_actions(Tags) ->
	try has_invalid_actions_t(Tags) of
		_ -> false
	catch
		throw:X -> {true, X}
	end.

dumpsecuritycheck(#swf{tags=Tags}) ->
	T1 = case lists:keysearch(unknownTag, 3, Tags) of
		false -> ok;
		{value, X} -> io:format("	contains unknown tag: ~p~n", [X])
	end,
	
	T2 = case has_invalid_actions(Tags) of
		false -> ok;
		{true, Why} -> io:format("	contains invalid action: ~p~n", [Why])
	end,
	
	(T1 =:= T2) =:= ok.

silentsecuritycheck(#swf{tags=Tags}) ->
	case lists:keysearch(unknownTag, 3, Tags) of
		false -> ok;
		{value, X} -> throw({not_ok, X})
	end,

	case has_invalid_actions(Tags) of
		false -> ok;
		{true, Why} -> throw({not_ok, Why})
	end,

	ok.

%%
%% helpers
%%
filtertags(Names, Tags) ->
	lists:filter(fun(#tag{name=Name}) -> lists:member(Name, Names) end, Tags).


savetofile(Fmt, Args, Data) ->
	Outfilename = lists:flatten(io_lib:format(Fmt, Args)),
	io:format("saving ~p~n", [Outfilename]),
	file:write_file(Outfilename, Data).

%% encode tag
tagencode(Code, B) ->
	BSize = size(B),
	TagAndLength = case BSize >= 16#3f of
		true ->
			<<A, X>> = <<Code:10, 16#3f:6>>,
			<<X, A, BSize:32/signed-integer-little>>;
		false ->
			<<A, X>> = <<Code:10, BSize:6>>,
			<<X, A>>
	end,
	<<TagAndLength/binary, B/binary>>.


abcdata(#swf{tags=RawTags}) ->
	AbcTags = [swf:tagdecode(Tag) || Tag <- filtertags(['doABC', 'doABC1'], RawTags)],
	lists:map(fun(#tag{contents=Contents}) ->
		{value, {data, Abc}} = lists:keysearch(data, 1, Contents),
		Abc
	end, AbcTags).

