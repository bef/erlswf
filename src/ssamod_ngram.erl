-module(ssamod_ngram).
-export([run/1]).
-include("ngram.hrl").

-commands([
	{"ngram", [
		{usage, "ngram <subcmd> <args> "},
		{shorthelp, "statistical classification"},
		{longhelp, "commands:
	new <profile> <name> <type> <n> <l>
		create new profile with given parameters, e.g. ngram new foo.profile foo abc 3 200
	
	debug dump <profile>
		debug-dump profile
	
	debug actions <swf>
		debug list actions
	
	add <profile> <swf|abc>...
		populate profile -- training
	
	distance <profile>... -- <swf|abc>..."}]}
	]).


run(["ngram", "new", Filename, Name, Type, N, L]) ->
	ngram:save_profile(Filename,
		#ngramprofile{
			name=Name,
			type=list_to_atom(Type),
			n=list_to_integer(N),
			l=list_to_integer(L),
			data=[]
		}),
	ok;

run(["ngram", "debug", "actions", Filename]) ->
	io:format("~p~n", [getactionoplist(Filename)]), ok;

run(["ngram", "debug", "dump", Filename]) ->
	P = ngram:load_profile(Filename),
	io:format("~p~n", [P]),
	ok;

run(["ngram", "add", ProfileFilename | Filenames]) ->
	%% load profile
	P1 = ngram:load_profile(ProfileFilename),
	N = P1#ngramprofile.n,
	L = P1#ngramprofile.l,
	io:format("profile ~s: n=~p l=~p~n", [P1#ngramprofile.name, N, L]),
	
	Profile = lists:foldl(fun(F, PL) ->
		calcprofile(F, PL, N, L, P1#ngramprofile.type)
	end, P1#ngramprofile.data, Filenames),
	
	%% save profile
	ngram:save_profile(ProfileFilename, P1#ngramprofile{data=Profile}),
	ok;

run(["ngram", "distance" | FL]) ->
	{PLs, ["--"| FNs]} = lists:splitwith(fun(A) -> A =/= "--" end, FL),
	AProfiles = lists:map(fun(Fn) -> ngram:load_profile(Fn) end, PLs),
	
	[P1|_] = AProfiles,
	N = P1#ngramprofile.n,
	% L = P1#ngramprofile.l,
	
	PProfiles = lists:map(fun(Fn) ->
		{Fn, calcprofile(Fn, [], N, 20000, P1#ngramprofile.type)}
	end, FNs),
	
	lists:foreach(fun({Fn, PP}) ->
		Dists = lists:map(fun(#ngramprofile{data=AP, name=APname}) ->
			Dist = ngram:simplified_distance(AP, PP),
			{APname, Dist}
		end, AProfiles),
		
		io:format("dists for ~p~n", [Fn]),
		lists:foreach(fun({Name, {Intersect, Dist}}) -> io:format("  profile ~s: intersect=~p, distance=~p~n", [Name, Intersect, Dist]) end, Dists)
	end, PProfiles),
	ok;

run(_) -> invalid_usage.


calcprofile(F, P0, N, L, Type) ->
	io:format("processing file ~p~n", [F]),
	LL = case Type of
		abc -> getabcoplist(F);
		action -> getactionoplist(F);
		T -> throw({unknown_profile_type, T})
	end,
	
	%% print stats
	NumFuncs = length(LL),
	NumOps = lists:sum([length(X) || X <- LL]),
	io:format("  ~p functions with ~p opcodes in total~n", [NumFuncs, NumOps]),
	
	%% calculate profile
	NewProfile = ngram:ngramfold(N, P0, LL),
	ngram:cut_profile(L, NewProfile).
	

getabcoplist(Filename) ->
	Bin = swf:readfile(Filename),
	case swfmime:getmime(Bin) of
		swf ->
			RawSwf = swf:parsetorawtags(Bin),
			AbcList = swfutils:abcdata(RawSwf),
			LLs = lists:map(fun(B) ->
				{Abc,_} = swfabc:abc(B),
				swfutils:abc2oplist(Abc)
			end, AbcList),
			lists:append(LLs);
		abc ->
			{Abc,_} = swfabc:abc(Bin),
			Abc;
		_ ->
			io:format("unknown fileformat~n"), []
	end.

getactionoplist(Filename) ->
	Bin = swf:readfile(Filename),
	RawSwf = swf:parsetorawtags(Bin),
	ActionL = swfutils:actiondata(RawSwf),
	lists:map(fun(AL) ->
		swfutils:actions2oplist(AL)
	end, ActionL).
	
