%%
%% @doc functions for n-gram analysis
%%
-module(ngram).
-export([
	ngram/2, ngram/3,
	ngramfold/2, ngramfold/3,
	cut_profile/2,
	intersect_profiles/2,
	simplified_distance/2,
	save_profile/2,
	load_profile/1
	]).
-include("ngram.hrl").


%% @doc generate n-gram profile
%% From a list L of elements count each N successive elements. PL is the accumulator / starting point. L with length(L) &lt; N are being silently ignored.
%% @spec ngram(integer(), profile(), list()) -> profile()
ngram(N, PL, L) when length(L) < N ->
	PL;
ngram(N, PL, [_|R]=L) ->
	{Key,_} = lists:split(N, L),
	PL2 = incr_pl(Key, PL),
	% io:format("~p ~p ~p~n", [length(PL2), Key, length(L)]),
	ngram(N, PL2, R).

%% @doc same as ngram(N, [], L)
ngram(N, L) -> ngram(N, [], L).

%% @doc increase integer value of property list by one
incr_pl(Key, PL) ->
	Val = proplists:get_value(Key, PL, 0),
	PL2 = proplists:delete(Key, PL),
	P = proplists:property(Key, Val+1),
	[P|PL2].


%% @doc generate n-gram from a list of lists
ngramfold(N, LL) -> ngramfold(N, [], LL).
ngramfold(N, PL0, LL) ->
	% io:format("L=~p~n", [length(LL)]),
	lists:foldl(fun(L, PL) ->
		ngram(N, PL, L)
	end, PL0, LL).

%% @doc limit n-gram profile to the Length
cut_profile(Length, P) when length(P) < Length ->
	P;
cut_profile(Length, P) ->
	{SP, _} = lists:split(Length, lists:reverse(lists:keysort(2, P))),
	SP.

%% @doc calculate P1 cap P2 / all common keys of two profiles
%% the resulting profiles are of equal length
%% @spec intersect_profiles(profile(), profile()) -> {profile(), profile()}
intersect_profiles(P1, P2) ->
	P1out = lists:filter(fun({K,_}) -> lists:keymember(K, 1, P2) end, P1),
	P2out = lists:filter(fun({K,_}) -> lists:keymember(K, 1, P1out) end, P2),
	{P1out, P2out}.

%% @doc simplified profile intersection (SPI)
%% @spec simplified_distance(profile(), profile()) -> {IntersectionLength::integer(), Distance::float()}
simplified_distance(P1, P2) ->
	{P1s, P2s} = intersect_profiles(P1, P2),
	Ndist = lists:map(fun({K, V1}) ->
		{value, {K, V2}} = lists:keysearch(K, 1, P2s),
		X = (2 * (V1-V2)) / (V1 + V2),
		X*X
	end, P1s),
	{length(P1s), lists:sum(Ndist)}.


%% incomplete

save_profile(Filename, P) ->
	file:write_file(Filename, term_to_binary(P)).
load_profile(Filename) ->
	{ok, B} = file:read_file(Filename),
	binary_to_term(B).

