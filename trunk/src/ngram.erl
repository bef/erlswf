%%
%% @doc functions for n-gram analysis
%%
-module(ngram).
-export([
	ngram/2, ngram/3,
	ngramfold/2, ngramfold/3,
	cut_profile/2,
	intersect_profiles/2,
	expand_profiles/2,
	distance/2, simplified_distance/2, common_distance/2,
	save_profile/2,
	load_profile/1,
	merge_profiles/2, merge_profiles/1,
	incr_pl/3
	]).
-include("ngram.hrl").


%% @doc generate n-gram profile
%% From a list L of elements count each N successive elements. PL is the accumulator / starting point. L with length(L) &lt; N are being silently ignored.
%% @spec ngram(integer(), profile(), list()) -> profile()
ngram(N, PL, L) when length(L) < N ->
	PL;
ngram(N, PL, [_|R]=L) ->
	{Key,_} = lists:split(N, L),
	PL2 = incr_pl(Key, PL, 1),
	% io:format("~p ~p ~p~n", [length(PL2), Key, length(L)]),
	ngram(N, PL2, R).

%% @doc same as ngram(N, [], L)
ngram(N, L) -> ngram(N, [], L).

%% @doc increase integer value of property list entry by I
incr_pl(Key, PL, I) ->
	Val = proplists:get_value(Key, PL, 0),
	PL2 = proplists:delete(Key, PL),
	P = proplists:property(Key, Val+I),
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

%% @doc calculate P1 cup P2 / add missing keys with value 0
%% the resulting profiles are of equal length
%% @spec expand_profiles(profile(), profile()) -> {profile(), profile()}
expand_profiles(P1, P2) ->
	P1a = [{K,0} || {K,_} <- lists:filter(fun({K,_}) -> not lists:keymember(K, 1, P2) end, P1)],
	P2a = [{K,0} || {K,_} <- lists:filter(fun({K,_}) -> not lists:keymember(K, 1, P1) end, P2)],
	{lists:append(P1, P2a), lists:append(P2, P1a)}.

%% @doc calculate distance between profiles.
%% profiles must be of equal length and contain the same keys
%% @spec distance(profile(), profile()) -> Distance::float()
distance(P1, P2) when length(P1) =:= length(P2) ->
	Ndist = lists:map(fun({K, V1}) ->
		{value, {K, V2}} = lists:keysearch(K, 1, P2),
		X = (2 * (V1-V2)) / (V1 + V2),
		X*X
	end, P1),
	lists:sum(Ndist).

%% @doc simplified profile intersection (SPI) distance.
%% intersect, then calculate distance
%% @spec simplified_distance(profile(), profile()) -> {IntersectionLength::integer(), Distance::float()}
simplified_distance(P1, P2) ->
	{P1a, P2a} = intersect_profiles(P1, P2),
	Ndist = distance(P1a, P2a),
	{length(P1a), Ndist}.

%% @doc common distance
%% expand profiles, then calculate distance
%% @spec common_distance(profile(), profile()) -> {IntersectionLength::integer(), Distance::float()}
common_distance(P1, P2) ->
	{P1a, P2a} = expand_profiles(P1, P2),
	Ndist = distance(P1a, P2a),
	{length(P1a), Ndist}.

%% @doc merge two profiles
%% @spec merge_profiles(profile(), profile()) -> profile()
merge_profiles(P1, P2) ->
	lists:foldl(fun({K, V}, Acc) ->
		incr_pl(K, Acc, V)
	end, P1, P2).

%% @doc merge many profiles
%% @spec merge_profiles([profile()]) -> profile()
merge_profiles([]) -> [];
merge_profiles([P1|L]) ->
	lists:foldl(fun(P, Acc) -> merge_profiles(Acc, P) end, P1, L).


save_profile(Filename, #ngramprofile{}=P) ->
	file:write_file(Filename, term_to_binary(P)).
load_profile(Filename) ->
	{ok, B} = file:read_file(Filename),
	binary_to_term(B).

