-module(ngram_SUITE).
-compile(export_all).

all() -> [ngram_n_lt_l, ngram, incr_pl, ngramfold, cut_profile, intersect_profiles, simplified_distance, merge_profiles].


ngram_n_lt_l(_Config) ->
	[] = ngram:ngram(4, [], [a,b,c]),
	ok.

ngram(_) ->
	[{[a,b],2}, {[b,a],1}] = ngram:ngram(2, [], [a,b,a,b]), ok.

incr_pl(_) ->
	[{a, 1}] = ngram:incr_pl(a, [], 1),
	[{a, 3}, {b,1}] = ngram:incr_pl(a, [{a,1}, {b,1}], 2), ok.

ngramfold(_) ->
	[{[a], 3}] = ngram:ngramfold(1, [[a,a],[a]]).

cut_profile(_) ->
	[{d,23}, {b,4}, {c,2}] = ngram:cut_profile(3, [{a,1}, {b,4}, {c,2}, {d,23}]), ok.

intersect_profiles(_) ->
	{[{b,2}],[{b,1}]} = ngram:intersect_profiles([{a,1}, {b,2}, {c,3}], [{d,4}, {b,1}]), ok.

simplified_distance(_) ->
	{1, 1.0} = ngram:simplified_distance([{a,1}, {b,3}, {c,3}], [{d,4}, {b,1}]), ok.

merge_profiles(_) ->
	[{b,4},{d,4},{a,1},{c,3}] = ngram:merge_profiles([{a,1}, {b,3}, {c,3}], [{d,4}, {b,1}]), ok.