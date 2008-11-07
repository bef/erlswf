-module(swfdt).
-compile(export_all).
-include("swfdt.hrl").

-export([
	ui16/1,
	sb/1, sb/2,
	encodedu32/1,
	string/1,
	rect/1,
	matrix/1
	]).

%%
%% primitive datatypes
%%

%% @doc decode 16-bit unsigned integer
%% @spec ui16(binary()) -> {integer(), binary()}
ui16(<<X:16/unsigned-integer-little, Rest/binary>>) ->
	{X, Rest}.

%% @doc decode signed-bit value (aka. sign extension)
%% @spec sb(bitstring()) -> integer()
sb(<<>>) -> <<>>; %% empty value (this should rarely occur)
sb(<<0:1, X/bitstring>>) -> %% positive value
	Size = bit_size(X),
	<<Value:Size/unsigned-integer-big>> = X,
	Value;
sb(<<1:1, X/bitstring>>) -> %% negative value
	Size = bit_size(X),
	<<Value:64/signed-integer-big>> = <<16#ffffffffffffffff:(64-Size), X/bitstring>>,
	Value.

sb(Bitstring, Count) ->
	<<B:Count/bitstring, Rest/bitstring>> = Bitstring,
	{sb(B), Rest}.

%% @doc decode 1-5 bit variable length u32
%% @spec encodedu32(binary()) -> {int(), binary()}
encodedu32(B) ->
	encodedu32(B, 0, <<>>).
encodedu32(<<0:1, X:7/bitstring, Rest/binary>>, _Count, Acc) ->
	BitValue = <<X/bitstring, Acc/bitstring>>,
	Size = bit_size(BitValue),
	<<Value35:Size/unsigned-integer-big>> = BitValue,
	Value = Value35 band (1 bsl 32 - 1), %% 35bit -> 32bit
	{Value, Rest};
encodedu32(<<1:1, X:7/bitstring, Rest/binary>>, Count, Acc) when Count < 4 ->
	encodedu32(Rest, Count + 1, <<X/bitstring, Acc/bitstring>>).

%% @doc null-terminated string
%% @spec string(binary()) -> {string(), binary()}
string(S) -> string(S, []).
string(<<>>, Acc) -> string(<<0>>, Acc); %% not nerminated by 0 ?
string(<<0,R/binary>>, Acc) -> {list_to_binary(lists:reverse(Acc)), R};
string(<<A,R/binary>>, Acc) -> string(R, [A|Acc]).


%%
%% meta types and helpers
%%

condfun(0, _Fun, B) -> {none, B};
condfun(1, Fun, B) -> Fun(B).

%% @doc traverse B Count times using Fun
%% @spec metaarray(any(), integer(), fun()) -> {[any()], any()}
metaarray(B, Count, Fun) ->
	metaarray(B, Count, Fun, []).
metaarray(B, 0, _Fun, Acc) ->
	{lists:reverse(Acc), B};
metaarray(B, Count, Fun, Acc) ->
	{Element, Rest} = Fun(B),
	metaarray(Rest, Count - 1, Fun, [Element|Acc]).

%% @doc align bits to a byte by cutting off bits at the beginning
%% @spec bytealign(bitstring()) -> binary()
bytealign(Bitstring) ->
	Padding = bit_size(Bitstring) rem 8,
	<<_:Padding, B/binary>> = Bitstring,
	B.



%%
%% data types
%%

rect(<<S:5/unsigned-integer,
		Xmin:S/integer-signed-big,
		Xmax:S/integer-signed-big,
		Ymin:S/integer-signed-big,
		Ymax:S/integer-signed-big,
		B/bitstring>>) ->
	{#rect{xmin=Xmin, xmax=Xmax, ymin=Ymin, ymax=Ymax}, bytealign(B)}.


%% @doc decode matrix into scale/rotate/translate tuples (bytealigned)
%% @spec matrix(binary()) -> {sbtuple(), sbtuple(), sbtuple()}
%%   where sbtuple() = {float(), float()}
matrix(X) ->
	{Scale, R1} = matrixsr(X),
	{Rotate, R2} = matrixsr(R1),
	{Translate, R3} = matrixtranslate(R2),
	{{Scale, Rotate, Translate}, bytealign(R3)}.
matrixsr(<<0:1, B/bitstring>>) -> {{}, B}; %% scale/rotate
matrixsr(<<1:1, N:5, A:N/bitstring, B:N/bitstring, R/bitstring>>) ->
	{{sb(A), sb(B)}, R}.
matrixtranslate(<<N:5, A:N/bitstring, B:N/bitstring, R/bitstring>>) ->
	{{sb(A), sb(B)}, R}.


assets(B) ->
	assets(B, []).
assets(<<>>, Acc) -> lists:reverse(Acc);
assets(<<CharID:16/unsigned-integer-little, B/binary>>, Acc) ->
	{S, R} = string(B),
	assets(R, [{CharID, S}|Acc]).

stringpool(B) ->
	stringpool(B, []).
stringpool(<<>>, Acc) ->
	lists:reverse(Acc);
stringpool(B, Acc) ->
	{S, R} = string(B),
	stringpool(R, [S|Acc]).


cstringpool(B, Count) ->
	metaarray(B, Count, fun(X) -> {S, R} = string(X), {S, R} end).
	

registerparampool(B, Count) ->
	metaarray(B, Count, fun(<<Register, X/binary>>) ->
		{S, R} = string(X),
		{{Register, S}, R}
		end).

soundinfoenveloperecords(B, Count) ->
	metaarray(B, Count, fun(<<Pos44:32/unsigned-integer-little,
			LeftLevel:16/unsigned-integer-little,
			RightLevel:16/unsigned-integer-little, R/binary>>) ->
		{{Pos44, LeftLevel, RightLevel}, R}
		end).
	
soundinfo(<<_Reserved:2, %% always 0
		SyncStop:1,
		SyncNoMultiple:1,
		HasEnvelope:1,
		HasLoops:1,
		HasOutPoint:1,
		HasInPoint:1,
		B/binary>>) ->
	InPointSize = 32 * HasInPoint,
	<<InPoint:InPointSize/unsigned-integer-little, R1/binary>> = B,
	OutPointSize = 32 * HasOutPoint,
	<<OutPoint:OutPointSize/unsigned-integer-little, R2/binary>> = R1,
	LoopsSize = 16 * HasLoops,
	<<LoopCount:LoopsSize/unsigned-integer-little, R3/binary>> = R2,
	EnvPointsSize = 8 * HasEnvelope,
	<<EnvPoints:EnvPointsSize, R4/binary>> = R3,
	{ER, _} = soundinfoenveloperecords(R4, EnvPoints),
	{soundinfo, [
		{syncStop, SyncStop},
		{syncNoMultiple, SyncNoMultiple},
		{inPoint, InPoint},
		{outPoint, OutPoint},
		{loopCount, LoopCount},
		{envelopeRecords, ER}]}.


stylearray(B, Fun) ->
	{SCount, SBin} = case B of
		<<16#ff, Count:16/unsigned-integer-little, SB/binary>> ->
			{Count, SB};
		<<Count, SB/binary>> ->
			{Count, SB}
	end,
	metaarray(SBin, SCount, Fun).


fillstylearray(Type, Bin) ->
	Fun = fun(<<0, R, G, B, B1/binary>>) -> %% solid fill
			{{rgb, R, G, B}, B1};
		(<<FSType, B1/binary>>) when FSType =:= 16#10; FSType =:= 16#12 -> %% linear/radial gradient fill
			{Matrix, B2} = matrix(B1),
			{Gradient, B3} = gradient(Type, B2),
			{{Matrix, Gradient}, B3};
		(<<FSType, B1/binary>>) when FSType =:= 16#13 -> %% focal radial gradient fill
			{FG, B2} = focalgradient(Type, B1),
			{FG, B2};
		(<<FSType, BitmapID:16/unsigned-integer-little, B1/binary>>) when FSType >= 16#40, FSType =< 16#43 -> %% bitmap fill
			{Matrix, B2} = matrix(B1),
			{{BitmapID, Matrix}, B2}%;
		%(<<FSType, Foo/binary>>) ->
		%	debug("~p, nonesense~n", [FSType]),
		%	{FSType, nonesense, Foo}
		end,
	stylearray(Bin, Fun).

gradient(Type, <<SpreadMode:2, InterpolationMode:2, NumGradients:4, B/binary>>) ->
	{GradRecords, R} = gradrecords(Type, B, NumGradients),
	{{SpreadMode, InterpolationMode, GradRecords}, R}.

gradrecords(Type, Bin, Count) ->
	Fun = case Type of
		shape1 -> fun(<<Ratio, R, G, B, Rest/binary>>) -> {{ratio_rgb, {Ratio, R, G, B}}, Rest} end
	end,
	metaarray(Bin, Count, Fun).

focalgradient(Type, B) ->
	{gradient, {S, I, G}, <<FocalPoint, R/binary>>} = gradient(Type, B),
	{focalgradient, {S, I, G, FocalPoint}, R}.

linestylearray(Type, Bin) ->
	LineStyleRGB = fun(<<Width:16/unsigned-integer-little, R, G, B, Rest/binary>>) -> {{Width, {rgb, R, G, B}}, Rest} end,
	Fun = case Type of
		shape1 -> LineStyleRGB
	end,
	stylearray(Bin, Fun).

buttonrecord(<<0:2, %% reserved
		_HasBlendMode:1,
		_HasFilterList:1,
		StateHitTest:1,
		StateDown:1,
		StateOver:1,
		StateUp:1,
		CharacterID:16/unsigned-integer-little,
		PlaceDepth:16/unsigned-integer-little,
		B/binary>>) ->
	{PlaceMatrix, Rest} = matrix(B),
	{buttonrecord, [{stateHitTest, StateHitTest},
		{stateDown, StateDown},
		{stateOver, StateOver},
		{stateUp, StateUp},
		{characterID, CharacterID},
		{placeDepth, PlaceDepth},
		{placeMatrix, PlaceMatrix}], Rest}.

%buttonrecord2(<<0:2,
%	HasBlendMode:1,
%	HasFilterList:1,
%	_/bitstring>> = B) ->
%	{buttonrecord, BR, R1} = buttonrecord(B),
%	{cxformwithalpha, CX, R2} = cxformwithalpha(R1),
%	{filterlist, FL, R3} = filterlistcond(HasFilterList, R2),
%	{blendmode, BM, R4} = case HasBlendMode of
%		0 -> {blendmode, none, R3};
%		1 -> <<BlendMode, R5/binary>> = R3, {blendmode, BlendMode, R5}
%	end,
%	{buttonrecord, [{cxFormWithAlpha, CX}, {filterList, FL}, {blendMode, BM}|BR], R4}.


records(B, Fun) ->
	records(B, Fun, []).
records(<<0, R/binary>>, _Fun, Acc) ->
	{lists:reverse(Acc), R};
records(<<>>, _Fun, Acc) -> %% unconventional end of block
	{lists:reverse(Acc), <<>>};
records(B, Fun, Acc) ->
	{BR, Rest} = Fun(B),
	records(Rest, [BR|Acc]).

buttonrecords(B) ->
	records(B, fun(X) ->
		{buttonrecord, BR, Rest} = buttonrecord(X),
		{BR, Rest}
		end).

%buttonrecords2(B) ->
%	records(B, fun(X) ->
%		{buttonrecord, BR, Rest} = buttonrecord2(X),
%		{BR, Rest}
%		end).
%

cxform(<<HasAddTerms:1, HasMultTerms:1, N:4, BS/bitstring>>) ->
	{MT, BS1} = case HasMultTerms of
		0 -> {#cxform{}, BS};
		1 -> <<R:N/bitstring, G:N/bitstring, B:N/bitstring, R1/bitstring>> = BS, {#cxform{redmult=sb(R), greenmult=sb(G), bluemult=sb(B)}, R1}
	end,
	{AT, BS2} = case HasAddTerms of
		0 -> {MT, BS1};
		1 -> <<Ra:N/bitstring, Ga:N/bitstring, Ba:N/bitstring, R2/bitstring>> = BS1, {MT#cxform{redadd=sb(Ra), greenadd=sb(Ga), blueadd=sb(Ba)}, R2}
	end,
	{AT, bytealign(BS2)}.

cxformwithalpha(<<HasAddTerms:1, HasMultTerms:1, N:4, BS/bitstring>>) ->
	{MT, BS1} = case HasMultTerms of
		0 -> {#cxformwa{}, BS};
		1 -> <<R:N/bitstring, G:N/bitstring, B:N/bitstring, A:N/bitstring, R1/bitstring>> = BS, {#cxformwa{redmult=sb(R), greenmult=sb(G), bluemult=sb(B), alphamult=sb(A)}, R1}
	end,
	{AT, BS2} = case HasAddTerms of
		0 -> {MT, BS1};
		1 -> <<Ra:N/bitstring, Ga:N/bitstring, Ba:N/bitstring, Aa:N/bitstring, R2/bitstring>> = BS1, {MT#cxformwa{redadd=sb(Ra), greenadd=sb(Ga), blueadd=sb(Ba), alphaadd=sb(Aa)}, R2}
	end,
	{AT, bytealign(BS2)}.


%filterlistcond(0, B) -> {filterlist, none, B};
%filterlistcond(1, B) -> filterlist(B).
%
%filterlist(<<N, B/binary>>) ->
%	metaarray(B, N, fun filter/1).
%
%filter(<<0,
%		R, G, B, A,
%		BlurX:32/bitstring,
%		BlurY:32/bitstring,
%		Angle:32/bitstring,
%		Distance:32/bitstring,
%		Strength:16/bitstring,
%		InnerShadow:1,
%		Knockout:1,
%		CompositeSource:1,
%		Passes:5,
%		Rest/binary>>) ->
%	{{dropShadowFilter,
%		[{id, 0},
%		{dropShadowColor, {rgba, R, G, B, A}},
%		{blurX, BlurX},
%		{blurY, BlurY},
%		{angle, Angle},
%		{distance, Distance},
%		{strength, Strength},
%		{innerShadow, InnerShadow},
%		{knockout, Knockout},
%		{compositeSource, CompositeSource},
%		{passes, Passes}]},
%		Rest};
%filter(<<Id, Rest/binary>>) ->
%	{{unknownFilter, [{id, Id}]}, Rest}.



%style(<<NumFillBits:4, NumLineBits, B/binary>>) ->
%	{foo, B}.
%shaperecords(B) ->
%	stylerecords(B, []).
%shaperecords(<<0:1, 0:5, _:2, Rest/binary>>, Acc) -> %% end shape record
%	{lists:reverse(Acc), Rest};
%shaperecords(<<>>, Acc) -> %% abrupt end of stylerecords
%	{lists:reverse(Acc), <<>>};
%shaperecords(B, Acc) ->
%	{SR, R} = stylerecord(B),
%	stylerecords(R, [SR|Acc]).
%shaperecord(<<0:1, %% type flag
%		StateNewStyles:1,
%		StateLineStyle:1,
%		StateFillStyle1:1,
%		StateFillStyle0:1,
%		StateMoveTo:1,
%		B/binary>>) -> %% change shape record
%	{Move, R1} = case StateMoveTo of
%		1 ->
%			<<N:5, DX:N/bitstring, DY:N/bitstring, R/bitstring>> = B,
%			{{DX, DY}, R};
%		0 -> {none, B}
%	end,
%	{FS0, R2} = case StateFillStyle0 of
%		1 ->
%			<<...>>,
%	{foo, B}.


%textrecords(Params, B) ->
%	textrecords(Params, B, []).
%textrecords(_Params, <<0:1,_:7, B/binary>>, Acc) ->
%	{textrecords, lists:reverse(Acc), B};
%textrecords({GB, AB, IsDefineText2}, B, Acc) ->
%	{textrecord, TR, R} = textrecord(B, IsDefineText2, GB, AB),
%	textrecords(GB, AB, R, [TR|Acc]).
%
%textrecordfield(fontID, 1, <<FontID:16/unsigned-integer-little, B/binary>>) -> {FontID, B};
%textrecordfield({textColor, 0}, 1, B) ->
%	{rgb, RGB, Rest} = rgb(B),
%	{{rgb, RGB}, Rest};
%textrecordfield({textColor, 1}, 1, B) ->
%	{argb, ARGB, Rest} = argb(B),
%	{{argb, ARGB}, Rest};
%textrecordfield(offset, 1, <<Offset:16/signed-integer-little, B/binary>>) -> {Offset, B};
%textrecordfield(offset, 0, B) -> {0, B};
%textrecordfield(textHeight, 1, <<Height:16/unsigned-integer-little, B/binary>>) -> {Height, B};
%textrecordfield(_Name, 0 = _Defined, B) -> {none, B}.
%
%textrecord(<<_TextRecordType:1, %% always 1
%		_StyleFlagsReserved:3, %% always 0
%		StyleFlagsHasFont:1,
%		StyleFlagsHasColor:1,
%		StyleFlagsHasXOffset:1,
%		StyleFlagsHasYOffset:1,
%		B/binary>>, IsDefineText2, GlyphBits, AdvanceBits) ->
%	{FontID, R1} = textrecordfield(fontID, StyleFlagsHasFont, B),
%	{TextColor, R2} = textrecordfield({textColor, IsDefineText2}, StyleFlagsHasColor, R1),
%	{XOffset, R3} = textrecordfield(offset, StyleFlagsHasXOffset, R2),
%	{YOffset, R4} = textrecordfield(offset, StyleFlagsHasYOffset, R3),
%	{TextHeight, <<GlyphCount, R5/binary>>} = textrecordfield(textHeight, StyleFlagsHasFont, R4),
%	glyphs, %% ????
%	{textrecord, [
%		{fontID, FontID},
%		{textColor, TextColor},
%		{xOffset, XOffset},
%		{yOffset, YOffset},
%		{textHeight, TextHeight}], Rest}.


clipactions(<<0:16, %% reserved
		B/binary>>) ->
	{unimplemented, B}.

rgb(<<R, G, B, Rest/binary>>) -> {{rgb, R, G, B}, Rest}.
rgba(<<R, G, B, A, Rest/binary>>) -> {{rgba, R, G, B, A}, Rest}.
argb(<<A, R, G, B, Rest/binary>>) -> {{argb, A, R, G, B}, Rest}.
