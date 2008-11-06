-module(swfjson).
-compile(export_all).
-include("swf.hrl").
-include("swfdt.hrl").

-define(record_obj(RName, Record), record_obj(RName, Record, record_info(fields, RName))).


obj(#swf{}=X) -> ?record_obj(swf, X);
obj(#swfheader{type=Type, version=Version, filelength=FileLength, framesize=FrameSize, framerate=FrameRate, framecount=FrameCount, headersize=HeaderSize, rawheader=_}) ->
	{obj, [
		{cn, swfheader},
		{type, Type},
		{version, Version},
		{filelength, FileLength},
		{framesize, obj(FrameSize)},
		{framerate, FrameRate},
		{framecount, FrameCount},
		{headersize, HeaderSize}]};

obj(#tag{code=Code, name=Name, pos=Pos, raw=_, contents=Contents}) ->
	{obj, [{cn, tag}, {code, Code}, {tagname, Name}, {pos, Pos}]};

obj(#rect{}=X) -> ?record_obj(rect, X);

obj(X) when is_integer(X) -> X;
%obj([]) -> [];
obj(X) when is_list(X) -> [obj(A) || A <- X];

% obj([T|_]=X) when is_tuple(T) -> {obj, [obj(A) || A <- X]}; %% todo

obj(_) -> not_implemented.


record_obj(RName, Record, Fields) when element(1, Record) =:= RName ->
	record_obj(RName, 2, Record, Fields, []).
record_obj(RName, _, _, [], Acc) ->
	{obj, lists:reverse([{cn, RName}|Acc])};
record_obj(RName, I, Record, [FieldName|R], Acc) ->
	Element = element(I, Record),
	Obj = obj(Element),
	record_obj(RName, I+1, Record, R, [{FieldName, Obj}|Acc]).
