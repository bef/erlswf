-module(swfjsonabc).
-compile(export_all).
-include("swfabc.hrl").
-define(record_obj(RName, Record), record_obj(RName, Record, record_info(fields, RName))).


obj(X) when is_atom(X) -> X;
obj(X) when is_integer(X) -> X;
obj([]) -> [];
obj(X) when is_list(X) -> [obj(A) || A <- X];

obj(#abcfile{}=X) -> ?record_obj(abcfile, X);
obj(#cpool{}=X) -> ?record_obj(cpool, X);
obj(#method{}=X) -> ?record_obj(method, X);
obj(#instance{}=X) -> ?record_obj(instance, X);
obj(#class{}=X) -> ?record_obj(class, X);
obj(#script{}=X) -> ?record_obj(script, X);
obj(#method_body{}=X) -> ?record_obj(method_body, X);

obj(#trait{}=X) -> ?record_obj(trait, X);
obj(#instr{}=X) -> ?record_obj(instr, X);

obj({string, X}) when is_binary(X) -> X;
obj({string, X}) when is_list(X) -> list_to_binary(X);
obj({string, X}) when is_atom(X) -> atom_to_list(X);

obj({namespace, {Kind, Name}}) -> {obj, [{kind, obj(Kind)}, {name, obj(Name)}, {cn, namespace}]};

obj({keyvalue, {Key, Value}}) -> [Key, Value];

obj(_) ->
	not_implemented.

%% encode special record members
obj(abcfile, version, {X, Y}) -> [X, Y];
obj(abcfile, metadata, X) -> [obj({keyvalue, A}) || A <- X];

obj(cpool, integer, X) -> X;
obj(cpool, uinteger, X) -> X;
obj(cpool, double, X) -> X;
obj(cpool, string, X) -> X;
obj(cpool, namespace, X) -> [obj({namespace, A}) || A <- X];

obj(_, _, X) -> obj(X).

record_obj(RName, Record, Fields) when element(1, Record) =:= RName ->
	record_obj(RName, 2, Record, Fields, []).
record_obj(RName, _, _, [], Acc) ->
	{obj, lists:reverse([{cn, RName}|Acc])};
record_obj(RName, I, Record, [FieldName|R], Acc) ->
	Element = element(I, Record),
	Obj = obj(RName, FieldName, Element),
	record_obj(RName, I+1, Record, R, [{FieldName, Obj}|Acc]).
