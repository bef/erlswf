%%
%% @doc parse abc (avm2 bytecode)
%%
-module(swfabc).
-export([abc/1, method/2, method_body/2,
	cpinteger/2, cpuinteger/2, cpdouble/2, cpstring/2, cpnamespace/2, cpnsset/2, cpmultiname/2]).
% -compile(export_all).
-include("swfabc.hrl").



%%
%% arrays
%%

array0(Fun, B) ->
	array0(Fun, B, []).
array0(Fun, B, Args) ->
	{Count, B2} = u30(B),
	array(Fun, B2, Args, Count-1).

array(Fun, B) ->
	array(Fun, B, []).
array(Fun, B, Args) ->
	{Count, B2} = u30(B),
	array(Fun, B2, Args, Count).

array(_, B, _, Count) when Count < 1 ->
	{[], B};
array(Fun, B, Args, Count) ->
	%io:format("array count: ~p~n", [Count]),
	{L, Bout} = lists:foldl(fun(_, {Acc, Bx}) ->
		{P, By} = apply(Fun, [Bx|Args]),
		{[P|Acc], By}
	end, {[], B}, lists:seq(1, Count)),
	{lists:reverse(L), Bout}.


%%
%% simple data types
%%

u8(<<X, R/binary>>) ->
	{X, R}.
u16(<<X:16/unsigned-integer-little, Rest/binary>>) ->
	{X, Rest}.

s24(<<X:24/signed-integer-little, Rest/binary>>) ->
	{X, Rest}.

u32(<<0:1, A:7, R/binary>>) ->
	{A, R};
u32(<<1:1, A:7/bitstring, 0:1, B:7/bitstring, R/binary>>) ->
	<<X:32/unsigned-integer-big>> = <<0:4, 0:7, 0:7, B/bitstring, A/bitstring>>, {X, R};
u32(<<1:1, A:7/bitstring, 1:1, B:7/bitstring, 0:1, C:7/bitstring, R/binary>>) ->
	<<X:32/unsigned-integer-big>> = <<0:4, 0:7, C/bitstring, B/bitstring, A/bitstring>>, {X, R};
u32(<<1:1, A:7/bitstring, 1:1, B:7/bitstring, 1:1, C:7/bitstring, 0:1, D:7/bitstring, R/binary>>) ->
	<<X:32/unsigned-integer-big>> = <<0:4, D/bitstring, C/bitstring, B/bitstring, A/bitstring>>, {X, R};
u32(<<1:1, A:7/bitstring, 1:1, B:7/bitstring, 1:1, C:7/bitstring, 1:1, D:7/bitstring, _:4/bitstring, E:4/bitstring, R/binary>>) ->
	<<X:32/unsigned-integer-big>> = <<E/bitstring, D/bitstring, C/bitstring, B/bitstring, A/bitstring>>, {X, R}.

u30(B) ->
	{X, R} = u32(B),
	<<_:2, Y:30/unsigned-integer-big>> = <<X:32/unsigned-integer-big>>,
	{Y, R}.

s32(B) ->
	{X, R} = u32(B),
	% Y = case X > (1 bsl 31) of
	% 	true -> X - (1 bsl 32);
	% 	false -> X
	% end,
	<<Y:32/signed-integer-little>> = <<X:32/unsigned-integer-little>>,
	{Y, R}.

d64(<<X:64/float-little, R/binary>>) ->
	{X, R};
d64(<<0,0,0,0,0,0,240,127, R/binary>>) ->
	{inf, R};
d64(<<0,0,0,0,0,0,248,127, R/binary>>) ->
	{nan, R};
d64(<<_:64, R/binary>>) ->
	{inv, R}.

string(B) ->
	{Count, B2} = u30(B),
	<<X:Count/binary, R/binary>> = B2,
	{X, R}.

%%
%% structs
%%

namespace(<<Kind, B/binary>>, CP) ->
	KindName = case Kind of
		16#08 -> namespace;
		16#16 -> package_namespace;
		16#17 -> package_internal_ns;
		16#18 -> protected_namespace;
		16#19 -> explicit_namespace;
		16#1A -> static_protected_ns;
		16#05 -> private_ns;
		X -> {unknown, X}
	end,
	{Index, R} = u30(B),

	{{KindName, cpstring(Index, CP)}, R}.

ns_set(B, CP) ->
	array(fun(Bx) ->
		{Index, R} = u30(Bx),
		{cpnamespace(Index, CP), R}
	end, B).

multiname(<<Type, B/binary>>, CP) ->
	case Type of
		16#07 ->
			{NsIndex, R1} = u30(B),
			{NameIndex, R2} = u30(R1),
			{{'QName', cpnamespace(NsIndex, CP), cpstring(NameIndex, CP)}, R2};
		16#0D ->
			{NsIndex, R1} = u30(B),
			{NameIndex, R2} = u30(R1),
			{{'QNameA', cpnamespace(NsIndex, CP), cpstring(NameIndex, CP)}, R2};
		16#0F ->
			{NameIndex, R1} = u30(B),
			{{'RTQName', cpstring(NameIndex, CP)}, R1};
		16#10 ->
			{NameIndex, R1} = u30(B),
			{{'RTQNameA', cpstring(NameIndex, CP)}, R1};
		16#11 ->
			{{'RTQNameL'}, B};
		16#12 ->
			{{'RTQNameLA'}, B};
		16#09 ->
			{NameIndex, R1} = u30(B),
			{NsSetIndex, R2} = u30(R1),
			{{'Multiname', cpstring(NameIndex, CP), cpnsset(NsSetIndex, CP)}, R2};
		16#0E ->
			{NameIndex, R1} = u30(B),
			{NsSetIndex, R2} = u30(R1),
			{{'MultinameA', cpstring(NameIndex, CP), cpnsset(NsSetIndex, CP)}, R2};
		16#1B ->
			{NsSetIndex, R1} = u30(B),
			{{'MultinameL', cpnsset(NsSetIndex, CP)}, R1};
		16#1C ->
			{NsSetIndex, R1} = u30(B),
			{{'MultinameLA', cpnsset(NsSetIndex, CP)}, R1};
		_ ->
			{{unknown, Type}, B}
	end.

option_detail(B, CP) ->
	{VIndex, <<Kind, R/binary>>} = u30(B),
	Val = cpconstant(Kind, VIndex, CP),
	{Val, R}.


% traits_info  
% { 
% u30 name 
% u8  kind 
% u8  data[] 
% u30 metadata_count 
% u30 metadata[metadata_count] 
% } 

traits_info(B, CP) ->
	{Name, R1} = u30(B),
	<<Attr:4, Kind:4, R2/binary>> = R1,
	AttrArr = flags(Attr, [{16#01, final}, {16#02, override}, {16#04, metadata}]),
	KindName = case Kind of
		0 -> slot;
		6 -> const;
		4 -> class;
		5 -> function;
		1 -> method;
		2 -> getter;
		3 -> setter;
		_ -> invalid
	end,
	{Data, R3} = case Kind of
		X when X =:= 0; X =:= 6 -> %% trait slot or const
			{SlotId, Q1} = u30(R2),
			{TypeNameIndex, Q2} = u30(Q1),
			TypeName = cpmultiname(TypeNameIndex, CP),
			{Vindex, Q3} = u30(Q2),
			{Vkind, Q4} = case Vindex =:= 0 of
				true -> {none, Q3};
				false -> u8(Q3)
			end,
			Val = cpconstant(Vkind, Vindex, CP),
			{#trait_slot{slot_id=SlotId, type_name=TypeName, value=Val}, Q4};
		4 -> %% trait class
			{SlotId, Q1} = u30(R2),
			{Classi, Q2} = u30(Q1),
			{#trait_class{slot_id=SlotId, classi=Classi}, Q2};
		5 -> %% trait function
			{SlotId, Q1} = u30(R2),
			{Functioni, Q2} = u30(Q1),
			{#trait_function{slot_id=SlotId, functioni=Functioni}, Q2};
		X when X =:= 1; X =:= 2; X =:= 3 -> %% trait method, getter, setter
			{DispId, Q1} = u30(R2),
			{Methodi, Q2} = u30(Q1),
			{#trait_method{disp_id=DispId, methodi=Methodi}, Q2};
		_ -> throw({invalid_trait_kind, Kind})
	end,
	
	{Metadata, R4} = case lists:member(metadata, AttrArr) of
		true ->
			array(fun u30/1, R3);
		false ->
			{[], R3}
	end,
	
	{#trait{name=Name, type=KindName, data=Data, metadata=Metadata}, R4}.


% exception_info  
% { 
% 	u30 from 
% 	u30 to  
% 	u30 target 
% 	u30 exc_type 
% 	u30 var_name 
% } 

exception_info(B, CP) ->
	{From, R1} = u30(B),
	{To, R2} = u30(R1),
	{Target, R3} = u30(R2),
	{ExcTypeI, R4} = u30(R3),
	ExcType = cpstring(ExcTypeI, CP),
	{VarNameI, R5} = u30(R4),
	VarName = cpstring(VarNameI, CP),
	{#exception{from=From, to=To, target=Target, exc_type=ExcType, var_name=VarName}, R5}.


%% parse instructions
code(B, CP) ->
	code(B, CP, [], 0).
code(<<>>, _CP, Acc, _Addr) ->
	lists:reverse(Acc);
code(<<OpCode/unsigned-integer, B/binary>>, CP, Acc, Addr) ->
	Fun_Multiname_Arg = fun(IName) ->
		{Idx, R1} = u30(B),
		{ArgCount, R} = u30(R1),
		{IName, [cpmultiname(Idx, CP), ArgCount], R}
	end,
	Fun_Arg = fun(IName) ->
		{ArgCount, R} = u30(B),
		{IName, [ArgCount], R}
	end,
	Fun_Multiname = fun(IName) ->
		{Idx, R} = u30(B),
		{IName, [cpmultiname(Idx, CP)], R}
	end,
	Fun_Idx = fun(IName) ->
		{Idx, R} = u30(B),
		{IName, [Idx], R}
	end,
	Fun_Offset = fun(IName) ->
		{Offset, R} = s24(B),
		{IName, [{offset, Offset}], R}
	end,
	
	Op = case OpCode of
		% 16#00 -> unknown_0x00; %% not is spec
		
		16#02 -> nop;
		16#03 -> throw;
		16#04 -> Fun_Multiname(getsuper);
		16#05 -> Fun_Multiname(setsuper);
		16#06 ->
			{Idx, R} = u30(B),
			{dxns, [cpstring(Idx, CP)], R};
		16#07 -> dxnslate;
		16#08 -> Fun_Idx(kill);
		16#09 -> label;
		
		16#0c -> Fun_Offset(ifnlt);
		16#0d -> Fun_Offset(ifnle);
		16#0e -> Fun_Offset(ifngt);
		16#0f -> Fun_Offset(ifnge);
		
		16#10 -> Fun_Offset(jump);
		16#11 -> Fun_Offset(iftrue);
		16#12 -> Fun_Offset(iffalse);
		16#13 -> Fun_Offset(ifeq);
		16#14 -> Fun_Offset(ifne);
		16#15 -> Fun_Offset(iflt);
		16#16 -> Fun_Offset(ifle);
		16#17 -> Fun_Offset(ifgt);
		16#18 -> Fun_Offset(ifeq);
		16#19 -> Fun_Offset(ifstricteq);
		16#1a -> Fun_Offset(ifstrictne);
		16#1b ->
			{DefaultOffset, R1} = s24(B),
			{Count, R2} = u30(R1),
			{CaseOffsets, R3} = array(fun(Bx) -> s24(Bx) end, R2, [], Count+1),
			{lookupswitch, [DefaultOffset, CaseOffsets], R3};
		16#1c -> pushwith;
		16#1d -> popscope;
		16#1e -> nextname;
		16#1f -> hasnext;
		16#20 -> pushnull;
		16#21 -> pushundefined;
		
		16#23 -> nextvalue;
		16#24 ->
			{Byte, R} = u8(B),
			{pushbyte, [Byte], R};
		16#25 -> Fun_Idx(pushshort);
		16#26 -> pushtrue;
		16#27 -> pushfalse;
		16#28 -> pushnan;
		16#29 -> pop;
		16#2a -> dup;
		16#2b -> swap;
		16#2c ->
			{Idx, R} = u30(B),
			{pushstring, [cpstring(Idx, CP)], R};
		16#2d ->
			{Idx, R} = u30(B),
			{pushint, [cpinteger(Idx, CP)], R};
		16#2e -> Fun_Idx(pushuint);

		16#2f ->
			{Idx, R} = u30(B),
			{pushdouble, [cpdouble(Idx, CP)], R};
		16#30 -> pushscope;
		16#31 ->
			{Idx, R} = u30(B),
			{pushnamespace, [cpnamespace(Idx, CP)], R};
		16#32 ->
			{I1, R1} = u30(B),
			{I2, R2} = u30(R1),
			{hasnext2, [I1, I2], R2};

		16#40 ->
			{Idx, R} = u30(B),
			{newfunction, [{method, Idx}], R};
		16#41 -> Fun_Arg(call);
		16#42 -> Fun_Arg(construct);
		16#43 ->
			{Idx, R1} = u30(B),
			{ArgCount, R} = u30(R1),
			{callmethod, [{method, Idx}, ArgCount], R};
		16#44 ->
			{Idx, R1} = u30(B),
			{ArgCount, R} = u30(R1),
			{callstatic, [{method, Idx}, ArgCount], R};
		16#45 -> Fun_Multiname_Arg(callsuper);
		16#46 -> Fun_Multiname_Arg(callproperty);
		16#47 -> returnvoid;
		16#48 -> returnvalue;
		16#49 -> Fun_Arg(constructsuper);

		16#4c -> Fun_Multiname_Arg(callproplex);

		16#4e -> Fun_Multiname_Arg(callsupervoid);
		16#4f -> Fun_Multiname_Arg(callpropvoid);

		16#4a -> Fun_Multiname_Arg(constructprop);
		
		16#55 -> Fun_Arg(newobject);
		16#56 -> Fun_Arg(newarray);
		16#57 -> newactivation;
		16#58 -> Fun_Idx(newclass);
		16#59 -> Fun_Multiname(getdescendants);
		
		16#5a -> Fun_Idx(newcatch);

		16#5d -> Fun_Multiname(findpropstrict);
		16#5e -> Fun_Multiname(findproperty);
		
		16#60 -> Fun_Multiname(getlex);
		16#61 -> Fun_Multiname(setproperty);
		16#62 -> Fun_Idx(getlocal);
		16#63 -> Fun_Idx(setlocal);
		16#64 -> getglobalscope;

		16#65 ->
			{Idx, R} = u8(B),
			{getscopeobject, [Idx], R};
		16#66 -> Fun_Multiname(getproperty);

		16#68 -> Fun_Multiname(initproperty);

		16#6a -> Fun_Multiname(deleteproperty);

		16#6c -> Fun_Idx(getslot);
		16#6d -> Fun_Idx(setslot);
		16#6e -> Fun_Idx(getglobalslot);
		16#6f -> Fun_Idx(setglobalslot);
		16#70 -> convert_s;
		16#71 -> esc_xelem;
		16#72 -> esc_xattr;
		16#73 -> convert_i;
		16#74 -> convert_u;
		16#75 -> convert_d;
		16#76 -> convert_b;
		16#77 -> convert_o;
		16#78 -> checkfilter;

		16#80 -> Fun_Multiname(coerce);

		16#82 -> coerce_a;

		16#85 -> coerce_s;

		16#86 -> Fun_Multiname(astype);
		16#87 -> astypelate;

		16#90 -> negate;
		16#91 -> increment;
		16#92 -> Fun_Idx(inclocal);
		16#93 -> decrement;
		16#94 -> Fun_Idx(declocal);
		16#95 -> typeof;
		16#96 -> 'not';
		16#97 -> bitnot;

		16#a0 -> add;
		16#a1 -> subtract;
		16#a2 -> multiply;
		16#a3 -> divide;
		16#a4 -> modulo;
		16#a5 -> lshift;
		16#a6 -> rshift;
		16#a7 -> urshift;
		16#a8 -> bitand;
		16#a9 -> bitor;
		16#aa -> bitxor;
		16#ab -> equals;
		16#ac -> strictequals;
		16#ad -> lessthan;
		16#ae -> lessequals;
		16#af -> greaterthan;
		16#b0 -> greaterequals;
		16#b1 -> instanceof;
		16#b2 -> Fun_Multiname(istype);
		16#b3 -> istypelate;
		16#b4 -> in;
		
		16#c0 -> increment_i;
		16#c1 -> decrement_i;
		16#c2 -> Fun_Idx(inclocal_i);
		16#c3 -> Fun_Idx(declocal_i);
		16#c4 -> negate_i;
		16#c5 -> add_i;
		16#c6 -> subtract_i;
		16#c7 -> multiply_i;
		
		16#d0 -> getlocal_0;
		16#d1 -> getlocal_1;
		16#d2 -> getlocal_2;
		16#d3 -> getlocal_3;
		16#d4 -> setlocal_0;
		16#d5 -> setlocal_1;
		16#d6 -> setlocal_2;
		16#d7 -> setlocal_3;
		
		16#ef ->
			{DebugType, R1} = u8(B),
			{RegNameIdx, R2} = u30(R1),
			RegName = cpstring(RegNameIdx, CP),
			{Reg, R3} = u8(R2),
			{_Extra, R} = u30(R3),
			{debug, [DebugType, RegName, Reg], R};

		16#f0 -> Fun_Idx(debugline);
		16#f1 ->
			{Idx, R} = u30(B),
			{debugfile, [cpstring(Idx, CP)], R};
		X -> throw({invalid, X}) %{{invalid, X}, B}
	end,
	
	{Name, Args, Rest} = case Op of
		{_Name, _Args, _Rest} -> Op;
		{Nm, Rx} -> {Nm, [], Rx};
		Nm when is_atom(Nm) -> {Nm, [], B}
	end,
	% io:format("~p~n", [{Name, Args}]),
	NewAddr = Addr + (1 + byte_size(B) - byte_size(Rest)),
	code(Rest, CP, [#instr{addr=Addr, name=Name, args=Args}|Acc], NewAddr).


%%
%% toplevel structs
%%

% abcFile  
% { 
%  u16 minor_version 
%  u16 major_version 
%  cpool_info constant_pool 
%  u30 method_count 
%  method_info method[method_count] 
%  u30 metadata_count 
%  metadata_info metadata[metadata_count] 
%  u30 class_count 
%  instance_info instance[class_count] 
%  class_info class[class_count] 
%  u30 script_count 
%  script_info script[script_count] 
%  u30 method_body_count 
%  method_body_info method_body[method_body_count] 
% } 

%% @doc parse binary abc bytecode
%% @spec abc(binary()) -> abcfile()
abc(B) when is_binary(B) ->
	{MinorVersion, R1} = u16(B),
	{MajorVersion, R2} = u16(R1),
	{ConstantPool, R3} = cpool_info(R2),
	{Method, R4} = array(fun method_info/2, R3, [ConstantPool]),
	{Metadata, R5} = array(fun metadata_info/2, R4, [ConstantPool]),
	{ClassCount, R6} = u30(R5),
	{Instance, R7} = array(fun instance_info/2, R6, [ConstantPool], ClassCount),
	{Class, R8} = array(fun class_info/2, R7, [ConstantPool], ClassCount),
	{Script, R9} = array(fun script_info/2, R8, [ConstantPool]),
	{MethodBody, R10} = array(fun method_body_info/2, R9, [ConstantPool]),

	{#abcfile{
		version={MinorVersion, MajorVersion},
		cpool=ConstantPool,
		method=Method,
		metadata=Metadata,
		instance=Instance,
		class=Class,
		script=Script,
		method_body=MethodBody
	}, R10}.
	


% cpool_info 
% { 
%  u30 int_count 
%  s32 integer[int_count] 
%  u30 uint_count 
%  u32 uinteger[uint_count] 
%  u30 double_count 
%  d64 double[double_count] 
%  u30 string_count 
%  string_info string[string_count] 
%  u30 namespace_count 
%  namespace_info namespace[namespace_count] 
%  u30 ns_set_count 
%  ns_set_info ns_set[ns_set_count] 
%  u30 multiname_count 
%  multiname_info multiname[multiname_count]  
% } 

cpool_info(B) ->
	{Integer, R1} = array0(fun s32/1, B),
	{Uinteger, R2} = array0(fun u32/1, R1),
	{Double, R3} = array0(fun d64/1, R2),
	{String, R4} = array0(fun string/1, R3),
	{Namespace, R5} = array0(fun namespace/2, R4, [#cpool{string=String}]),
	{NsSet, R6} = array0(fun ns_set/2, R5, [#cpool{namespace=Namespace}]),
	{Multiname, R7} = array0(fun multiname/2, R6, [#cpool{namespace=Namespace, string=String, ns_set=NsSet}]),
		
	{#cpool{
		integer=Integer,
		uinteger=Uinteger,
		double=Double,
		string=String,
		namespace=Namespace,
		ns_set=NsSet,
		multiname=Multiname
	},R7}.



% method_info
% { 
%  u30 param_count 
%  u30 return_type 
%  u30 param_type[param_count] 
%  u30 name 
%  u8  flags 
%  option_info options 
%  param_info param_names 
% } 

method_info(B, CP) ->
	{ParamCount, R1} = u30(B),
	
	{ReturnTypeIndex, R2} = u30(R1),
	ReturnType = cpmultiname(ReturnTypeIndex, CP),
	
	{ParamType, R3} = array(fun(Bx, C) ->
		{I, Q} = u30(Bx),
		{cpmultiname(I, C), Q}
	end, R2, [CP], ParamCount),
	
	{NameIndex, R4} = u30(R3),
	Name = cpstring(NameIndex, CP),
	
	<<Flags, R5/binary>> = R4,
	FlagsArr = flags(Flags, [
		{16#01, need_arguments},
		{16#02, need_activation},
		{16#04, need_rest},
		{16#08, has_optional},
		{16#40, set_dxns},
		{16#80, has_param_names}
	]),
	
	{Options, R6} = case lists:member(has_optional, FlagsArr) of
		true ->
			{ODetail, Q2} = array(fun option_detail/2, R5, [CP]),
			{ODetail, Q2};
		false ->
			{[], R5}
	end,
	
	{ParamNames, R7} = case lists:member(has_param_names, FlagsArr) of
		true ->
			array(fun(Bx, C) ->
				{I, R} = u30(Bx),
				{cpstring(I, C), R}
			end, R6, [CP], ParamCount);
		false ->
			{[], R6}
	end,
	
	{#method{
		param_count=ParamCount,
		return_type=ReturnType,
		param_type=ParamType,
		name=Name,
		flags=Flags,
		flags_arr=FlagsArr,
		options=Options,
		param_names=ParamNames}, R7}.



% metadata_info  
% { 
%  u30 name 
%  u30 item_count 
%  item_info items[item_count] 
% } 

metadata_info(B, CP) ->
	{NameIndex, R1} = u30(B),
	Name = cpstring(NameIndex, CP),
	{Items, R2} = array(fun(Bx, C) ->
		{KeyIndex, Q1} = u30(Bx),
		{ValueIndex, Q2} = u30(Q1),
		Value = cpstring(ValueIndex, C),
		case KeyIndex of
			0 -> {Value, Q2};
			_ ->
				Key = cpstring(KeyIndex, C),
				{{Key, Value}, Q2}
		end
	end, R1, [CP]),
	{{Name, Items}, R2}.



% instance_info  
% { 
% 	u30 name 
% 	u30 super_name 
% 	u8  flags 
% 	u30 protectedNs  
% 	u30 intrf_count 
% 	u30 interface[intrf_count] 
% 	u30 iinit 
% 	u30 trait_count 
% 	traits_info trait[trait_count] 
% } 

instance_info(B, CP) ->
	{NameIndex, R1} = u30(B),
	Name = cpmultiname(NameIndex, CP),
	
	{SuperNameIndex, R2} = u30(R1),
	SuperName = cpmultiname(SuperNameIndex, CP),
	
	{Flags, R3} = u8(R2),
	FlagsArr = flags(Flags, [
		{16#01, class_sealed},
		{16#02, class_final},
		{16#04, class_interface},
		{16#08, class_protected_ns}
	]),
	
	{ProtectedNs, R4} = case lists:member(class_protected_ns, FlagsArr) of
		true ->
			{PNsIndex, Q1} = u30(R3),
			{cpnamespace(PNsIndex, CP), Q1};
		false -> {undefined, R3}
	end,
	
	{Interface, R5} = array(fun(Bx, C) ->
		{I, Q} = u30(Bx),
		{cpmultiname(I, C), Q}
	end, R4, [CP]),
	
	{Iinit, R6} = u30(R5),
	
	{Trait, R7} = array(fun traits_info/2, R6, [CP]),
	
	{#instance{
		name=Name,
		super_name=SuperName,
		flags=Flags,
		flags_arr=FlagsArr,
		protected_ns=ProtectedNs,
		interface=Interface,
		iinit=Iinit,
		trait=Trait}, R7}.


% class_info  
% { 
% 	u30 cinit 
% 	u30 trait_count 
% 	traits_info traits[trait_count] 
% } 

class_info(B, CP) ->
	{Cinit, R1} = u30(B),
	{Trait, R2} = array(fun traits_info/2, R1, [CP]),
	{#class{cinit=Cinit, trait=Trait}, R2}.


% script_info  
% { 
% 	u30 init 
% 	u30 trait_count 
% 	traits_info trait[trait_count] 
% } 

script_info(B, CP) ->
	{Init, R1} = u30(B),
	{Trait, R2} = array(fun traits_info/2, R1, [CP]),
	{#script{init=Init, trait=Trait}, R2}.


% method_body_info 
% { 
% 	u30 method 
% 	u30 max_stack 
% 	u30 local_count 
% 	u30 init_scope_depth  
% 	u30 max_scope_depth 
% 	u30 code_length 
% 	u8  code[code_length] 
% 	u30 exception_count 
% 	exception_info exception[exception_count] 
% 	u30 trait_count 
% 	traits_info trait[trait_count] 
% } 

method_body_info(B, CP) ->
	{Method, R1} = u30(B),
	% io:format("method: ~p~n", [Method]),
	{MaxStack, R2} = u30(R1),
	{LocalCount, R3} = u30(R2),
	{InitScopeDepth, R4} = u30(R3),
	{MaxScopeDepth, R5} = u30(R4),
	{CodeLength, R6} = u30(R5),
	<<CodeB:CodeLength/binary, R7/binary>> = R6,
	Code = code(CodeB, CP),
	{Exception, R8} = array(fun exception_info/2, R7, [CP]),
	{Trait, R9} = array(fun traits_info/2, R8, [CP]),
	{#method_body{
		methodi=Method,
		max_stack=MaxStack,
		local_count=LocalCount,
		init_scope_depth=InitScopeDepth,
		max_scope_depth=MaxScopeDepth,
		code=Code,
		exception=Exception,
		trait=Trait
	}, R9}.


%%
%% helper functions
%%


%% parse flags
flags(U8, FlagList) ->
	lists:foldl(fun({Code, FlagName}, Acc) ->
		case (U8 band Code) =/= 0 of
			true -> [FlagName|Acc];
			_ -> Acc
		end
	end, [], FlagList).

%% constant pool access helpers
cpinteger(Index, #cpool{integer=X}) -> {integer, cpget(Index, X)}.
cpuinteger(Index, #cpool{uinteger=X}) -> {uinteger, cpget(Index, X)}.
cpdouble(Index, #cpool{double=X}) -> {double, cpget(Index, X)}.
cpstring(Index, #cpool{string=X}) -> {string, cpget(Index, X)}.
cpnamespace(Index, #cpool{namespace=X}) -> {namespace, cpget(Index, X)}.
cpnsset(Index, #cpool{ns_set=X}) -> {ns_set, cpget(Index, X)}.
cpmultiname(Index, #cpool{multiname=X}) -> {multiname, cpget(Index, X)}.

cpget(_, Arr) when Arr =:= undefined -> undefined;
cpget(0, _) -> '*';
cpget(Index, Arr) ->
	case Index =< length(Arr) of
		true -> lists:nth(Index, Arr);
		false -> undefined
	end.

cpconstant(Kind, VIndex, CP) ->
	case Kind of
		16#03 -> {integer, cpinteger(VIndex, CP)};
		16#04 -> {uinteger, cpuinteger(VIndex, CP)};
		16#06 -> {double, cpdouble(VIndex, CP)};
		16#01 -> {utf8, cpstring(VIndex, CP)};
		16#0B -> true;
		16#0A -> false;
		16#0C -> null;
		16#00 -> undefined;
		none -> none;
		X when X =:= 16#08; X >= 16#16, X =< 16#1A; X =:= 16#05 ->
			{namespace, cpnamespace(VIndex, CP)};
		X -> {unknown, X}
	end.

%% method access helper
% method_and_body(MethodIndex, #abcfile{method=Methods, method_body=MB}) ->
% 	Method = lists:nth(MethodIndex+1, Methods),
% 	{value, Methodbody} = lists:keysearch(MethodIndex, 2, MB),
% 	{Method, Methodbody}.

method(Index, Methods) ->
	lists:nth(Index+1, Methods).

method_body(MethodIndex, #abcfile{method_body=MB}) ->
	{value, Methodbody} = lists:keysearch(MethodIndex, 2, MB),
	Methodbody.

