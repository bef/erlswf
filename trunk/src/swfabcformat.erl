%%
%% @doc format abc dumps in a logically structured and human readable manner
%%

-module(swfabcformat).
-export([abc/1, abc/3, dtfmt/1]).
-include("swfabc.hrl").


%% @doc same as abc(standard_io, Abc, all)
%% @spec abc(abc()) -> ok
abc(Abc) ->
	abc(standard_io, Abc, all).

%% @doc stream fancy abc to given io device
%% @spec abc(io_device(), abc(), [identifier()]) -> ok
abc(Io, Abc, all) ->
	abc(Io, Abc, ["version", "cpool", "metadata", "scripts", "classes"]);
abc(_, _, []) -> ok;
abc(Io, Abc, [Part|Parts]) ->
	abcfmt(Io, Part, Abc),
	abc(Io, Abc, Parts).

abcfmt(Io, "version", #abcfile{version={Minor, Major}}) ->
	io:format(Io, "abc version: ~p.~p~n", [Major, Minor]);
abcfmt(Io, "cpool", #abcfile{cpool=Cpool}) ->
	cpool(Io, Cpool);
abcfmt(Io, "metadata", #abcfile{metadata=Metadata}) ->
	metadata(Io, Metadata);
abcfmt(Io, "methods", Abc) ->
	methods(Io, Abc);
abcfmt(Io, "scripts", Abc) ->
	scripts(Io, Abc);
abcfmt(Io, "classes", Abc) ->
	classes(Io, Abc);

abcfmt(_Io, X, _Abc) ->
	throw({unknown_option, X}).


cpool(Io, #cpool{integer=Integer, uinteger=UInteger, double=Double, string=String, namespace=Namespace, ns_set=NsSet, multiname=Multiname}) ->
	io:format(Io, "~nCONSTANT POOL~n", []),
	
	IFun = fun(X) -> lists:flatten(io_lib:format("~p", [X])) end,
	cpoolfmt(Io, "integer pool", IFun, Integer),
	cpoolfmt(Io, "unsigned integer pool", IFun, UInteger),
	cpoolfmt(Io, "double pool", IFun, Double),
	cpoolfmt(Io, "string pool", IFun, String),
	cpoolfmt(Io, "namespace pool", fun({Type, {string, S}}) -> lists:flatten(io_lib:format("[~p] \"~s\"", [Type, S])) end, Namespace),
	cpoolfmt(Io, "ns set pool", IFun, NsSet),
	cpoolfmt(Io, "multiname pool", fun cpmn/1, Multiname),

	ok.

cpoolfmt(Io, Desc, Fun, L) ->
	io:format(Io, "~s:~n", [Desc]),
	case L of
		[] -> io:format(Io, "	empty~n", []);
		X when is_list(X) ->
			lists:foldl(fun(El, Idx) ->
				io:format(Io, "	~p	~s~n", [Idx, Fun(El)]),
				Idx+1
			end, 1, X);
		_ -> io:format(Io, "	~p~n", [L])
	end,
	io:nl(Io).

%% constant pool multiname
cpmn({Type}) ->
	lists:flatten(io_lib:format("[~p]", [Type]));
cpmn({Type, P1}) ->
	lists:flatten(io_lib:format("[~p] ~s", [Type, dtfmt(P1)]));
cpmn({Type, P1, P2}) ->
	lists:flatten(io_lib:format("[~p] ~s::~s", [Type, dtfmt(P1), dtfmt(P2)])).

%% metadata
metadata(Io, Metadata) ->
	io:format(Io, "~nMETADATA~n", []),
	case Metadata of
		[] -> io:format(Io, "	empty~n", []);
		_ -> metadata(Io, Metadata, 1)
	end.
metadata(_, [], _) -> ok;
metadata(Io, [M|R], N) ->
	io:format(Io, "	~p	~p~n", [N, M]),
	metadata(Io, R, N+1).

methods(Io, #abcfile{method_body=MBs}=Abc) ->
	methods(Io, MBs, Abc, 0).
methods(_, [], _, _) -> ok;
methods(Io, [MB|MBs], Abc, N) ->
	io:format(Io, "/* method no. ~p */~n", [N]),
	method(Io, MB, Abc),
	methods(Io, MBs, Abc, N+1).

method(Io, #method_body{methodi=Methodi, max_stack=MaxStack, local_count=LocalCount, init_scope_depth=InitScopeDepth, max_scope_depth=MaxScopeDepth, code=Code, exception=Exceptions, trait=Traits}, #abcfile{method=Methods}) ->
	#method{param_count=ParamCount, return_type=ReturnType, param_type=ParamType, name=Name, flags=Flags, flags_arr=FlagsArr, options=Options, param_names=ParamNames} = swfabc:method(Methodi, Methods),
	ParamTypes = [dtfmt(PT) || PT <- ParamType],
	io:format(Io, "  method ~s/~p(~s):~p returns ~s {~n", [dtfmt(Name), ParamCount, string:join(ParamTypes, ", "), ParamNames, dtfmt(ReturnType)]),
	io:format(Io, "    // max stack: ~p~n", [MaxStack]),
	io:format(Io, "    // local count: ~p~n", [LocalCount]),
	io:format(Io, "    // init/max scope depth: ~p/~p~n", [InitScopeDepth, MaxScopeDepth]),
	io:format(Io, "    // flags: ~p/~p~n", [Flags, FlagsArr]),
	io:format(Io, "    // exceptions: ~p~n", [Exceptions]),
	io:format(Io, "    // traits: ~p~n", [Traits]),
	io:format(Io, "    // options: ~p~n", [Options]),
	code(Io, Code),
	io:format(Io, "  }~n", []),
	ok.
	
code(_, []) -> ok;
code(Io, [#instr{addr=Addr, name=Name, args=Args}|R]) ->
	io:format(Io, "    /* ~5.16.0B */ ~p ~s~n", [Addr, Name, [dtfmt(A) || A <- Args]]),
	code(Io, R).


scripts(Io, #abcfile{script=Scripts}=Abc) ->
	io:format(Io, "~nSCRIPTS~n", []),
	case Scripts of
		[] -> io:format(Io, "	empty~n", []);
		_ -> scripts(Io, Scripts, Abc, 0)
	end.
scripts(_Io, [], _Abc, _N) -> ok;
scripts(Io, [#script{init=I, trait=Traits}|Scripts], Abc, N) ->
	io:format(Io, "/* script no. ~p */~n", [N]),
	io:format(Io, "~p, ~p~n", [I, Traits]), %% incomplete/todo
	scripts(Io, Scripts, Abc, N+1).


classes(Io, #abcfile{class=Classes, instance=Instances}=Abc) ->
	io:format(Io, "~nCLASSES~n", []),
	case Classes of
		[] -> io:format(Io, "	empty~n", []);
		_ -> classes(Io, Classes, Instances, Abc, 0)
	end.
classes(_Io, [], [], _Abc, _N) -> ok;
classes(Io, [#class{cinit=Cinit, trait=Ctraits}|Classes],
		[#instance{name=Name, super_name=SuperName, flags=Flags, flags_arr=FlagsArr, protected_ns=ProtectedNs, interface=Interface, iinit=Iinit, trait=Itraits}|Instances],
		Abc, N) ->
	io:format(Io, "/* class no. ~p */~n", [N]),
	io:format(Io, "class ~s extends ~s {~n", [dtfmt(Name), dtfmt(SuperName)]),
	io:format(Io, "  // flags: ~p/~p~n", [Flags, FlagsArr]),
	io:format(Io, "  // protected ns: ~s~n", [dtfmt(ProtectedNs)]),
	io:format(Io, "  // interface: ~p~n", [Interface]),
	io:format(Io, "  static class init (~p)~n", [Cinit]),
	Methodbody = swfabc:method_body(Cinit, Abc),
	method(Io, Methodbody, Abc),

	io:format(Io, "  __new__ (~p)~n", [Iinit]),
	Methodbody2 = swfabc:method_body(Iinit, Abc),
	method(Io, Methodbody2, Abc),

	case Ctraits of
		[] -> ok;
		_ ->
			io:format(Io, "  /* class traits */~n", []),
			traits(Io, Ctraits, Abc)
	end,
	
	case Itraits of
		[] -> ok;
		_ ->
			io:format(Io, "  /* instance traits */~n", []),
			traits(Io, Itraits, Abc)
	end,
	
	io:format(Io, "}~n", []),
	classes(Io, Classes, Instances, Abc, N+1).



dtfmt({string, X}) when is_binary(X) ->
	io_lib:format("~s", [X]);
dtfmt({string, X}) when is_atom(X) ->
	io_lib:format("~s", [atom_to_list(X)]);

dtfmt({namespace, {_Kind, {string, Str}}}) ->
	dtfmt({string, Str});
dtfmt({multiname, Type}) when is_atom(Type) ->
	io_lib:format("{~s}", [atom_to_list(Type)]);
dtfmt({multiname, {_Type, P1}}) ->
	dtfmt(P1);
dtfmt({multiname, {_Type, P1, P2}}) ->
	lists:flatten(io_lib:format("~s::~s", [dtfmt(P1), dtfmt(P2)]));

dtfmt(X) ->
	io_lib:format("<<~p>>", [X]).


traits(_, [], _) -> ok;
traits(Io, [T|Traits], Abc) ->
	traitfmt(Io, T, Abc),
	traits(Io, Traits, Abc).

traitfmt(Io, #trait{name=NameI, type=Type, data=Data, metadata=Metadata}, #abcfile{cpool=CP}=Abc) ->
	Name = swfabc:cpmultiname(NameI, CP),
	case Data of
		#trait_slot{slot_id=SlotId, type_name=TypeName, value=Value} ->
			io:format(Io, "  ~s ~s ~s = ~s /* slot id: ~p */~n", [atom_to_list(Type), dtfmt(TypeName), dtfmt(Name), dtfmt(Value), SlotId]);
		#trait_method{disp_id=DispId, methodi=MethodI} ->
			io:format("  ~s /* disp id ~p */~n", [dtfmt(Name), DispId]),
			Methodbody = swfabc:method_body(MethodI, Abc),
			method(Io, Methodbody, Abc);
		_ -> %% unknown/unimplemented trait type
			io:format(Io, "  trait ~s ~s~n", [atom_to_list(Type), dtfmt(Name)]),
			io:format(Io, "    data: ~p~n", [Data]),
			io:format(Io, "    metadata: ~p~n", [Metadata])
	end.
