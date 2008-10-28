-module(swfaction).
-export([
	actionrecords/1,
	action/1,
	action/2
]).


%%
%% actions
%%

%% @doc decode null-terminated list of ACTIONRECORD[]
%% @spec actionrecords(binary()) -> {actions, paramlist(), rest()}
%%   where rest() = binary()
actionrecords(B) ->
	actionrecords(B, 0, []).
actionrecords(<<>> = B, Pos, Acc) ->
	{actions, lists:reverse([{invalid_end_of_actionrecord, [{code, -1},{pos, Pos}]}|Acc]), B};
actionrecords(<<0, B/binary>>, _Pos, Acc) ->
	{actions, lists:reverse(Acc), B};
actionrecords(<<0:1, ActionCode:7, B/binary>>, Pos, Acc) -> %% record w/o data
	swf:debug("action [~2.16.0B]~n", [ActionCode]),
	actionrecords(B, Pos + 1, [{action(ActionCode), [{code, ActionCode}, {pos, Pos}]}|Acc]);
actionrecords(<<ActionCode, Length:16/unsigned-integer-little, Data:Length/binary, B/binary>>, Pos, Acc) -> %% record w/ data
	swf:debug("action [~2.16.0B]: ~p~n", [ActionCode, Data]),
	{Name, Content} =
		try action(ActionCode, Data) of
			Val -> Val
		catch
			error:X ->
				error_logger:warning_msg("<~8.10.0B> [~2.16.0B] action : ~p~n     ~p~n", [Pos, ActionCode, X, erlang:get_stacktrace()]),
				{parseerror, X}
		end,
	actionrecords(B, Pos + 3 + Length, [{Name, [{code, ActionCode}, {pos, Pos}|Content]}|Acc]).

%% @doc actions from 0x01 to 0x9f (actions w/o data)
%% @spec action(integer()) -> atom()
action(16#04) -> nextFrame;
action(16#05) -> previousFrame;
action(16#06) -> play;
action(16#07) -> stop;
action(16#08) -> toggleQuality;
action(16#09) -> stopSounds;
action(16#0A) -> add;
action(16#0B) -> subtract;
action(16#0C) -> multiply;
action(16#0D) -> divide;
action(16#0E) -> equals;
action(16#0F) -> less;
action(16#10) -> 'and';
action(16#11) -> 'or';
action(16#12) -> 'not';
action(16#13) -> stringEquals;
action(16#14) -> stringLength;
action(16#15) -> stringExtract;

action(16#17) -> pop;
action(16#18) -> toInteger;

action(16#1C) -> getVariable;
action(16#1D) -> setVariable;

action(16#20) -> setTarget2;
action(16#21) -> stringAdd;
action(16#22) -> getProperty;
action(16#23) -> setProperty;
action(16#24) -> cloneSprite;
action(16#25) -> removeSprite;
action(16#26) -> trace;
action(16#27) -> startDrag;
action(16#28) -> endDrag;
action(16#29) -> stringLess;
action(16#2A) -> 'throw';
action(16#2B) -> castOp;
action(16#2C) -> implementsOp;

action(16#30) -> randomNumber;
action(16#31) -> mbStringLength;
action(16#32) -> charToAscii;
action(16#33) -> asciiToChar;
action(16#34) -> getTime;
action(16#35) -> mbStringExtract;
action(16#36) -> mbCharToAscii;
action(16#37) -> mbAsciiToChar;

action(16#3A) -> delete;
action(16#3B) -> delete2;
action(16#3C) -> defineLocal;
action(16#3D) -> callFunction;
action(16#3E) -> return;
action(16#3F) -> modulo;
action(16#40) -> newObject;
action(16#41) -> defineLocal2;
action(16#42) -> initArray;
action(16#43) -> initObject;
action(16#44) -> typeOf;
action(16#45) -> targetPath;
action(16#46) -> enumerate;
action(16#47) -> add2;
action(16#48) -> less2;
action(16#49) -> equals2;
action(16#4A) -> toNumber;
action(16#4B) -> toString;
action(16#4C) -> pushDuplicate;
action(16#4D) -> stackSwap;
action(16#4E) -> getMember;
action(16#4F) -> setMember;
action(16#50) -> increment;
action(16#51) -> decrement;
action(16#52) -> callMethod;
action(16#53) -> newMethod;

action(16#54) -> instanceOf;
action(16#55) -> enumerate2;

action(16#60) -> bitAnd;
action(16#61) -> bitOr;
action(16#62) -> bitXor;
action(16#63) -> bitLShift;
action(16#64) -> bitRShift;
action(16#65) -> bitURShift;
action(16#66) -> strictEquals;
action(16#67) -> greater;
action(16#68) -> stringGreater;
action(16#69) -> extends;

action(_ActionCode) -> unknownAction.

%% @doc actions from 0x80 to 0xff (actions w/ data)
%% @spec action(integer(), binary()) -> {atom(), paramlist()}
action(16#81, <<Frame:16/unsigned-integer-little>>) ->
	{gotoFrame, [{frame, Frame}]};

action(16#83, <<B/binary>>) ->
	{URL, R} = swfdt:string(B),
	{Target, _} = swfdt:string(R),
	{getURL, [{url, URL}, {target, Target}]};

action(16#87, <<RegisterNumber>>) ->
	{storeRegister, [{registerNumber, RegisterNumber}]};

action(16#88, <<_Count:16, B/binary>>) ->
	{constantPool, [{pool, swfdt:stringpool(B)}]};

action(16#8A, <<Frame:16/unsigned-integer-little, SkipCount>>) ->
	{waitForFrame, [{frame, Frame}, {skipCount, SkipCount}]};

action(16#8B, <<B/binary>>) ->
	{Name, _} = swfdt:string(B),
	{setTarget, [{name, Name}]};

action(16#8C, <<B/binary>>) ->
	{Label, _} = swfdt:string(B),
	{goToLabel, [{label, Label}]};

action(16#8D, <<SkipCount>>) ->
	{waitForFrame2, [{skipCount, SkipCount}]};

action(16#8E, B) ->
	{Name, <<
			NumParams:16/unsigned-integer-little,
			RegisterCount,
			PreloadParentFlag:1,
			PreloadRootFlag:1,
			SuppressSuperFlag:1,
			PreloadSuperFlag:1,
			SuppressArgumentsFlag:1,
			PreloadArgumentsFlag:1,
			SuppressThisFlag:1,
			PreloadThisFlag:1,
			_Reserved:7,
			PreloadGlobalFlag:1,
			R/binary
		>>} = swfdt:string(B),
	{RegisterParam, <<CodeSize:16/unsigned-integer-little>>} = swfdt:registerparampool(R, NumParams),
	
	{defineFunction2, [	{name, Name},
						{registerCout, RegisterCount},
						{preloadParentFlag, PreloadParentFlag},
						{preloadRootFlag, PreloadRootFlag},
						{suppressSuperFlag, SuppressSuperFlag},
						{preloadSuperFlag, PreloadSuperFlag},
						{suppressArgumentsFlag, SuppressArgumentsFlag},
						{preloadArgumentsFlag, PreloadArgumentsFlag},
						{suppressThisFlag, SuppressThisFlag},
						{preloadThisFlag, PreloadThisFlag},
						{preloadGlobalFlag, PreloadGlobalFlag},
						{registerParams, RegisterParam},
						{codeSize, CodeSize}]};

action(16#8F, <<_Reserved:5,
			CatchInRegisterFlag:1,
			FinallyBlockFlag:1,
			CatchBlockFlag:1,
			_TrySize:16/unsigned-integer-little,
			_CatchSize:16/unsigned-integer-little,
			_FinallySize:16/unsigned-integer-little,
			B/binary>>) ->
	{CatchNameOrRegister, _R} = case CatchInRegisterFlag of
		0 ->
			{Name, R0} = swfdt:string(B),
			{{catchName, Name}, R0};
		1 ->
			<<CatchRegister, R0/binary>> = B,
			{{catchRegister, CatchRegister}, R0}
	end,
	
	%% NOTE: opposed to the documentation the bodies are not contained within this action
	%<<TryBody:TrySize/binary, CatchBody:CatchSize/binary, FinallyBody:FinallySize/binary>> = R,
	{'try', [CatchNameOrRegister,
		{finallyBlockFlag, FinallyBlockFlag},
		{catchBlockFlag, CatchBlockFlag}
		%, {tryBody, TryBody}, {catchBody, CatchBody}, {finallyBody, FinallyBody}
		]};

action(16#94, <<Size:16/unsigned-integer-little>>) ->
	{with, [{size, Size}]};

action(16#96, <<0, B/binary>>) ->
	{S, _} = swfdt:string(B),
	{push, [{data, {'string', S}}]};
action(16#96, <<1, Value:32/signed-float-little>>) -> {push, [{data, {'float', Value}}]};
action(16#96, <<4, Value>>) -> {push, [{data, {registerNumber, Value}}]};
action(16#96, <<5, Value>>) -> {push, [{data, {'boolean', Value}}]};
action(16#96, <<6, Value:64/signed-float-little>>) -> {push, [{data, {'double', Value}}]};
action(16#96, <<7, Value:32/unsigned-integer-little>>) -> {push, [{data, {'integer', Value}}]};
action(16#96, <<8, Value>>) -> {push, [{data, {'constant8', Value}}]};
action(16#96, <<9, Value:16/unsigned-integer-little>>) -> {push, [{data, {'constant16', Value}}]};
action(16#96, <<Type, Value/binary>>) -> {push, [{data, {Type, Value}}]};

action(16#99, <<BranchOffset:16/signed-integer-little>>) ->
	{jump, [{branchOffset, BranchOffset}]};

action(16#9A, <<Method:2, _Reserved:4, LoadTargetFlag:1, LoadVariablesFlag:1>>) ->
	{getURL2, [{sendVarsMethod, Method}, {loadTargetFlag, LoadTargetFlag}, {loadVariablesFlag, LoadVariablesFlag}]};

action(16#9B, B) ->
	{Name, <<NumParams:16/unsigned-integer-little, R/binary>>} = swfdt:string(B),
	{Params, <<CodeSize:16/unsigned-integer-little>>} = swfdt:cstringpool(R, NumParams),
	{defineFunction, [{name, Name}, {params, Params}, {codeSize, CodeSize}]};

action(16#9D, <<BranchOffset:16/signed-integer-little>>) ->
	{'if', [{branchOffset, BranchOffset}]};

action(16#9E, _) ->
	{call, []};

action(16#9F, <<_Reserved:6, 0:1, PlayFlag:1>>) ->
	{gotoFrame2, [{playFlag, PlayFlag}]};
action(16#9F, <<_Reserved:6, 1:1, PlayFlag:1, SceneBias:16/unsigned-integer-little>>) ->
	{gotoFrame2, [{playFlag, PlayFlag}, {sceneBias, SceneBias}]};
	
action(_ActionCode, B) ->
	{unknownAction, [{raw, B}]}.


