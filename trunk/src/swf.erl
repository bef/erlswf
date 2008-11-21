%%
%% simple swf analyzer
%%   author: Ben Fuhrmannek <bef@pentaphase.de>
%%   license: GPLv3 - http://www.gnu.org/licenses/licenses.html#GPL
%%

-module(swf).
-include("swf.hrl").

-export([
	debug/2,
	readfile/1,
	uncompress/1,
	headerdecode/1,
	tagsplit/1,
	tagdecode/1,
	parsetorawtags/1,
	swf/1,
	swffile/1,
	tag/2
	]).


%debug(Fmt, Args) -> io:format("DEBUG: " ++ Fmt, Args). %% display message
debug(_, _) -> foo. %% do nothing

%%
%% edoc type definitions
%%
%% @type paramlist() = [{key(), value()}] where key() = atom()
%% value() = any()
%%

%%
%% swf parser
%%

%% @doc read a whole file
%% @spec readfile(string()) -> binary()
readfile(Filename) ->
	{ok, B} = file:read_file(Filename),
	B.

%% @doc read swf type, version, file length and uncompress if necessary
%% @spec uncompress(binary()) -> binary() | atom()
%% @throws no_swf
uncompress(<<"FWS", Version, FileLength:32, B/binary>>) ->
	{rawswf1,
		#swfheader{type=fws, version=Version, filelength=FileLength},
		B};

uncompress(<<"CWS", Version, FileLength:32, B/binary>>) ->
	{rawswf1,
		#swfheader{type=cws, version=Version, filelength=FileLength},
		zlib:uncompress(B)};

uncompress(_) -> throw(no_swf).

%% @doc decode swf header
%% @spec headerdecode({rawswf(), header1(), binary()}) -> {rawswf, paramlist(), binary()}
headerdecode({rawswf1, Header1, Raw}) ->
	RawSize = size(Raw),
	{FrameSize, <<_FrameRateIgn:8,
				FrameRate:8/unsigned-integer,
				FrameCount:16/unsigned-integer-little,
				R/binary>>} = swfdt:rect(Raw),
	HeaderSize = RawSize - size(R),
	<<RawHeader:HeaderSize/binary, _/binary>> = Raw,
	
	%% assemble swf structure of the form {rawswf, Header, RawTags}
	{rawswf,
		Header1#swfheader{
			framesize=FrameSize,
			framerate=FrameRate,
			framecount=FrameCount,
			headersize=HeaderSize,
			rawheader=RawHeader},
		R}.

%% @doc split raw tag blob into tag list
%% @spec tagsplit(binary()) -> [rawtag()]
tagsplit(B) ->
	tagsplit(B, 0, []).

tagsplit(<<>>, _, Acc) ->
	lists:reverse(Acc);

tagsplit(<<TagCodeAndLength1:8, TagCodeAndLength2:8, B/binary>>, InPos, Acc) ->
	%% decode tag code and preliminary tag length
	<<TagCode:10, TagLength0:6>> = <<TagCodeAndLength2, TagCodeAndLength1>>,

	%% decode possibly overflowing length
	{TagLength, B2} = case TagLength0 of
		16#3f ->
			<<Length:32/signed-integer-little, R1/binary>> = B,
			{Length, R1};
		_ ->
			{TagLength0, B}
	end,
	
	%% cut tag by length
	{{Code, Name, Raw}, Rest} = tagcut(TagCode, TagLength, B2),
	debug("found tag: ~p~n", [{Code, Name}]),
	
	%% recalculate position in binary
	Pos = InPos + 2 + (size(B)-size(B2)), %% add bytes (tag code and length) to current binary position in taglist
	NewPos = InPos + 2 + (size(B) - size(Rest)),

	tagsplit(Rest, NewPos, [#tag{code=Code, name=Name, pos=Pos, raw=Raw} | Acc]).

tagcut(Code, Length, B) ->
	<<TB:Length/binary, R/binary>> = B,
	Name = tag(name, Code),
	{{Code, Name, TB}, R}.


%% @doc decode rawtag
%% @spec tagdecode(rawtag()) -> tag()
%%   where
%% tagcode() = integer()
%% pos() = integer()
%% raw() = binary()
tagdecode(#tag{code=Code, name=Name, pos=Pos, raw=Raw, contents=undefined} = InTag) ->
	DecodedTag = try tag(Code, Raw) of
		[unknown] ->
			error_logger:warning_msg("<~8.10.0B> [~3.10.0B] ~p : unable to decode tag correctly~n", [Pos, Code, Name]),
			[unknown];
		Val -> Val
	catch
		error:X ->
			error_logger:warning_msg("<~8.10.0B> [~3.10.0B] ~p : ~p~n     ~p~n", [Pos, Code, Name, X, erlang:get_stacktrace()]),
			[{error, {Code, Name, X}}]
	end,
	debug("decoded tag: ~p~n", [InTag#tag{contents=DecodedTag}]),
	InTag#tag{contents=DecodedTag};
tagdecode(#tag{contents=C} = Tag) when C =/= undefined ->
	Tag.

%%
%% convenience interface
%%

%% @doc swf blob to swf structure with rawtags
%% @spec parsetorawtags(binary()) -> {swf, swfheader(), [rawtag()]}
%%   where swfheader() = paramlist()
parsetorawtags(B) ->
	{rawswf, Header, RawTags} = headerdecode(uncompress(B)),
	{swf, Header, swf:tagsplit(RawTags)}.

%% @doc swf blob to swf structure with decoded tags
%% @spec swf(binary()) -> {swf, swfheader(), [tag()]}
swf(B) -> %% decode all tags
	{swf, Header, RawTags} = parsetorawtags(B),
	{swf, Header, lists:map(fun tagdecode/1, RawTags)}.

%% @doc convert swf file to fully decoded swf structure
swffile(Filename) ->
	swf(readfile(Filename)).




%%
%% tags
%%

%% @doc decode tag by code OR get name using tag(name, Code)
%% @spec tag(tagcode(), binary()) -> paromlist()
tag(name, 0) -> 'endTag';
tag(0, _) ->
	[];

tag(name, 1) -> 'showFrame';
tag(1, _) ->
	[];

tag(name, 2) -> 'defineShape';
tag(2, <<ShapeID:16/unsigned-integer-little, B/binary>>) ->
	{Bounds, ShapeWithStyle} = swfdt:rect(B),
	
	%% SHAPEWITHSTYLE
	{FSA, R1} = swfdt:fillstylearray(shape1, ShapeWithStyle),
	{LSA, _R2} = swfdt:linestylearray(shape1, R1),
	%{Shape, _} = style(R2),
	
	[{shapeID, ShapeID},
	{shapeBounds, Bounds},
	{fillStyles, FSA},
	{lineStyles, LSA},
	unimplemented];

%% tag 3 is unused

tag(name, 4) -> 'placeObject';
tag(4, <<CharacterID:16/unsigned-integer-little, Depth:16/unsigned-integer-little, B/binary>>) ->
	{M, R1} = swfdt:matrix(B),
	{CX, _} = swfdt:cxform(R1),
	[{characterID, CharacterID},
	{depth, Depth},
	{matrix, M},
	{colorTransform, CX}];

tag(name, 5) -> 'removeObject';
tag(5, <<CharacterID:16/unsigned-integer-little, Depth:16/unsigned-integer-little>>) ->
	[{characterID, CharacterID}, {depth, Depth}];

tag(name, 6) -> 'defineBits';
tag(6, <<CharacterID:16/unsigned-integer-little, Data/binary>>) ->
	[{characterID, CharacterID}, {jpegImage, Data}];

tag(name, 7) -> 'defineButton';
tag(7, <<ButtonID:16/unsigned-integer-little, B/binary>>) ->
	{BR, R1} = swfdt:buttonrecords(B),
	{actions, Actions, _} = swfaction:actionrecords(R1),
	[{buttonID, ButtonID}, {buttonrecords, BR}, {actions, Actions}];

tag(name, 8) -> 'jpegTables';
tag(8, Data) ->
	[{jpegEncodingTable, Data}];

tag(name, 9) -> 'setBackgroundColor';
tag(9, <<R, G, B, _/binary>>) ->
	[{rgb, R, G, B}];

tag(name, 10) -> 'defineFont';
tag(10, <<FontID:16/unsigned-integer-little, _/binary>>) ->
	[{fontID, FontID}, unimplemented];

tag(name, 11) -> 'defineText';
tag(11, <<CharacterID:16/unsigned-integer-little, B/binary>>) ->
	{TextBounds, R1} = swfdt:rect(B),
	{TextMatrix, <<_GlyphBits, _AdvanceBits, _R2/binary>>} = swfdt:matrix(R1),
	%{textrecords, TextRecords, _} = textrecords({GlyphBits, AdvanceBits, 0}, R2),
	[
		{characterID, CharacterID},
		{textBounds, TextBounds},
		{textMatrix, TextMatrix},
		unimplemented
	%	{textrecords, TextRecords}
	];

tag(name, 12) -> 'doAction';
tag(12, ActionRecord) ->
	{actions, Actions, _} = swfaction:actionrecords(ActionRecord),
	[{actions, Actions}];

tag(name, 13) -> 'defineFontInfo';
tag(13, <<FontID:16/unsigned-integer-little,
				FontNameLen,
				FontName:FontNameLen/binary,
				_FontFlagsReserved:2,
				SmallText:1,
				ShiftJIS:1,
				ANSI:1,
				Italic:1,
				Bold:1,
				WideCodes:1,
				CodeTable/binary>>) ->
	[
		{fontID, FontID},
		{fontName, binary_to_list(FontName)},
		{smallText, SmallText},
		{shiftJIS, ShiftJIS},
		{ansi, ANSI},
		{italic, Italic},
		{bold, Bold},
		{wideCodes, WideCodes},
		{codeTable, CodeTable}];

tag(name, 14) -> 'defineSound';
tag(14, <<ID:16/unsigned-integer-little,
		Format:4, %% 0=uncompressed, 1=ADPCM, 2=MP3, 3=uncompressen/little-endian, 6=Nellymoser
		Rate:2, %% 0=5.5, 1=11, 2=22, 3=44 kHz
		Size:1, %% 0=8bit, 1=16bit
		Type:1, %% 0=mono, 1=stereo
		SampleCount:32/unsigned-integer-little,
		B/binary>>) ->
	%DataSize = (Size * 8 + 8) * SampleCount,
	%debug("data size ~p, bin size ~p~n", [DataSize, size(B)]),
	%<<Data:DataSize/binary, _Unknown/binary>> = B, 
	[
		{id, ID},
		{format, Format},
		{rate, Rate},
		{size, Size},
		{type, Type},
		{sampleCount, SampleCount},
		{data, B}];

tag(name, 15) -> 'startSound';
tag(15, <<ID:16/unsigned-integer-little, B/binary>>) ->
	{soundinfo, SI} = swfdt:soundinfo(B),
	[{id, ID}, {info, SI}];

%% tag 16: stopSound (undocumented)

tag(name, 17) -> 'defineButtonSound';
tag(17, _) ->
	[unimplemented];

tag(name, 18) -> 'soundStreamHead';
tag(18, _) ->
	[unimplemented];

tag(name, 19) -> 'soundStreamBlock';
tag(19, _) ->
	[unimplemented];

tag(name, 20) -> 'defineBitsLossless';
tag(20, _) ->
	[unimplemented];

tag(name, 21) -> 'defineBitsJPEG2';
tag(21, <<CharacterID:16/unsigned-integer-little, Data/binary>>) ->
	[{characterID, CharacterID}, {jpegData, Data}];

tag(name, 22) -> 'defineShape2';
tag(22, _) ->
	[unimplemented];

tag(name, 23) -> 'defineButtonCxform';
tag(23, _) ->
	[unimplemented];

tag(name, 24) -> 'protect';
tag(24, <<>>) ->
	[unprotected];
tag(24, S) ->
	{MD5, _} = swfdt:string(S),
	[{md5password, MD5}];

tag(name, 25) -> 'pathsArePostscript';
tag(25, _) -> %% lacks documentation
	[unimplemented];

tag(name, 26) -> 'placeObject2';
tag(26, <<FlagHasClipActions:1,
		FlagHasClipDepth:1,
		FlagHasName:1,
		FlagHasRatio:1,
		FlagHasColorTransform:1,
		FlagHasMatrix:1,
		FlagHasCharacter:1,
		FlagMove:1,
		Depth:16/unsigned-integer-little,
		B/binary>>) ->
	{CharacterID, R1} = swfdt:condfun(FlagHasCharacter, fun swfdt:ui16/1, B),
	{Matrix, R2} = swfdt:condfun(FlagHasMatrix, fun swfdt:matrix/1, R1),
	{ColorTransform, R3} = swfdt:condfun(FlagHasColorTransform, fun swfdt:cxformwithalpha/1, R2),
	{Ratio, R4} = swfdt:condfun(FlagHasRatio, fun swfdt:ui16/1, R3),
	{Name, R5} = swfdt:condfun(FlagHasName, fun(B1) -> {S, Rf} = swfdt:string(B1), {S, Rf} end, R4),
	{ClipDepth, R6} = swfdt:condfun(FlagHasClipDepth, fun swfdt:ui16/1, R5),
	{ClipActions, _} = swfdt:condfun(FlagHasClipActions, fun swfdt:clipactions/1, R6),
	[{depth, Depth},
	{characterID, CharacterID},
	{matrix, Matrix},
	{colorTransform, ColorTransform},
	{ratio, Ratio},
	{name, Name},
	{clipDepth, ClipDepth},
	{flagMove, FlagMove},
	{clipActions, ClipActions},
	unimplemented];

%% tag 27 is unknown

tag(name, 28) -> 'removeObject2';
tag(28, <<Depth:16/unsigned-integer-little>>) ->
	[{depth, Depth}];

%% tag 29: syncFrame (undocumented)

%% tag 30 is unknown

%% tag 31: freeAll (undocumented)

tag(name, 32) -> 'defineShape3';
tag(32, _) ->
	[unimplemented];

tag(name, 33) -> 'defineText2';
tag(33, _) ->
	[unimplemented];

tag(name, 34) -> 'defineButton2';
tag(34, <<ButtonID:16/unsigned-integer-little,
		_:7, %% reserved
		TrackAsMenu:1,
		B/binary>>) ->
	<<ActionOffset:16/unsigned-integer-little, _R1/binary>> = B,
	<<CharB:ActionOffset/binary, ActionB/binary>> = B,
	{CharB, ActionB} = case ActionOffset of
		0 -> {B, <<>>};
		_ -> <<CB:ActionOffset/binary, AB/binary>> = B,
			{CB, AB}
	end,
	Characters = unimplemented,
	% {Characters, _} = swfdt:buttonrecords2(CharB),
	
	Actions = case ActionOffset of
		0 -> [];
		_ -> swfdt:buttoncondactions(ActionB)
	end,

	[
		{buttonID, ButtonID},
		{trackAsMenu, TrackAsMenu},
		{characters, Characters}
	] ++ Actions;

tag(name, 35) -> 'defineBitsJPEG3';
tag(35, <<	CharacterID:16/unsigned-integer-little,
			AlphaDataOffset:32/unsigned-integer-little,
			JPEGData:AlphaDataOffset/binary,
			BitmapAlphaData/binary>>) ->
	[
		{characterID, CharacterID},
		{jpegData, JPEGData},
		{bitmapAlphaData, zlib:uncompress(BitmapAlphaData)}];

tag(name, 36) -> 'defineBitsLossless2';
tag(36, _) ->
	[unimplemented];

tag(name, 37) -> 'defineEditText';
tag(37, <<	CharacterID:16/unsigned-integer-little, R/binary>>) ->
	{Bounds, R1} = swfdt:rect(R),
	<<Flagbits:16/integer-big, R2/binary>> = R1,
	Flags = lists:foldl(fun(I, FAcc) ->
		case Flagbits band (1 bsl (15-I)) of
			0 -> FAcc;
			_ ->
				case I of
					0 -> [hasText|FAcc];
					1 -> [wordWrap|FAcc];
					2 -> [multiline|FAcc];
					3 -> [password|FAcc];
					4 -> [readOnly|FAcc];
					5 -> [hasTextColor|FAcc];
					6 -> [hasMaxLength|FAcc];
					7 -> [hasFont|FAcc];
					8 -> [hasFontClass|FAcc];
					9 -> [autoSize|FAcc];
					10 -> [hasLayout|FAcc];
					11 -> [noSelect|FAcc];
					12 -> [border|FAcc];
					13 -> [wasStatic|FAcc];
					14 -> [html|FAcc];
					15 -> [useOutlines|FAcc]
				end
		end
	end, [], lists:seq(0,15)),
	{Opt, _Rest} = lists:foldl(fun(El, {Acc, B}) ->
		case El of
			a ->
				case lists:member(hasFont, Flags) of
					true -> {FontID, B1} = swfdt:ui16(B), {[{fontID, FontID}|Acc], B1};
					false -> {Acc, B}
				end;
			b ->
				case lists:member(hasFontClass, Flags) of
					true -> {FontClass, B1} = swfdt:string(B), {[{fontClass, FontClass}|Acc], B1};
					false -> {Acc, B}
				end;
			c ->
				case lists:member(hasFont, Flags) of
					true -> {FontHeight, B1} = swfdt:ui16(B), {[{fontHeight, FontHeight}|Acc], B1};
					false -> {Acc, B}
				end;
			d ->
				case lists:member(hasTextColor, Flags) of
					true -> {TextColor, B1} = swfdt:rgba(B), {[{textColor, TextColor}|Acc], B1};
					false -> {Acc, B}
				end;
			e ->
				case lists:member(hasMaxLength, Flags) of
					true -> {MaxLength, B1} = swfdt:ui16(B), {[{maxLength, MaxLength}|Acc], B1};
					false -> {Acc, B}
				end;
			f ->
				case lists:member(hasLayout, Flags) of
					true ->
						<<Align,
						LeftMargin:16/unsigned-integer-little,
						RightMargin:16/unsigned-integer-little,
						Indent:16/unsigned-integer-little,
						Leading:16/signed-integer-little,
						B1/binary>> = B,
						AlignName = case Align of
							0 -> left;
							1 -> right;
							2 -> center;
							3 -> justify;
							_ -> unknown
						end,
						{[{align, AlignName}, {leftMargin, LeftMargin}, {rightMargin, RightMargin}, {indent, Indent}, {leading, Leading}|Acc], B1};
					false -> {Acc, B}
				end;
			g ->
				{VarName, B1} = swfdt:string(B), {[{variableName, VarName}|Acc], B1};
			h ->
				case lists:member(hasText, Flags) of
					true -> {InitialText, B1} = swfdt:string(B), {[{initialText, InitialText}|Acc], B1};
					false -> {Acc, B}
				end
		end 
	end, {[], R2}, [a,b,c,d,e,f,g,h]),
	
	[
		{characterID, CharacterID},
		{bounds, Bounds},
		{flags, Flagbits},
		{flagsArr, Flags}] ++ Opt;

%% tag 38: defineVideo (undocumented)

tag(name, 39) -> 'defineSprite';
tag(39, _) ->
	[unimplemented];

%% tag 40: nameCharacter (undocumented)

tag(name, 41) -> 'productInfo';
tag(41, <<ProductID:32/unsigned-integer-little,
			Edition:32/unsigned-integer-little,
			MajorVersion,
			MinorVersion,
			BuildNumber:64/unsigned-integer-little,
			CompilationDate:64/unsigned-integer-little>>) -> %% ??
	[
			{productID, ProductID},
			{edition, Edition},
			{majorVersion, MajorVersion},
			{minorVersion, MinorVersion},
			{buildNumber, BuildNumber},
			{compilationDate, CompilationDate}];

%% tag 42: defineTextFormat (undocumented)

tag(name, 43) -> 'frameLabel';
tag(43, S) ->
	{Label, NA} = swfdt:string(S),
	[{namedAnchorFlag, NA}, {label, Label}];

%% tag 44 is unknown

tag(name, 45) -> 'soundStreamHead2';
tag(45, _) ->
	[unimplemented];

tag(name, 46) -> 'defineMorphShape';
tag(46, _) ->
	[unimplemented];

%% tag 47: generateFrame (undocumented)

tag(name, 48) -> 'defineFont2';
tag(48, _) ->
	[unimplemented];

%% tag 49: generatorCommand (undocumented)
%% tag 50: defineCommandObject (undocumented)
%% tag 51: characterSet (undocumented)
%% tag 52: externalFont (undocumented)
%% tags 53 to 55 are unknown

tag(name, 56) -> 'exportAssets';
tag(56, <<Count:16/unsigned-integer-little, Assets/binary>>) ->
	[{count, Count}, {assets, swfdt:assets(Assets)}];

tag(name, 57) -> 'importAssets';
tag(57, <<B/binary>>) ->
	{URL, <<Count:16/unsigned-integer-little, Assets/binary>>} = swfdt:string(B),
	[{count, Count}, {url, URL}, {assets, swfdt:assets(Assets)}];

tag(name, 58) -> 'enableDebugger';
tag(58, S) ->
	{MD5, _} = swfdt:string(S),
	[{md5password, MD5}];

tag(name, 59) -> 'doInitAction';
tag(59, <<SpriteID:16/unsigned-integer-little, ActionRecord/binary>>) ->
	{actions, Actions, _} = swfaction:actionrecords(ActionRecord),
	[{spriteID, SpriteID}, {actions, Actions}];

tag(name, 60) -> 'defineVideoStream';
tag(60, _) ->
	[unimplemented];

tag(name, 61) -> 'videoFrame';
tag(61, _) ->
	[unimplemented];

tag(name, 62) -> 'defineFontInfo2';
tag(62, <<FontID:16/unsigned-integer-little,
		FontNameLen,
		FontName:FontNameLen/binary,
		_FontFlagsReserved:2,
		SmallText:1,
		ShiftJIS:1, %% always 0
		ANSI:1, %% always 0
		Italic:1,
		Bold:1,
		WideCodes:1, %% always 1
		LanguageCode,
		CodeTable/binary>>) ->
	[
		{fontID, FontID},
		{fontName, binary_to_list(FontName)},
		{smallText, SmallText},
		{shiftJIS, ShiftJIS},
		{ansi, ANSI},
		{italic, Italic},
		{bold, Bold},
		{wideCodes, WideCodes},
		{languageCode, LanguageCode},
		{codeTable, CodeTable}];

%% tag 63: debugID (undocumented)

tag(name, 64) -> 'enableDebugger2';
tag(64, <<_Reserved:16, S/binary>>) ->
	{MD5, _} = swfdt:string(S),
	[{md5password, MD5}];

tag(name, 65) -> 'scriptLimits';
tag(65, <<MaxRecursionDepth:16/unsigned-integer-little, ScriptTimeoutSeconds:16/unsigned-integer-little>>) ->
	[{maxRecursionDepth, MaxRecursionDepth}, {scriptTimeoutSeconds, ScriptTimeoutSeconds}];

tag(name, 66) -> 'setTabIndex';
tag(66, <<Depth:16/unsigned-integer-little, TabIndex:16/unsigned-integer-little>>) ->
	[{depth, Depth}, {tabIndex, TabIndex}];

%% tags 67 to 68 are unknown

tag(name, 69) -> 'fileAttributes';
tag(69, <<_Reserved0:3, HasMetadata:1, ActionScript3:1, _Reserved0:2, UseNetwork:1, _Reserved0:24>>) ->
	[{hasMetadata, HasMetadata}, {actionScript3, ActionScript3}, {useNetwork, UseNetwork}];

tag(name, 70) -> 'placeObject3';
tag(70, <<
		%FlagHasClipActions:1,
		%FlagHasClipDepth:1,
		%FlagHasName:1,
		%FlagHasRatio:1,
		%FlagHasColorTransform:1,
		%FlagHasMatrix:1,
		%FlagHasCharacter:1,
		%FlagMove:1,
		%_Reserved:3,
		%FlagHasImage:1,
		%FlagHasClassName:1,
		%FlagHasCacheAsBitmap:1,
		%FlagHasBlendMode:1,
		%FlagHasFilterList:1,
		%Depth:16/unsigned-integer-little,
		_B/binary>>) ->
	[unimplemented];

tag(name, 71) -> 'importAssets2';
tag(71, <<B/binary>>) ->
	{URL, <<_Reserved1, _Reserved0, Count:16/unsigned-integer-little, Assets/binary>>} = swfdt:string(B),
	[{count, Count}, {url, URL}, {assets, swfdt:assets(Assets)}];

%% tag 72: doABC (undocumented)
tag(name, 72) -> doABC1;
tag(72, <<Data/binary>>) ->
	[{data, Data}];

tag(name, 73) -> 'defineFontAlignZones';
tag(73, <<FontID:16/unsigned-integer-little,
		CSMTableHint:2, _Reserved:6,
		_R/binary>>) ->
	Thickness = case CSMTableHint of
		0 -> thin;
		1 -> medium;
		2 -> thick;
		_ -> unknown
	end,
	[{fontID, FontID}, {csmTableHint, CSMTableHint}, {thickness, Thickness}, {zoneTable, unimplemented}];

tag(name, 74) -> 'csmTextSettings';
tag(74, <<TextID:16/unsigned-integer-little,
		UseFlashType:2, GridFit:3, _Reserved:3,
		Thickness:32/unsigned-float-little,
		Sharpness:32/unsigned-float-little,
		_Reserved2, _/binary>>) ->
	[
		{textID, TextID},
		{useFlashType, case UseFlashType of 0 -> normal; 1 -> advanced; _ -> unknown end},
		{gridFit, case GridFit of 0 -> none; 1 -> pixel; 2 -> subpixel; _ -> unknown end},
		{thickness, Thickness},
		{sharpness, Sharpness}
	];

tag(name, 75) -> 'defineFont2';
tag(75, _) ->
	[unimplemented];

tag(name, 76) -> 'symbolClass';
tag(76, <<NumSymbols:16/unsigned-integer-little, Assets/binary>>) ->
	[{numSymbols, NumSymbols}, {assets, swfdt:assets(Assets)}];

tag(name, 77) -> 'metadata';
tag(77, S) ->
	{Metadata, _} = swfdt:string(S),
	[{xml, Metadata}];

tag(name, 78) -> 'defineScalingGrid';
tag(78, <<CharacterID:16/unsigned-integer-little, R/binary>>) ->
	{Splitter, _} = swfdt:rect(R),
	[{characterID, CharacterID}, {splitter, Splitter}];

%% tags 79 to 81 are unknown

tag(name, 82) -> 'doABC'; %% sometimes referred to as 'doABC2'
tag(82, <<Flags:32/unsigned-integer-little, Data/binary>>) ->
	{Name, B} = swfdt:string(Data),
	[{flags, Flags}, {name, Name}, {data, B}];

tag(name, 83) -> 'defineShape4';
tag(83, _) ->
	[unimplemented];

tag(name, 84) -> 'defineMorphShape2';
tag(84, _) ->
	[unimplemented];

%% tag 85 is unknown

tag(name, 86) -> 'defineSceneAndFrameLabelData';
tag(86, B) ->
	{SceneCount, R1} = swfdt:encodedu32(B),
	{Scenes, R2} = swfdt:metaarray(R1, SceneCount, fun(B1) ->
		{Offset, RS1} = swfdt:encodedu32(B1),
		{Name, RS2} = swfdt:string(RS1),
		{{Offset, Name}, RS2}
		end),
	{FrameLabelCount, R3} = swfdt:encodedu32(R2),
	{Labels, _} = swfdt:metaarray(R3, FrameLabelCount, fun(B2) ->
		{Num, RL1} = swfdt:encodedu32(B2),
		{Label, RL2} = swfdt:string(RL1),
		{{Num, Label}, RL2}
		end),
	[{scenes, Scenes}, {labels, Labels}];

tag(name, 87) -> 'binaryData';
tag(87, <<Tag:16/unsigned-integer-little, _Reserved:32, Data/binary>>) ->
	[{tag, Tag}, {data, Data}];

tag(name, 88) -> 'defineFontName';
tag(88, <<FontID:16/unsigned-integer-little, S/binary>>) ->
	{Name, R} = swfdt:string(S),
	{Copyright, _} = swfdt:string(R),
	[{fontID, FontID}, {fontName, Name}, {fontCopyright, Copyright}];

tag(name, 89) -> 'startSound2';
tag(89, _) ->
	[unimplemented];

tag(name, _) ->	'unknownTag';
tag(_Code, _B) ->
	[unknown].

