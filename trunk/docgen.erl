#!/usr/bin/env escript

main([TargetDir|Files]) ->
	edoc:files(Files, [{dir, TargetDir}]).

