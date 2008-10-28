-record(abcfile, {version, cpool, method, metadata, instance, class, script, method_body}).
-record(cpool, {integer, uinteger, double, string, namespace, ns_set, multiname}).
-record(method, {param_count, return_type, param_type, name, flags, flags_arr, options, param_names}).
-record(instance, {name, super_name, flags, flags_arr, protected_ns, interface, iinit, trait}).

-record(trait, {name, type, data, metadata}).
-record(trait_slot, {slot_id, type_name, value}). %% trait slot and const
-record(trait_class, {slot_id, classi}).
-record(trait_function, {slot_id, functioni}).
-record(trait_method, {disp_id, methodi}). %% trait method, getter, setter

-record(class, {cinit, trait}).
-record(script, {init, trait}).
-record(exception, {from, to, target, exc_type, var_name}).

-record(instr, {addr, name, args}).
-record(method_body, {methodi, max_stack, local_count, init_scope_depth, max_scope_depth, code, exception, trait}).
