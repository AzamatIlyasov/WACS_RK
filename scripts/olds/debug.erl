%% Script must be an Erlang fun of arity 2.
%% - VARS argument is a map that contains
%%     values for the defined variables.
%%     Key - variable name, Value - variable value
%% - State is an argument that can be used as an accumulator.
%%     It keeps the value of the accumulator that was returned
%%     from the previous call
%% - Set Value in order to set a new value for a field, you need to return an tuple, 
%%     the first parameter is a map of variables and values, the second parameter is a state.
%%     for example: { #{'var_2' => Var_1}, _State }
%% The function must return a tuple:
%%      { <values_map>, <accumulator> }
%%
fun(VARS,State)->
    %code:purge(dlss_storage_supervisor),
    %code:load_file(dlss_storage_supervisor),
    TS = fp_lib:get_datetime()-3600*1000,
    fp:log(info, "DEBUG: RADV test wacs_predict:predict_next_day(TS= ~p)", [TS]),
    SnapshotRadv = wacs_snapshot:get_snapshot(TS),
    %wacs_predict:predict_next_day(TS),
    fp:log(info, "DEBUG: RADV test wacs_predict:predict_next_day(TS) result ~p",[SnapshotRadv]),
    %wacs_predict:predict_next_day( fp_lib:get_datetime() ),
    %fp:log(info, "DEBUG: PREDICT_FINISH"),
    {#{}, State}
    
end.