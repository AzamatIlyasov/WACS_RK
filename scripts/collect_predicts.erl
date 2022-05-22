%% If the event is triggered by the timer then the first argument of the on_event 
%% callback will be:
%%    {on_cycle,Cycle} - Cycle is period (ms).
%% If the event is triggered by the change of the value of the linked tag field 
%% then the first argument of the on_event will be:
%%    {tag,TagID,Field,Value} - TagID - OID of the tag
%%                              Field - name of the field
%%                              Value - current value of the field. 
%% The returned value is considered being state and passed as the second argument
%% at the next call.
%%-----------------------------------------------------------------

-module(collect_predicts).

-include("fp_struct.hrl").

-export([
    on_event/2,
    collect/1
    ]).
    
-define(COLLECT_TAG,<<"/TAGS/WACS_(FP)/Predict_EE">>).

on_event({on_cycle,_Cycle},State)->
   % here should be your code that is executed each Cycle milliseconds
%   collect([<<"/TAGS/WACS/pqw">>]),
   State;
on_event(_,State)->
   % here should be your code that is executed on change of the Field of the Tag
   State.

% {_Header, Archives0} = fp_db:query("get .oid, .name from root where .folder=$oid('"++ArchivesPath++"') order by .name asc"),

collect(Folders)->
    Arr = collect(Folders, []),
    Arr1 = fp:to_json(#{value => Arr}),
    ok = fp:set_value(?COLLECT_TAG, "value", Arr1),
    HeadToCut = get_point(Arr),
    ok = fp:set_value(?COLLECT_TAG, "define", HeadToCut).
    
    
collect([], Acc) ->
    lists:reverse(Acc);
collect([Folder|Rest], Acc) ->
    {ok, JSON} = fp:get_value(<<Folder/binary,"/predict">>, "value"),
    #{
       <<"request">> := _Request,
       <<"predict">> := Predict
   } = fp:from_json(JSON),
   fp:log(info,"AAA prdict: ~p", [Predict]),
    collect(Rest, [Predict|Acc]).
    
get_point([Values|_]) ->
    Len = length(Values),
    fp:log(info,"AAA Len: ~p", [Len]),
    get_point(Values,0).
% calendar:system_time_to_local_time(1622841508605,millisecond)
get_point([],Acc) -> Acc;
get_point([[DT,_]|Rest], Acc)->
    {_, {H,_,_}}=calendar:system_time_to_local_time(DT,millisecond),
    fp:log(info,"AAA H: ~p",[H]),
    if
        H =:= 0 -> get_point([],Acc+1);
        true -> get_point(Rest, Acc+1)
    end.
