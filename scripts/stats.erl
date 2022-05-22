%%-----------------------------------------------------------------
%% This script is executed at server side. The programming language
%% is Erlang.
%% The module must export method "on_event/1" or "on_event/2" . 
%% The on_event/2 should be used if your code needs to keep some state between
%% calls.
%%
%% The callback "on_event" is called by the faceplate only in run-time. 
%% As the trigger might be used timer and/or change of the value of some field 
%% of the tag.
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

-module(stats).

-include("fp_struct.hrl").

-export([
    on_event/2,
    mean/1,
    std/2,
    max_func/1,
    min_func/1,
    nan_values/1
    ]).

on_event(_,State)->
    % DD = [[1622717269990,49.915391666666665,none,49.91535611111111],[1622720869990,50.275058888888886,none,50.27496555555555],[1622724469990,49.76509111111111,none,49.76505138888889],[1622728069990,49.98508,none,49.98509972222222],[1622731669990,50.28871888888889,none,50.288578333333334],[1622735269990,49.6949975,2340.3562370258537,49.69502305555555],[1622738869990,50.05501388888889,2335.5158252867936,50.05493277777778],[1622742469990,50.16232194444444,2286.0,50.16226638888889],[1622746069990,49.76515222222222,2294.7694138888887,49.76501527777778],[1622749669990,50.12514361111111,2299.3916575,50.125101666666666],[1622753269990,50.036026666666665,2333.483205,50.03588083333333],[1622756869990,49.83510861111111,2285.397458888889,49.835010833333335],[1622760469990,50.194944444444445,2271.0,50.195101944444446],[1622764069990,49.90941055555555,2326.008103333333,49.90929361111111],[1622767669990,49.90502636250732,2297.7473724298256,49.90481722222222],[1622771269990,50.26519,2338.0220444444444,50.2650725],[1622774869990,49.78303666666667,2290.411343333333,49.78299194444445],[1622778469990,49.975115277777775,2333.1969991666665,49.975078333333336],[1622782069990,50.30664111111111,2344.8111233333334,50.306545],[1622785669990,49.68509611111111,2341.6083425,49.68505888888889],[1622789269990,50.04489638888889,2278.0196638888888,50.04511222222222],[1622792869990,50.18044138888889,2274.755506666667,50.180505555555555],[1622796469990,49.755074722222226,2261.2583916666667,49.754915277777776],[1622800069990,50.115098333333336,2346.288577777778,50.1150775]],
%   SS = mean(DD),
%   fp:log(info, "AAA SS: ~p",[SS]),
   State.
% -------------------- %
mean(Data)->
    [D|_] = Data,
    Acc0 = lists:duplicate(length(D)-1, {0,0}),
    mean(Data,Acc0).

mean([], Acc) ->
    [  Sum/Cnt ||{Sum,Cnt}<-Acc];
    
mean([[_DT|Elems]|Rest], Acc) ->
    Acc1=
        [begin
            if 
                El =:= none -> {0,0};
                true -> {El, 1}
            end
        end || El <- Elems],
    Acc_res = lists:zipwith(fun({A,B}, {A1,B1}) -> {A1+A, B1+B} end, Acc, Acc1),
    mean(Rest ,Acc_res).

% -------------------- %  
    
std(Data, MeanValues)->
    [D|_] = Data,
    Acc0 = lists:duplicate(length(D)-1, {0,0}),
    Acc1 = lists:zipwith(fun({A,B}, MeanV) -> {A, B, MeanV} end, Acc0, MeanValues),
    std_func(Data,Acc1).
 
std_func([], Acc) ->
    [  math:sqrt(Sum/Cnt) ||{Sum,Cnt,_}<-Acc];
    
std_func([[_DT|Elems]|Rest], Acc) ->
    Acc1=
        [begin
            if 
                El =:= none -> {0,0};
                true -> {El, 1}
            end
        end || El <- Elems],
    Acc_res = 
        lists:zipwith(fun({SumDiff,Cnt,MV}, {V,C}) -> 
                if 
                    C =:= 0 -> {SumDiff,Cnt,MV};
                    true -> {SumDiff+ math:pow(V-MV,2), Cnt +1, MV}
                end        
            end, Acc, Acc1),
    std_func(Rest ,Acc_res).
% -------------------- %

max_func(Data)->
    [D|_] = Data,
    Acc0 = lists:duplicate(length(D)-1, 0),
    max_func(Data, Acc0).
    
max_func([], Acc) -> Acc;
max_func([[_DT|Elems]|Rest], Acc)->
    Acc1=
        [begin
            if 
                El =:= none -> 0;
                true -> El
            end
        end || El <- Elems],
    Acc_res = 
        lists:zipwith(
            fun(A, B) -> 
                if 
                    A > B -> A;
                    true -> B
                end
            end, Acc, Acc1),
    max_func(Rest,Acc_res).

% -------------------- %    
    
min_func(Data)->
    [D|_] = Data,
    Acc0 = lists:duplicate(length(D)-1, 0),
    min_func(Data,Acc0).
    
min_func([], Acc)->
    Acc;
min_func([[_DT|Elems]|Rest], Acc)->
    Acc1=
        [begin
            if 
                El =:= none -> 0;
                true -> El
            end
        end || El <- Elems],
    Acc_res = 
        lists:zipwith(
            fun(A, B) -> 
                if 
                    A < B -> A;
                    true -> B
                end
            end, Acc, Acc1),
    min_func(Rest,Acc_res).

% -------------------- %

nan_values(Data)->
    [D|_] = Data,
    Acc0 = lists:duplicate(length(D)-1, 0),
    nan_values(Data, Acc0).

nan_values([], Acc) -> Acc;
nan_values([[_DT|Elems]|Rest], Acc)->
    Acc1=
        [begin
            if 
                El =:= none -> 1;
                true -> 0
            end
        end || El <- Elems],
    Acc_res = lists:zipwith(fun(A, B) -> A + B end, Acc, Acc1),
    nan_values(Rest,Acc_res).

