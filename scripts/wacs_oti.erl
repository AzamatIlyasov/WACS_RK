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

-module(wacs_oti).

-include("fp_struct.hrl").

-export([on_event/2]).

-export([
    selU/3,
    selU/5, 
    selU/7, 
    sum_for_oti/2,
    sum_for_oti/3,
    selVal/2,
    selVal/3,
    ots_vl/2,
    selTS_for_ots/2,
    selRes_for_ots/2,
    state_vl/2
]).

on_event(_vent, State)->
   % DUMMY
   State.

   
selU(_Path, Val1in, Qds1in) ->
    % %fp:log(info, "DEBUG: test_wacs_oti_selV for ~p : val1:~p, qds1:~p , val2:~p, qds2:~p",[fp_db:to_path(Path), Val1,Qds1,Val2,Qds2]),
    % {Value, Qds} = {Val1, Qds1}, %%% временно!!!!
    Val1 = 
        case Val1in of 
            none -> 0;
            -1 -> 0.1;
            _ -> Val1in
        end,
   
    Qds1 = 
        case Qds1in of 
            none -> 0;
            -1 -> 16;
            _ -> Qds1in
        end,
    Value = Val1, 
    Qds = Qds1,
    
    %fp:log(info, "DEBUG: selU END for ~p : selVal:~p, selQds:~p",[Path, Value,Qds]),
    {Value, Qds}.


selU(_Path, Val1in, Qds1in, Val2in, Qds2in) ->
    % %fp:log(info, "DEBUG: test_wacs_oti_selV for ~p : val1:~p, qds1:~p , val2:~p, qds2:~p",[fp_db:to_path(Path), Val1,Qds1,Val2,Qds2]),
    % {Value, Qds} = {Val1, Qds1}, %%% временно!!!!
    Val1 = 
        case Val1in of 
            none -> 0;
            -1 -> 0.1;
            _ -> Val1in
        end,
    Val2 = 
        case Val2in of 
            none -> 0;
            -1 -> 0.1;
            _ -> Val2in
        end,
    Qds1 = 
        case Qds1in of 
            none -> 0;
            -1 -> 16;
            _ -> Qds1in
        end,
    Qds2 = 
        case Qds2in of 
            none -> 0;
            -1 -> 16;
            _ -> Qds2in
        end,
    if 
        Val1 =:= none -> 
            Value = Val2,
            Qds = Qds2;
        Val2 =:= none -> 
            Value = Val1, 
            Qds = Qds1;
        Val1 < 0 -> 
            Value = Val2, 
            Qds = Qds2;
        Val2 < 0 -> 
            Value = Val1, 
            Qds = Qds1;
        Val1 > Val2 ->
            Value = Val1, 
            Qds = Qds1;
        Val2 > Val1 -> 
            Value = Val2, 
            Qds = Qds2;
        Val2 =:= Val1 ->
            Value = Val2, 
            Qds = Qds2;
        true -> 
            Value = -0.8, 
            Qds = 32
    end,
    %fp:log(info, "DEBUG: selU END for ~p : selVal:~p, selQds:~p",[Path, Value,Qds]),
    {Value, Qds}.

selU(_Path, Val1in, Qds1in, Val2in, Qds2in, Val3in, Qds3in) ->
    %fp:log(info, "DEBUG: test_wacs_oti_selV for ~p : val1:~p, qds1:~p , val2:~p, qds2:~p",[Path, Val1,Qds1,Val2,Qds2]),
    Val1 = 
        case Val1in of 
            none -> 0;
            -1 -> 0.1;
            _ -> Val1in
        end,
    Val2 = 
        case Val2in of 
            none -> 0;
            -1 -> 0.1;
            _ -> Val2in
        end,
    Val3 = 
        case Val3in of 
            none -> 0;
            -1 -> 0.1;
            _ -> Val3in
        end,
    Qds1 = 
        case Qds1in of 
            none -> 0;
            -1 -> 16;
            _ -> Qds1in
        end,
    Qds2 = 
        case Qds2in of 
            none -> 0;
            -1 -> 16;
            _ -> Qds2in
        end,
    Qds3 = 
        case Qds3in of 
            none -> 0;
            -1 -> 16;
            _ -> Qds3in
        end,
    if 
        Val1 =:= none ->
            Value = Val2,
            Qds = Qds2;
        Val2 =:= none ->
            Value = Val1, 
            Qds = Qds1;
        Val1 < 0 -> 
            Value = Val2, 
            Qds = Qds2;
        Val2 < 0 -> 
            Value = Val1, 
            Qds = Qds1;
        Val1 > Val2 -> 
            Value = Val1, 
            Qds = Qds1;
        Val2 > Val1 -> 
            Value = Val2, 
            Qds = Qds2;
        Val2 =:= Val1 ->
            Value = Val2, 
            Qds = Qds2;
        Val3 > Val2 ->
            Value = Val3, 
            Qds = Qds3;
        Val3 =:= Val2 -> 
            Value = Val3, 
            Qds = Qds3;
        true -> 
            Value = -0.9, 
            Qds = 32
    end,
    %fp:log(info, "DEBUG: selU END for ~p : selVal:~p, selQds:~p",[Path, Value,Qds]),
    {Value, Qds}.



%% for select one of two elements
selVal(Path, Elements) ->
    selVal(Path, Elements, 0,0).

selVal(_, [], Acc1,Acc1s) -> 
    Acc={Acc1,Acc1s},
    case Acc of 
        {0,0} -> 
            Res=0,Rs=0;
        {0,-1} -> 
            Res=0,Rs=16;
        {-1,-1} -> 
            Res=0,Rs=16;
        {_,-1} -> 
            Res=0,Rs=16;
        {Res,0}-> 
            Rs=0;
        {Res,_}-> 
            Rs=16
    end,
    %fp:log(debug,"DEBUG wacs_oti selVal: 5 END END - Res ~p Rs ~p", [Res,Rs]),
    {Res,Rs};
selVal(_, Elements, _Acc1,_Acc1s) ->
    [Vals|Rest] = Elements,
    case Vals of 
        {none, none} -> 
            selVal([], Rest, 0, 0);
        {none, Vs} ->    
            selVal([], Rest, 0, Vs);
        {V, none} ->    
            selVal([], Rest, V, 0);
        {V, -1} ->       
            selVal([], Rest, V, 16);
        {V, 0} ->       
            selVal([], Rest, V, 0);
        {V, Vs} ->       
            selVal([], Rest, V, Vs)
    end.



%% forReactors
selVal(_Path, IsManual, Elements) ->
    {SV,SVqds} = Elements,
    Result = 
        case SV of 
            none -> 0;
            -1 -> 0;
            _ -> SV
        end,
    Result_qds = 
            case SVqds of 
                none -> 16;
                -1 -> 16;
                _ -> SVqds
            end, 
    MapManual = #{},
    MapAuto = #{"res" => Result, "res_status" => Result_qds },
    ValuesMap = 
        case IsManual of 
            false -> MapAuto;
            true ->  MapManual;
            _ -> MapAuto
        end,  
    ValuesMap. 
 
 
 
%% for sum of elemetns with params {v1, -1}ex
%% {resval,qds} = sum_for_oti(Path, VARS, Params) 
%%  params = [{v1, -1},...] - for what is vars need negative value
sum_for_oti(Path, VARS, Params) -> 
    
    V1=wacs_util:maps_get_params("v1",VARS, none, Params),
    V1qds=maps:get("v1s",VARS, -1),

    V2=wacs_util:maps_get_params("v2",VARS, none, Params),
    V2qds=maps:get("v2s",VARS, -1),

    V3=wacs_util:maps_get_params("v3",VARS, none, Params),
    V3qds=maps:get("v3s",VARS, -1),

    V4=wacs_util:maps_get_params("v4",VARS, none, Params),
    V4qds=maps:get("v4s",VARS, -1),

    V5=wacs_util:maps_get_params("v5",VARS, none, Params),
    V5qds=maps:get("v5s",VARS, -1),

    V6=wacs_util:maps_get_params("v6",VARS, none, Params),
    V6qds=maps:get("v6s",VARS, -1),

    V7=wacs_util:maps_get_params("v7",VARS, none, Params),
    V7qds=maps:get("v7s",VARS, -1),

    V8=wacs_util:maps_get_params("v8",VARS, none, Params),
    V8qds=maps:get("v8s",VARS, -1),
    
    V9=wacs_util:maps_get_params("v9",VARS, none, Params),
    V9qds=maps:get("v9s",VARS, -1),
    
    V10=wacs_util:maps_get_params("v10",VARS, none, Params),
    V10qds=maps:get("v10s",VARS, -1),
    
    V11=wacs_util:maps_get_params("v11",VARS, none, Params),
    V11qds=maps:get("v11s",VARS, -1),
    
    %WAMS
    WV1=wacs_util:maps_get_params("wv1",VARS, none, Params),
    WV1qds=maps:get("wv1s",VARS, -1),

    Elements = [ {WV1,WV1qds}, {V1,V1qds}, {V2,V2qds}, {V3,V3qds}, {V4,V4qds},
         {V5,V5qds}, {V6,V6qds}, {V7,V7qds}, {V8,V8qds}, {V9,V9qds}, {V10,V10qds}, {V11,V11qds} ],
         
    % case Path of
    % [{53,1271}] -> 
    %         ?LOGINFO("____"),
    %         ?LOGINFO("DEBUG g1: Path ~p  Elements = ~p ",[Path, Elements]);
    %     _ -> ok 
    % end,
    
    {Res, ResStatus} = sum_for_oti(Path, Elements),
    
    % case Path of
    %     [{53,1271}] -> ?LOGINFO("DEBUG g2: Path ~p RESULT = ~p ~p ",[Path, Res, ResStatus]),
    %         ?LOGINFO("____");
    %     _ -> ok 
    % end,
    case {Res, ResStatus} of
        {none,none} -> ?LOGWARNING("DEBUG WARNING G2: NONE NONE Path ~p RESULT = ~p ~p ",[Path, Res, ResStatus]);
        {none,16} ->   ?LOGWARNING("DEBUG WARNING G2: NONE 16 Path ~p RESULT = ~p ~p ",[Path, Res, ResStatus]);
        {-1,-1} -> ?LOGWARNING("DEBUG WARNING G2: -1 -1 Path ~p RESULT = ~p ~p ",[Path, Res, ResStatus]);
        _ -> ok 
    end,
    %[_Path| [_Res| [_ResStatus| ElList ] ] ] = maps:to_list(Elements),
    % ParamsList = Params,
    % ?LOGINFO("DEBUG: Path ~p ",[Path]),
    % % ?LOGINFO("DEBUG: Path1 ~p ",[Path1]),
    % ?LOGINFO("DEBUG: ElList ~p ",[ElList]),
    % ?LOGINFO("DEBUG: Params ~p ",[Params]),
    % {Res,ResStatus}=sum_for_oti(Path, ElList, ParamsList, {0,0}),
    % ?LOGINFO("DEBUG: Res ~p ResStatus ~p ",[Res,ResStatus]),
    {Res,ResStatus}.

% sum_for_oti(Path, [], ParamsList, {Acc1,Acc1s})->    
%     % ?LOGINFO("DEBUG: Path ~p ",[Path]),
%     % ?LOGINFO("DEBUG: ElList ~p ",[ElList]),
%     % ?LOGINFO("DEBUG: Params ~p ",[Params]),
%     %maps:fold(fun(K, V, Acc) -> [V | Acc] end, [], M).
%     ok,
%     {0,128};
% sum_for_oti(Path, ElList, ParamsList, {Acc1,Acc1s})->
%     [P1|_]=ParamsList,
%     ok,
%     {0,128}.
 
%% invert value - defined in params
% %% check_params_val([{V,Qds},...]},[{v, -1},...])
% check_params_val(ElementsList,ParamsList)->
%     [ begin 

%         case V of 
%             {v1,_} -> V1 = -maps:get("v1",VARS);
%             {v2,_} -> V2 = -maps:get("v2",VARS);
%             {v3,_} -> V3 = -maps:get("v3",VARS);
%             {v4,_} -> V4 = -maps:get("v4",VARS);
%             {v5,_} -> V5 = -maps:get("v5",VARS);
%             {v6,_} -> V6 = -maps:get("v6",VARS);
%             {v7,_} -> V7 = -maps:get("v7",VARS);
%             {v8,_} -> V8 = -maps:get("v8",VARS);
%         end

%       end || V <- ParamsList],
%     % lists:filter(, Params),
%     % -V.
%     % TODO - comment - we nee creat a sctipt, where find Params in ElList and invert this value
%     ElementsList.


 
%% for sum of elemetns
sum_for_oti(Path, Elements) -> 
    % case Path of 
    %     [{53,1387}] -> 
    %         ?LOGINFO("_Path 0 ",[Path, Elements]);
    %     _ -> ok
    % end,
    sum_for_oti_old(Path, Elements, {0,0}).

sum_for_oti_old(_Path, [], {Acc0,Acc0s}) -> 
    Ac0={Acc0,Acc0s},
    case Ac0 of 
        % {0,0} -> 
        %     Res=0,Rs=0;
        % {0,-1} ->
        %     Res=0,Rs=16;
        % {-1,-1} ->
        %     Res=0,Rs=16;
        {Res,none} ->
            Rs=16;
        {none,Rs} ->
            Res=0;
        {Res,-1} ->
            Rs=16;
        % {Res,0}-> 
        %     Rs=0;
        % {0,_}-> 
        %     Res=0,Rs=16;
        {Res,Rs}-> 
            ok
    end,
    Result = {Res,Rs},
    %fp:log(info,"DEBUG: 5 SUM_OTI END END - Res ~p Rs ~p", [Res,Rs]),
    % case _Path of 
    %     [{53,1271}] -> 
    %         ?LOGINFO("DEBUG: END _Path ~p Ac0 ~p Res ~p Rs ~p ",[_Path, Ac0, Res, Rs]);
    %     _ -> ok
    % end,
     case Result of 
        {none,16} -> 
            ?LOGWARNING("DEBUG WARNING: END NONE 16 _Path ~p Ac0 ~p Res ~p Rs ~p ",[_Path, Ac0, Res, Rs]);
        % {0,16} -> 
        %     ?LOGWARNING("DEBUG WARNING: END ZERO 16 _Path ~p Ac0 ~p Res ~p Rs ~p ",[_Path, Ac0, Res, Rs]);
        {-1,-1} -> 
            ?LOGWARNING("DEBUG WARNING: END -1 -1 _Path ~p Ac0 ~p Res ~p Rs ~p ",[_Path, Ac0, Res, Rs]);
        {none,none} -> 
            ?LOGWARNING("DEBUG WARNING: END NONE NONE _Path ~p Ac0 ~p Res ~p Rs ~p ",[_Path, Ac0, Res, Rs]);
        _ -> ok
    end,
    {Res,Rs};
sum_for_oti_old(_Path, Elements, {Acc1,Acc1s}) ->
    [Vals|Rest] = Elements,
    Ac1 = {Acc1,Acc1s},
    case Ac1 of 
        {none, none} -> 
             Acc2=0, Acc2s=0;
        {Acc2,none} ->
            Acc2s=16;
        {none,Acc2s} ->
            Acc2=0;  
        {Acc2,-1} ->
            Acc2s=16;
        % {Res,0}-> 
        %     Rs=0;
        % {0,_}-> 
        %     Res=0,Rs=16;
        {Acc2,Acc2s}-> 
            ok
    end,
    
    % case _Path of 
    %     [{53,1271}] -> 
    %         ?LOGINFO("DEBUG: EL _Path ~p Vals ~p Ac1 ~p Acc1 ~p Acc1s ~p Acc2 ~p Acc2s ~p Rest ~p ",[_Path, Vals, Ac1, Acc1, Acc1s, Acc2, Acc2s, Rest]);
    %     _ -> ok
    % end,
    %Path1 = fp_db:to_path(_Path),
    case Vals of 
        {none, none} -> 
            % case _Path of 
            %     [{53,1271}] -> 
            %         ?LOGINFO("DEBUG: EL _Path {none, none} ");
            %     _ -> ok
            % end,
            sum_for_oti_old(_Path, Rest, {0+Acc2, Acc2s});
        {none, Vs2} ->
            % case _Path of 
            %     [{53,1271}] -> 
            %         ?LOGINFO("DEBUG: EL _Path {none, Vs2} ",[none, Vs2]);
            %     _ -> ok
            % end,
            sum_for_oti_old(_Path, Rest, {0+Acc2, Vs2});
        {V1, none} ->
            % case _Path of 
            %     [{53,1271}] -> 
            %         ?LOGINFO("DEBUG: EL _Path {V1, none} ",[V1, none]);
            %     _ -> ok
            % end,
            sum_for_oti_old(_Path, Rest, {V1+Acc2, 16});
        {-1, -1} ->
            % case _Path of 
            %     [{53,1271}] -> 
            %         ?LOGINFO("DEBUG: EL _Path {-1, -1} ");
            %     _ -> ok
            % end,
            sum_for_oti_old(_Path, Rest, {0+Acc2, 16});
        {V2, -1} ->
            % case _Path of 
            %     [{53,1271}] -> 
            %         ?LOGINFO("DEBUG: EL _Path {V2, -1} ",[V2, -1]);
            %     _ -> ok
            % end,
            sum_for_oti_old(_Path, Rest, {V2+Acc2, 16});
        % {V, 0} ->
        %     sum_for_oti([], Rest, {V+Acc2, 0});
        {V3, Vs3} ->
            % case _Path of 
            %     [{53,1271}] -> 
            %         ?LOGINFO("DEBUG: EL _Path {V3, Vs3} ",[V3, Vs3]);
            %     _ -> ok
            % end,
            sum_for_oti_old(_Path, Rest, {V3+Acc2, Vs3})
    end.
    



%% for ots for two vl or great
state_vl(Path, Elements) ->
    %fp:log(info,"DEBUG: SUM_OTI for ~p el:~p ", [Path, Elements]),
    Result = 0,
    Result_qds = 16,
    {Result, Result_qds}.
    
 
 
%% for ots from two susbstaions
ots_vl(Path, Elements) -> 
    %fp:log(info,"DEBUG: OTS for ~p el:~p ", [Path, Elements]),
    Result = 0,
    Result_qds = 16,
    {Result, Result_qds}.


%% for select TS of two elements
selTS_for_ots(Path, Elements) ->
%{SV1, SV2} = wacs_oti:selTS_for_ots(Path, [ {V1,V1qds},{V2,V2qds} ]),
%{true/false}
    % ?LOGINFO("DEBUG:test selTSOTS P ~p Elements ~p ",[Path, Elements]),
    Res = 
     [begin 
        % ?LOGINFO("DEBUG:testOTS V ~p Vqds ~p ",[V,Vqds]),
        case V of 
            1 -> false;
            2 -> true;
            Other -> false
        end
     end || {V,Vqds}<-Elements],
    ResTuple = list_to_tuple(Res),
    % ?LOGINFO("DEBUG:testOTS Res ~p ~p ",[Res, ResTuple]),
    ResTuple.


%     selTS_for_ots(Path, Elements, 0,0).

% selTS_for_ots(_, [], Acc1,Acc1s) -> 
%     Acc={Acc1,Acc1s},
%     case Acc of 
%         {0,0} -> 
%             Res=0,Rs=0;
%         {0,-1} -> 
%             Res=0,Rs=16;
%         {-1,-1} -> 
%             Res=0,Rs=16;
%         {_,-1} -> 
%             Res=0,Rs=16;
%         {Res,0}-> 
%             Rs=0;
%         {Res,_}-> 
%             Rs=16
%     end,
%     %fp:log(debug,"DEBUG wacs_oti selVal: 5 END END - Res ~p Rs ~p", [Res,Rs]),
%     {Res,Rs};
% selTS_for_ots(_, Elements, Acc1,Acc1s) ->
%     [Vals|Rest] = Elements,

%     Result = 
%         case Vals of 
%             {none, none} -> 
%                 selTS_for_ots([], Rest, 0, 0),
%                 {false, }
%             {none, Vs} ->    
%                 selTS_for_ots([], Rest, 0, Vs);
%             {V, none} ->    
%                 selTS_for_ots([], Rest, V, 0);
%             {V, -1} ->       
%                 selTS_for_ots([], Rest, V, 16);
%             {V, 0} ->       
%                 selTS_for_ots([], Rest, V, 0);
%             {V, Vs} ->       
%                 selTS_for_ots([], Rest, V, Vs)
%         end.

   
%% for select OTS of elements
selRes_for_ots(Path, RState) -> 
    % ?LOGINFO("DEBUG:test selRes P ~p RState ~p ",[Path, RState]),
    {Result, Result_qds} = 
        case RState of 
            true ->     {2,0}; %link On
            false ->    {1,0}; %link Off
            none ->     {0,16};
            Other ->    {0,32}
        end,
    % ?LOGINFO("DEBUG:test selRes Result ~p , Result_qds ~p ",[Result, Result_qds]),
    {Result, Result_qds}.
    
    % selRes_for_ots(Path, Elements, {0,0}).

% selRes_for_ots(_, [], {Acc1,Acc1s}) -> 
%     Acc={Acc1,Acc1s},
%     case Acc of 
%         {0,0} -> 
%             Res=0,Rs=0;
%         {0,-1} ->
%             Res=0,Rs=16;
%         {-1,-1} ->
%             Res=0,Rs=16;
%         {_,-1} ->
%             Res=0,Rs=16;
%         {Res,0}-> 
%             Rs=0;
%         {0,_}-> 
%             Res=0,Rs=16;
%         {Res,_}-> 
%             Rs=16
%     end,
%     %fp:log(info,"DEBUG: 5 SUM_OTI END END - Res ~p Rs ~p", [Res,Rs]),
%     {Res,Rs};
% selRes_for_ots(_, Elements, {Acc1,Acc1s}) ->
%     [Vals|Rest] = Elements,
%     case Vals of 
%         {none, none} -> 
%             selRes_for_ots([], Rest, {0, 0});
%         {none, Vs} ->
%             selRes_for_ots([], Rest, {0, Vs});
%         {V, none} ->
%             selRes_for_ots([], Rest, {0+Acc1, 0});
%         {V, -1} ->
%             selRes_for_ots([], Rest, {V+Acc1, 0});
%         {V, 0} ->
%             selRes_for_ots([], Rest, {V+Acc1, 0});
%         {V, Vs} -> 
%             selRes_for_ots([], Rest, {V+Acc1, Vs})
%     end.
    