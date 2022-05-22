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

-module(wacs_predict).

-include("fp_struct.hrl").

-define(HOUR,3600000).
-define(DAY,86400000).
-define(TIMEZONE,6).% 6
-define(BEGINOPERSUTKI,5). %5
-define(SDVIG_ARCHIVE,0). %6
-define(STEPTIME,23). %для 24 часа - 23.

-define(UTILITY_CMD_DIR," cd /mnt/poolFP/files/radv/main/ ").
-define(UTILITY_CMD1," ./1_clean.sh "). %for clean before runs 24 utils
-define(UTILITY_CMD4," ./4_clean_end.sh "). %for clean after runs 24 utils

-export([
  on_event/2
]).

-export([
  predict_next_day/1,
  load_plan/2
]).

on_event(_Event, State)->
  State.
  
  
  
load_plan(_ID, Archives)->
    % Sum the same archives
    ArchivesZip = 
        lists:foldl(fun([ A | Values ], Acc)->
            ValuesAcc = maps:get( A, Acc, [ [T,0] || [T,_V] <- Values] ),
            Values1 = 
                [ [T, V + VAcc] || {[T,V], [_T,VAcc]} <- lists:zip( Values, ValuesAcc ) ],
            Acc#{ A => Values1 } 
        end,#{}, Archives),
    
    ?LOGINFO("DEBUG: Archives Size ~p",[ length(Archives) ]),
    ?LOGINFO("DEBUG: ArchivesZip Size ~p",[ maps:size(ArchivesZip) ]),
    
    [ begin 
        ?LOGINFO("DEBUG: load plan ~p ",[A]),% [{T,V} || [T,V] <- Values ] ]),
        
        %exclusion - this archives should be divide
            StateInsert = 
                case A of 
                    <<"/root/PROJECT/TAGS//Nodes/EGRES-1_F/u220/futureP/predict_p_gen">> -> 
                        ?LOGINFO("DEBUG: load 2 plan EGRES1 ~ts, values ~p ",[A, [{T,V} || [T,V] <- Values ] ]),
                        Aeg20 = <<"/root/PROJECT/TAGS//Nodes/EGRES-1_F/u20/futureP/predict_p_gen">> ,
                        In1=fp_archive:insert_values(A, [{T,V*0.14} || [T,V] <- Values ]),
                        In2=fp_archive:insert_values(Aeg20, [{T,V*0.86} || [T,V] <- Values ]),
                        {"insert",In1,In2};
                        
                    _ ->  fp_archive:insert_values(A, [{T,V} || [T,V] <- Values ])
                end,
        ?LOGINFO("DEBUG: StateInsert ~p ",[StateInsert])
        
    end || { A , Values } <- maps:to_list(ArchivesZip)],
    
    
    %exclusion - this archives should be divide EEC500 and EEC220 + acc
    try 
        ValEECs500 = maps:get(<<"/root/PROJECT/TAGS//Nodes/EEC_F/bl5-8/futureP/predict_p_gen">>, ArchivesZip),
        %?LOGINFO("DEBUG: ValEECs500 ~p ",[ValEECs500]),
        fp_archive:insert_values(<<"/root/PROJECT/TAGS//Nodes/EEC_F/bl5-8/futureP/predict_p_gen">>, [{Teec500,Veec500*0.5} || [Teec500, Veec500] <- ValEECs500]),
        ValEECs220 = maps:get(<<"/root/PROJECT/TAGS//Nodes/EEC_F/u220/futureP/predict_p_gen">>, ArchivesZip),
        %?LOGINFO("DEBUG: ValEECs220 ~p ",[ValEECs220])
        ResIns = fp_archive:insert_values(<<"/root/PROJECT/TAGS//Nodes/EEC_F/u220/futureP/predict_p_gen">>, 
            [{Teec220,Veec220+Veec500*0.5} || [Teec500, Veec500] <- ValEECs500, [Teec220,Veec220]<-ValEECs220, Teec500 =:= Teec220 ] ),
        ?LOGINFO("DEBUG: ResIns EECu220 ~p ",[ResIns])
    catch
        _: Error: Stack->
                ?LOGINFO("DEBUG: not eec500 in map: ~p, ~p",[ Stack, Error ])
    end,
    
    ok.
    
    

predict_next_day( TS )->
    %1634860800000    %1634825293215
    TS0 = next_ts( TS, ?DAY )- (?TIMEZONE * ?HOUR - ?BEGINOPERSUTKI * ?HOUR ),  % +6 is a local time zone - begin next day at 5 am o'cloc,k

    Points = lists:seq( TS0, TS0 + ?STEPTIME*?HOUR, ?HOUR ),
    
    {{_Y,_M,_D},{Hour,_,_}} = calendar:system_time_to_universal_time(TS,1000),
    <<YYYY:4/binary,"-",MM:2/binary,"-",DD:2/binary," ",HH:2/binary,_/binary>> = fp_lib:dt_to_local( TS ),
    HHStr = binary_to_list(HH),%integer_to_list(Hour),
    DDStr = binary_to_list(DD),
    MMStr = binary_to_list(MM),
    YYYYStr = binary_to_list(YYYY),
    YMDStr = YYYYStr++MMStr++DDStr,
    
    UTILITY_CMD1 = ?UTILITY_CMD_DIR ++ " && "++ ?UTILITY_CMD1 ++ " F" ++ YMDStr,
    ?LOGINFO("DEBUG: YMDStr ~p UTILITY_CMD1 ~p ",[ YMDStr, UTILITY_CMD1 ]),
    ResRun1 = os:cmd( UTILITY_CMD1 ),
    ?LOGINFO("DEBUG: ResRun CMD1 ~p ",[ ResRun1 ]),

    Snapshots = 
        [try
            ?LOGINFO("DEBUG: wacs_predict get snapshot for point ~ts",[ fp_lib:dt_to_string(P) ]),
            Snapshot = wacs_snapshot:get_snapshot( P ),
            ?LOGINFO("DEBUG: save snapshot for ~ts",[ fp_lib:dt_to_string(P) ]),
            {P, Snapshot}
        catch
            _: Error: Stack->
                ?LOGERROR("ERROR take snapshot error: ~p, ~p",[ Stack, Error ]),
                error
        end|| P <- Points],
    
    ?LOGINFO("DEBUG: RADV save snapshots: ~p, good  ~p", [length(Snapshots), length([ok || {_TS,_Data} <- Snapshots])]),
    save_snapshots( Snapshots ),
    
    ?LOGINFO("DEBUG: update archives"),
    update_archives( Snapshots ),
    
    UTILITY_CMD4 = ?UTILITY_CMD_DIR ++ " && "++ ?UTILITY_CMD4 ++ " F" ++ YMDStr,
    ?LOGINFO("DEBUG: END UTILITY_CMD4 ~p ",[ UTILITY_CMD4 ]),
    ResRun4 = os:cmd( UTILITY_CMD4 ),
    ?LOGINFO("DEBUG: ResRun END CMD4 ~p ",[ ResRun4 ]),
    ok.



save_snapshots( Snapshots )->
    % Purge previous results - TODO - purge catalogs after some time
    %% fp_db:query("delete from root where .folder=$oid('/root/PROJECT/CATALOGS/SNAPSHOT')"),
    % Create new snapshots
    [ save_snapshot( TS, Data ) || {TS, Data} <-Snapshots ],
    ok.
    
save_snapshot( TS, Data )->
  Name = <<YYYY:4/binary,"-",MM:2/binary,"-",DD:2/binary," ",HH:2/binary,_/binary>> = fp_lib:dt_to_local( TS ),
  ID = binary_to_integer( <<YYYY/binary,MM/binary,DD/binary,HH/binary>> ),
  ?LOGINFO("DEBUG: snapshot id ~p ",[ ID ]),
  %fp:log(info,"DEBUG: snapshot id ~p, data ~p",[ ID, Data ]),
  
  fp_db:edit_or_create(#{
    <<".name">> => Name,
    <<".folder">> => ?OID(<<"/root/PROJECT/CATALOGS/SNAPSHOT">>),
    <<".pattern">> => ?OID(<<"/root/.patterns/SNAPSHOT">>),
    <<"id">> => ID,
    <<"data">> => fp:to_json( Data )
  }),
  ok.
  
  
  
update_archives( Snapshots )->
    [try
        Values0=
            [ {TS, fp_lib:json_path(Path, JSON, 0) } || {TS, JSON} <- Snapshots ],
        %?LOGINFO("DEBUG: RADV insert snapshot values into ~ts path ~p ",[ A, Path ]),
        %?LOGINFO("DEBUG: RADV BEFORE insert values into ~ts path ~p Values0 ~p",[ A, Path, Values0 ]),
        KorK = 1, %for korrection
        Values = [ {_TS0,KorK*Val0} || {_TS0,Val0} <- Values0],
        %Val1 = 1.12*Val0,
        %Values = [{_TS0,Val1}],
        ResIns = fp_archive:insert_values( <<"/root/PROJECT/TAGS/Sections",A/binary>>, Values ),
        ?LOGINFO("DEBUG: RADV insert snapshot values into ~ts path ~p ResIns ~p",[ A, Path, ResIns ])
     catch
        _:Error:Stack->
            ?LOGERROR("ERROR RADV dump archive from shapshot error ~p, ~p , A ~p , Path ~p ",[ Stack, Error, A, Path ])
     end || { A, Path } <- maps:to_list( archives_config() ) ],
    ok.
    
    
    
archives_config()->
    #{
        
        <<"/ss1/futureP/predict_p_pp_uz_s">> => [<<"pred">>, <<"ss1">>, <<"p_pp_jak_s">> ],
        <<"/ss1/futureP/predict_p_pp_kz_s">> => [<<"pred">>, <<"ss1">>, <<"p_pp_kz_s">> ],
        <<"/ss1/futureP/predict_p_pp_kn_s">> => [<<"pred">>, <<"ss1">>, <<"p_pp_kn_s">> ],
        <<"/ss1/futureP/predict_p_mdp_kzu_s">> => [<<"pred">>, <<"ss1">>, <<"p_mdp_jak_s">> ],
        <<"/ss1/futureP/predict_p_mdp_kz_s">> => [<<"pred">>, <<"ss1">>, <<"p_mdp_kz_s">> ],
        <<"/ss1/futureP/predict_p_mdp_kn_s">> => [<<"pred">>, <<"ss1">>, <<"p_mdp_kn_s">> ],
        <<"/ss1/futureP/predict_kzu_norm_s">> => [<<"pred">>, <<"ss1">>, <<"kzu_kz_s">> ],
        <<"/ss1/futureP/predict_kzu_jakobian_s">> => [<<"pred">>, <<"ss1">>, <<"kzu_jak_s">> ],
        <<"/ss1/futureP/predict_kn_s">> => [<<"pred">>, <<"ss1">>, <<"kzu_kn_s">> ],
        <<"/ss1/futureP/predict_p_section">> => [<<"pred">>, <<"ss1">>, <<"p_fact">> ],
        
        <<"/ss2/futureP/predict_p_pp_uz_s">> => [<<"pred">>, <<"ss2">>, <<"p_pp_jak_s">> ],
        <<"/ss2/futureP/predict_p_pp_kz_s">> => [<<"pred">>, <<"ss2">>, <<"p_pp_kz_s">> ],
        <<"/ss2/futureP/predict_p_pp_kn_s">> => [<<"pred">>, <<"ss2">>, <<"p_pp_kn_s">> ],
        <<"/ss2/futureP/predict_p_mdp_kzu_s">> => [<<"pred">>, <<"ss2">>, <<"p_mdp_jak_s">> ],
        <<"/ss2/futureP/predict_p_mdp_kz_s">> => [<<"pred">>, <<"ss2">>, <<"p_mdp_kz_s">> ],
        <<"/ss2/futureP/predict_p_mdp_kn_s">> => [<<"pred">>, <<"ss2">>, <<"p_mdp_kn_s">> ],
        <<"/ss2/futureP/predict_kzu_norm_s">> => [<<"pred">>, <<"ss2">>, <<"kzu_kz_s">> ],
        <<"/ss2/futureP/predict_kzu_jakobian_s">> => [<<"pred">>, <<"ss2">>, <<"kzu_jak_s">> ],
        <<"/ss2/futureP/predict_kn_s">> => [<<"pred">>, <<"ss2">>, <<"kzu_kn_s">> ],
        <<"/ss2/futureP/predict_p_section">> => [<<"pred">>, <<"ss2">>, <<"p_fact">> ],
        
        <<"/ss3/futureP/predict_p_pp_uz_s">> => [<<"pred">>, <<"ss3">>, <<"p_pp_jak_s">> ],
        <<"/ss3/futureP/predict_p_pp_kz_s">> => [<<"pred">>, <<"ss3">>, <<"p_pp_kz_s">> ],
        <<"/ss3/futureP/predict_p_pp_kn_s">> => [<<"pred">>, <<"ss3">>, <<"p_pp_kn_s">> ],
        <<"/ss3/futureP/predict_p_mdp_kzu_s">> => [<<"pred">>, <<"ss3">>, <<"p_mdp_jak_s">> ],
        <<"/ss3/futureP/predict_p_mdp_kz_s">> => [<<"pred">>, <<"ss3">>, <<"p_mdp_kz_s">> ],
        <<"/ss3/futureP/predict_p_mdp_kn_s">> => [<<"pred">>, <<"ss3">>, <<"p_mdp_kn_s">> ],
        <<"/ss3/futureP/predict_kzu_norm_s">> => [<<"pred">>, <<"ss3">>, <<"kzu_kz_s">> ],
        <<"/ss3/futureP/predict_kzu_jakobian_s">> => [<<"pred">>, <<"ss3">>, <<"kzu_jak_s">> ],
        <<"/ss3/futureP/predict_kn_s">> => [<<"pred">>, <<"ss3">>, <<"kzu_kn_s">> ],
        <<"/ss3/futureP/predict_p_section">> => [<<"pred">>, <<"ss3">>, <<"p_fact">> ]
    }.



next_ts(TS, Cycle) ->
  (TS div Cycle) * Cycle + Cycle.


