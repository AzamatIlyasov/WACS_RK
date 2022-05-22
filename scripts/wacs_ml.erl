% %%-----------------------------------------------------------------
% %% This script is executed at server side. The programming language
% %% is Erlang.
% %% The module must export method "on_event/1" or "on_event/2" . 
% %% The on_event/2 should be used if your code needs to keep some state between
% %% calls.
% %%
% %% The callback "on_event" is called by the faceplate only in run-time. 
% %% As the trigger might be used timer and/or change of the value of some field 
% %% of the tag.
% %% If the event is triggered by the timer then the first argument of the on_event 
% %% callback will be:
% %%    {on_cycle,Cycle} - Cycle is period (ms).
% %% If the event is triggered by the change of the value of the linked tag field 
% %% then the first argument of the on_event will be:
% %%    {tag,TagID,Field,Value} - TagID - OID of the tag
% %%                              Field - name of the field
% %%                              Value - current value of the field. 
% %% The returned value is considered being state and passed as the second argument
% %% at the next call.
% %%-----------------------------------------------------------------

-module(wacs_ml).

-include("fp_struct.hrl").

-export([on_event/2]).

-export([
    request/3,
    response/2,
    weather/2,

    req_pp/3,
    resp_pp/2
]).

-define(HOUR, 3600).

on_event(_vent, State)->
   % DUMMY
   State.



%% request_new
request(Consumer, TS, #{ 
    "archives" := Archives0,
    "regressors":=Regressors0
})->
    % Path settings
    ArchivesPath = <<Consumer/binary,"/archivesP/">>,
    FuturePath = <<Consumer/binary,"/futureP/">>,
    TagPath = <<Consumer/binary,"/model_control/">>,
    %for debuging
    IsDebug = fp:get_value(TagPath, "isDebug"),
    
    case IsDebug of 
    {ok,true}-> 
        ?LOGINFO("DEBUG wacs_ml: TagPath ~p Request IsDebug ~p", [ fp_db:to_path(TagPath), IsDebug]),
        ?LOGINFO("DEBUG wacs_ml :  ~p , path ~p", ["Request data from archive", TagPath]);
    _ -> ok
    end,
    
    #{
        <<"config">>:= Config0,
        <<"history_length">>:= History0,
        <<"future_length">>:=Future0,
        <<"granularity">>:=Granularity0,
        <<"title">>:=Point0
    } = fp_db:read_fields( fp_db:open(<<Consumer/binary,"/model_settings">>),[
        <<"config">>,
        <<"history_length">>,
        <<"future_length">>,
        <<"granularity">>,
        <<"title">>
    ] ),
    
    ModelPath = <<Consumer/binary,"/model">>,
    
    %%Config00="{    'growth': 'linear',    'changepoint_prior_scale': 30,    'seasonality_prior_scale': 35,    'interval_width': 0.98,    'daily_seasonality': false,    'weekly_seasonality': false,    'yearly_seasonality': false,    'seasonality': [        {            'name': 'hour',            'period': 0.0417,            'fourier_order': 5        }    ]}",
    %  Config0list=binary:bin_to_list(Config0),
    %  Config1 = fp_db:to_json(term, Config0list),   
    %  ?LOGINFO("DEBUG wacs_ml :  Config0 ~p", [Config0]),
    %  ?LOGINFO("DEBUG wacs_ml :  Config0list ~p", [Config0list]),
     Config1 = Config0, 
    % Get the selected model
    ResReq = 
     case fp:get_value(ModelPath,"path") of
        none->
            none;
        {ok,<<"MODELS/",Model/binary>>}->
            
            % Check settings
            History = if is_number(History0)->History0; true-> 96 * ?HOUR end,
            Future = if is_number(Future0)->Future0; true-> 48 * ?HOUR end,
            Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
            
            % The time point to which read the history 
            To = next_ts( TS, Granularity*1000 ) - Granularity*1000,  %in ms
            
            % case IsDebug of 
            % {ok,true}-> ?LOGINFO("DEBUG wacs_ml :  ~p", ["Start read data from archive"]);
            % _ -> ok
            % end,
            % Read the archives
            %Archives0 = "p_load_sc",
            Archives = [ [<<ArchivesPath/binary,(unicode:characters_to_binary(A))/binary>>,<<"avg">>] || A <- Archives0 ],
            
            HistoryData0 = fp:archives_get_periods(#{
                <<"step_unit">> => <<"second">>, %
                <<"step_size">> => Granularity,
                <<"step_count">> => History div Granularity,
                <<"end">> => To
            }, Archives),
            % case IsDebug of 
            % {ok,true}-> ?LOGINFO("DEBUG wacs_ml :  Readed archives ~p ", [Archives]);
            % _ -> ok
            % end,
            % Handle 'none' values as 'null'
            HistoryData1 = fp_db:to_json(term, HistoryData0),
            %case IsDebug of {ok,true}-> ?LOGINFO("DEBUG wacs_ml :  Data from history archive ~p", [HistoryData1]) end,
            
            % Dempher the values in archives
            % HistoryData = wacs_util:time_lag( HistoryData1, #{gain=>1, time_lag => 5} ),
            % ?LOGINFO("DEBUG wacs_ml :  HistoryData ~p",[HistoryData]),
            
            % Read the regressors if they are defined
            % case IsDebug of 
            % {ok,true}-> ?LOGINFO("DEBUG wacs_ml :  ~p", [FuturePath]);
            % _ -> ok
            % end,
            FutureData = 
                if
                    is_list(Regressors0)->
                        Regressors = [ [<<FuturePath/binary,(unicode:characters_to_binary(R))/binary>>,<<"avg">>] || R <- Regressors0 ],
                        % case IsDebug of 
                        % {ok,true}-> ?LOGINFO("DEBUG wacs_ml :  regressors ~p", [Regressors]);
                        % _ -> ok
                        % end,
                        FutureData0 = fp:archives_get_periods(#{
                            <<"step_unit">> => <<"second">>,
                            <<"step_size">> => Granularity,
                            <<"step_count">> => Future div Granularity,
                            <<"start">> => To
                        }, Regressors),
                        % Handle 'none' values as 'null'
                        FutureData1 = fp_db:to_json(term, FutureData0);
                        % wacs_util:time_lag( FutureData1, #{gain=>1, time_lag => 5} );
                    true->
                        null
                end,
            
            case IsDebug of 
                {ok,true}-> 
            %     ?LOGINFO("DEBUG wacs_ml :  ~p ", ["Create JSON"]) ,
            %     ?LOGINFO("DEBUG wacs_ml :  Fut ~p ", [FutureData]),
            %     ?LOGINFO("DEBUG wacs_ml :  Gran ~p ", [Granularity]),
            %     ?LOGINFO("DEBUG wacs_ml :  Model ~p", [Model]),
            %     ?LOGINFO("DEBUG wacs_ml :  HistoryData1 ~p", [HistoryData1]),
                ?LOGINFO("DEBUG wacs_ml :  H ~p G ~p HdG ~p To ~p", [History,Granularity, History div Granularity,To]);
                %?LOGINFO("DEBUG wacs_ml :  Config1 ~p", [Config1]);
                _ -> ok
            end,
            
            #{
                "type"=> <<"PROPHET">>,
                "point"=> Point0,
                "data"=> HistoryData1,
                "regressor"=> FutureData,
                "config" => Config1,
                "features"=> none
             }
           
     end,
     
    % case IsDebug of 
    %     {ok,true}-> ?LOGINFO("DEBUG wacs_ml :  Req DONE result ~p ",[ResReq]);
    %     _ -> ok
    % end,
    
    ResReq.

    
response(Consumer, #{
    "predict" := Predict0,
    "data" := Data,
    "name" := ArchiveNames
})->
    %?LOGINFO("DEBUG wacs_ml: response start ~p, ~p, ~p, ~p", [Consumer, Predict0, Data, ArchiveNames]),
    TagPath = <<Consumer/binary,"/model_control/">>,
    
    %for debuging
    IsDebug = fp:get_value(TagPath, "isDebug"),
    ?LOGINFO("DEBUG wacs_ml: TagPath ~p Response IsDebug ~p", [ fp_db:to_path(TagPath), IsDebug]),
    
    case IsDebug of 
        {ok,true}-> 
        ?LOGINFO("DEBUG wacs_ml :  ~p , path ~p", ["RESPONSE data", TagPath]);
    _ -> ok
    end,
    
    #{
        <<"granularity">>:=Granularity0
    } = fp_db:read_fields( fp_db:open(<<Consumer/binary,"/model_settings">>),[
        <<"granularity">>
    ] ),
    
    % case IsDebug of 
    % {ok,true}-> ?LOGINFO("DEBUG wacs_ml : response List of archive names ~p", [ArchiveNames]);
    % _ -> ok
    % end,
    
    % QLoadFactor = 0.3, % fp:get_value(NtagPath, "q_load_k1"), %field: q_load_k1
    
    % case IsDebug of 
    %     {ok,true}-> ?LOGINFO("DEBUG wacs_ml :  predict ~p",[Predict0]);
    %     _ -> ok
    % end,
    
    Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
    
    % Add the timestamp
    [LastTS|_] = lists:last( Data ),
    
    case IsDebug of 
        {ok,true}-> ?LOGINFO("DEBUG wacs_ml :  last ts ~p",[LastTS]) ;
        _ -> ok
    end,
    
    Predict = 
        [ {LastTS + N*Granularity*1000, V} || {N, V } <- lists:zip( lists:seq(1,length(Predict0)), Predict0) ],
        
    ArchivePredict = 
        <<Consumer/binary, "/futureP/", (unicode:characters_to_binary(hd(ArchiveNames)))/binary>>,
    % case IsDebug of 
    %     {ok,true}-> ?LOGINFO("DEBUG wacs_ml : future insert Arch ~p Pred ~p",[ArchivePredict, Predict ]) ;
    %     _ -> ok
    % end,
    
    try 
        ResIns = fp_archive:insert_values(ArchivePredict, Predict )
        , case IsDebug of 
            {ok,true}-> ?LOGINFO("DEBUG wacs_ml : ResInsert ~p ",[ResIns]);
            _ -> ok
        end
    catch
        Error -> ?LOGINFO("DEBUG wacs_ml :  ERROR ~p ",[Error])
    end,
    
    
    % % Save Q predict to the archive
    % ResQResp = 
    %  case ArchiveNames of
    %     [_,Q_archive|_]->
    %         ArchiveQload = 
    %             <<Consumer/binary, "/futureP/", (unicode:characters_to_binary(Q_archive))/binary>>,
    %         fp_archive:insert_values(ArchiveQload, [ {T, V * QLoadFactor} || {T,V} <- Predict ] );
    %     _->
    %         ignore
    %  end,
    %  ?LOGINFO("DEBUG wacs_ml :  _Path ~p RESPONSE DONE result ~p ",[fp_db:to_path(TagPath), ResResp]),
    % case IsDebug of {ok,true}-> 
    %     ?LOGINFO("DEBUG wacs_ml :  RESPONSE DONE result ~p ",[ResResp]) ;
    % _ -> ok
    % end,
  ok.
  
  
weather(Archive, #{
    "granularity" := Granularity0,
    "temp":=Value,
    "ts":=TS0
})->

    Granularity = if is_number(Granularity0)->Granularity0 * 1000; true-> 1 * ?HOUR * 1000 end,
    
    TS = [ begin 
                ?LOGINFO("DEBUG wacs_ml: weather ts ~p",[T]),
                next_ts( fp_lib:parse_dt(T * 1000), Granularity ) - Granularity 
            end  || T <- TS0 ],
    
    Values = lists:zip( TS, Value ),
    ?LOGINFO("DEBUG wacs_ml: values ~p",[Values]),
    
    Res = fp_archive:insert_values(Archive ,Values),
    ?LOGINFO("DEBUG wacs_ml: res ~p",[Res]),
    
    Values.


create_edit() ->
    fp_db:query("get .oid, .path, v_zd from root where .pattern=$oid('/root/.patterns/Nodes_EE') ")

    Request_CE = #{
        "status": "Create_Edit",
        "ThreePhase": false,
        "is_estimate": false,    
        "is_inverted_in_service_val":true,
        "elements": Elements
        }
    Request.


req_sim() ->
    Elements = {},
    Request = #{
        "status": "SIM_REQUEST",
        "ThreePhase": false,
        "is_estimate": false,    
        "is_inverted_in_service_val":true,
        "elements": Elements
        }
    Request.


resp_sim() ->
    ok.




find_tags()->
    %from fp_db
     %index, name, etype - bus, vn_kv, type-b, in_service, zone
    Bus=fp_db:query(<<"get .oid, .path, nodenum_n, name_n, u_nom, state_n, zone from root where and( .pattern=$oid('/root/.patterns/Nodes_EE'), isDisabled_n=false )">>),
    
     %index, name, etype - load, bus (index), p_mw, q_mvar, in_service
    Load=fp_db:query(<<"get .oid, .path, nodenum_n, name_n, p_load, q_load, state_n  from root where and( .pattern=$oid('/root/.patterns/Nodes_EE'), isDisabled_n=false )">>),
    
     %index, name, etype - gen, bus (index), p_mw, vm_pu, sn_mva(none), in_service, min_q_mvar, max_q_mvar
    Gen=fp_db:query(<<"get .oid, .path, nodenum_n, name_n, p_gen, vz_zd, state_n, min_q, max_q from root where and( .pattern=$oid('/root/.patterns/Nodes_EE'), isDisabled_n=false )">>),
    
     %index, name, etype - shunt, bus (index), p_mw(0), q_mvar, in_service
    Shunt=fp_db:query(<<"get .oid, .path, nodenum_n, name_n, p, q, state_n from root where and( .pattern=$oid('/root/.patterns/Reactors_EE'), isDisabled_n=false )">>),
    
     %index, name, etype - line, from_bus (index), to_bus (index), length_km, r_om_per_km, x_om_per_km, c_nf_per_km, max_i_ka, parallel, type(ol), in_service
    Line=fp_db:query(<<"get .oid, .path, nodenum_n, name_n, u_nom, state_n, zone from root where and( .pattern=$oid('/root/.patterns/Branches_EE'), isDisabled_n=false )">>),
    
    %to json

    %for only tag - where disable is false
    %  "25": {
    %     "name": "ЭГРЭС-1-500",
    %     "etype": "bus",
    %     "vn_kv": 500,
    %     "type": "b",
    %     "in_service": false,
    %     "zone": 2
    %   },
    ResQuery.

next_ts(TS, Cycle) ->
    (TS div Cycle) * Cycle + Cycle.
    
    
    