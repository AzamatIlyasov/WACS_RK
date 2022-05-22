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

% -module(wacs_ml).

% -include("fp_struct.hrl").

% -export([on_event/2]).

% -export([
%     request/3,
%     response/2,
%     response_s/2,
%     weather/2
% ]).

% -define(HOUR, 3600).

% on_event(_vent, State)->
%   % DUMMY
%   State.

% request(Consumer, TS, #{ 
%     "history":= History0, 
%     "future":=Future0,
%     "granularity":=Granularity0,
%     "archives" := Archives0,
%     "regressors":=Regressors0
% })->
%     fp:log(info, "DEBUG wacs_ml :  ~p", ["Request data"]),
%     % Path settings
%     Root = "/root/PROJECT/TAGS/" ++ Consumer,
%     ArchivesPath = unicode:characters_to_binary( Root ++ "/archivesP/" ),
%     FuturePath = unicode:characters_to_binary( Root ++ "/futureP/" ),
    
%     ModelPath = Root ++ "/model",
    
%     fp:log(info, "DEBUG wacs_ml :  model_path ~p", [ModelPath]),
    
%     % Get the selected model
%     case fp:get_value(ModelPath,"path") of
%         none->
%             none;
%         {ok,<<"MODELS/",Model/binary>>}->
            
%             fp:log(info, "DEBUG wacs_ml :  model ~p", [Model]),
            
%             % Check settings
%             History = if is_number(History0)->History0; true-> 96 * ?HOUR end,
%             Future = if is_number(Future0)->Future0; true-> 48 * ?HOUR end,
%             Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
            
%             % The time point to which read the history 
%             To = next_ts( TS, Granularity ) - Granularity,
            
%             fp:log(info, "DEBUG wacs_ml :  to ~p", [To]),
    
%             % Read the archives
%             Archives = [ [<<ArchivesPath/binary,(unicode:characters_to_binary(A))/binary>>,<<"avg">>] || A <- Archives0 ],
%             HistoryData0 = fp:archives_get_periods(#{
%                 <<"step_unit">> => <<"second">>,
%                 <<"step_size">> => Granularity,
%                 <<"step_count">> => History div Granularity,
%                 <<"end">> => To
%             }, Archives),
%             % Handle 'none' values as 'null'
%             HistoryData = fp_db:to_json(term, HistoryData0),
            
%             % Read the regressors if they are defined
%             FutureData = 
%                 if
%                     is_list(Regressors0)->
%                         Regressors = [ [<<FuturePath/binary,(unicode:characters_to_binary(R))/binary>>,<<"avg">>] || R <- Regressors0 ],
%                         fp:log(info, "DEBUG wacs_ml :  regressors ~p", [Regressors]),
%                         FutureData0 = fp:archives_get_periods(#{
%                             <<"step_unit">> => <<"second">>,
%                             <<"step_size">> => Granularity,
%                             <<"step_count">> => Future div Granularity,
%                             <<"start">> => To
%                         }, Regressors),
%                         % Handle 'none' values as 'null'
%                         fp_db:to_json(term, FutureData0);
%                     true->
%                         null
%                 end,
            
%             %% [ [DT, V1, V2], [].. ]
            
%             #{
%                 "steps"=>Future div Granularity,
%                 "point"=> Model,
%                 "data"=> HistoryData,
%                 "regressor"=> FutureData,
%                 "time_freq"=> <<"H">>
%           }
%     end.
    
% response(Consumer, #{
%     "predict" := Predict0,
%     "data" := Data,
%     "granularity" := Granularity0
% })->
    
%     QLoadFactor = 0.3,
    
%     fp:log(info,"DEBUG wacs_ml :  predict ~p",[Predict0]),
    
%     Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
    
%     ArchivePload = 
%         <<"/root/PROJECT/TAGS/", (unicode:characters_to_binary(Consumer))/binary, "/futureP/predict_p_load">>,
        
%     ArchiveQload = 
%         <<"/root/PROJECT/TAGS/", (unicode:characters_to_binary(Consumer))/binary, "/futureP/predict_q_load">>,
    
%     % Add the timestamp
%     [LastTS|_] = lists:last( Data ),
%     fp:log(info,"DEBUG wacs_ml :  last ts ~p",[LastTS]),
%     Predict = 
%         [ {LastTS + N*Granularity*1000, V} || {N, V } <- lists:zip( lists:seq(1,length(Predict0)), Predict0) ],
        
%     % Save predict to the archive
%     fp:log(info, "DEBUG wacs_ml :  ~ts future insert ~p",[ArchivePload, Predict]),
%     fp_archive:insert_values(ArchivePload, Predict ),
%     fp_archive:insert_values(ArchiveQload, [ {T, V * QLoadFactor} || {T,V} <- Predict ] ),
    
%   % This is an example code, replace it with your code
%   #{
%       request => Data,
%       predict => Predict
%   }.

% response_s(Consumer, #{
%     "predict" := Predict0,
%     "data" := Data,
%     "granularity" := Granularity0
% })->
    
%     fp:log(info,"DEBUG wacs_ml :  predict ~p",[Predict0]),
    
%     Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
    
%     ArchivePload = 
%         <<"/root/PROJECT/TAGS/", (unicode:characters_to_binary(Consumer))/binary, "/futureP/predict_p_load_baseline">>,
    
%     % Add the timestamp
%     [LastTS|_] = lists:last( Data ),
%     fp:log(info,"DEBUG wacs_ml :  last ts ~p",[LastTS]),
%     Predict = 
%         [ {LastTS + N*Granularity*1000, V} || {N, V } <- lists:zip( lists:seq(1,length(Predict0)), Predict0) ],
        
%     % Save predict to the archive
%     fp:log(info, "DEBUG wacs_ml :  ~ts future insert ~p",[ArchivePload, Predict]),
%     fp_archive:insert_values(ArchivePload, Predict ),
    
%   % This is an example code, replace it with your code
%   #{
%       request => Data,
%       predict => Predict
%   }.
  
% weather(Archive, #{
%     "granularity" := Granularity0,
%     "temp":=Temp,
%     "ts":=TS0
% })->

%     Granularity = if is_number(Granularity0)->Granularity0 * 1000; true-> 1 * ?HOUR * 1000 end,
    
%     ArchivePath = 
%         case is_binary(Archive) of
%             true -> Archive;
%             _ -> <<"/root/PROJECT/TAGS/", (unicode:characters_to_binary(Archive))/binary>>
%         end,
    
%     TS = [ begin fp:log(info,"DEBUG wacs_ml :  weather ts ~p",[T]),next_ts( fp_lib:parse_dt(T * 1000), Granularity ) - Granularity end  || T <- TS0 ],
    
%     Values = lists:zip( TS, Temp ),
%     fp:log(info,"DEBUG wacs_ml :  values ~p",[Values]),
    
%     Res = fp_archive:insert_values(ArchivePath ,Values),
%     fp:log(info,"DEBUG wacs_ml :  res ~p",[Res]),
    
%     Values.

% next_ts(TS, Cycle) ->
%     (TS div Cycle) * Cycle + Cycle.
    
    
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

-module(wacs_ml).

-include("fp_struct.hrl").

-export([on_event/2]).

-export([
    request/3,
    response/2,
    weather/2
]).

-define(HOUR, 3600).

on_event(_vent, State)->
   % DUMMY
   State.

request(Consumer, TS, #{ 
    "archives" := Archives0,
    "regressors":=Regressors0
})->
    fp:log(debug, "DEBUG wacs_ml :  ~p", ["Request data from archive"]),
    
    % Path settings
    ArchivesPath = <<Consumer/binary,"/archivesS/">>,
    FuturePath = <<Consumer/binary,"/futureP/">>,
    
    #{
        <<"config">>:= Config0,
        <<"history_length">>:= History0,
        <<"future_length">>:=Future0,
        <<"granularity">>:=Granularity0
    } = fp_db:read_fields( fp_db:open(<<Consumer/binary,"/model_settings">>),[
        <<"config">>,
        <<"history_length">>,
        <<"future_length">>,
        <<"granularity">>
    ] ),
    
    ModelPath = <<Consumer/binary,"/model">>,
    
    % Get the selected model
    case fp:get_value(ModelPath,"path") of
        none->
            none;
        {ok,<<"MODELS/",Model/binary>>}->
            
            % Check settings
            History = if is_number(History0)->History0; true-> 96 * ?HOUR end,
            Future = if is_number(Future0)->Future0; true-> 48 * ?HOUR end,
            Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
            
            % The time point to which read the history 
            To = next_ts( TS, Granularity ) - Granularity,
            
            fp:log(debug, "DEBUG wacs_ml :  ~p", ["Start read data from archive"]),
            % Read the archives
            Archives = [ [<<ArchivesPath/binary,(unicode:characters_to_binary(A))/binary>>,<<"avg">>] || A <- Archives0 ],
            
            HistoryData0 = fp:archives_get_periods(#{
                <<"step_unit">> => <<"second">>,
                <<"step_size">> => Granularity,
                <<"step_count">> => History div Granularity,
                <<"end">> => To
            }, Archives),
            
            % Handle 'none' values as 'null'
            HistoryData1 = fp_db:to_json(term, HistoryData0),
            fp:log(debug, "DEBUG wacs_ml :  Data from history archive ~p", [HistoryData1]),
            
            % Dempher the values in archives
            % HistoryData = wacs_util:time_lag( HistoryData1, #{gain=>1, time_lag => 5} ),
            % fp:log(debug,"DEBUG wacs_ml :  HistoryData ~p",[HistoryData]),
            
            % Read the regressors if they are defined
            fp:log(debug, "DEBUG wacs_ml :  ~p", [FuturePath]),
            FutureData = 
                if
                    is_list(Regressors0)->
                        Regressors = [ [<<FuturePath/binary,(unicode:characters_to_binary(R))/binary>>,<<"avg">>] || R <- Regressors0 ],
                        fp:log(debug, "DEBUG wacs_ml :  regressors ~p", [Regressors]),
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
                
            fp:log(debug, "DEBUG wacs_ml :  ~p", [HistoryData1]),
            fp:log(debug, "DEBUG wacs_ml :  ~p", ["Create JSON"]),
            
            #{
                "steps"=>Future div Granularity,
                "point"=> Model,
                "data"=> HistoryData1,
                "regressor"=> FutureData,
                "time_freq"=> <<"H">>,
                "config" => Config0
           }
    end.

    
response(Consumer, #{
    "predict" := Predict0,
    "data" := Data,
    "name" := ArchiveNames
})->
    #{
        <<"granularity">>:=Granularity0
    } = fp_db:read_fields( fp_db:open(<<Consumer/binary,"/model_settings">>),[
        <<"granularity">>
    ] ),
    
    fp:log(debug, "DEBUG wacs_ml :  List of archive names ~p", [ArchiveNames]),
    
    QLoadFactor = 0.3,
    
    fp:log(debug,"DEBUG wacs_ml :  predict ~p",[Predict0]),
    
    Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
    
    % Add the timestamp
    [LastTS|_] = lists:last( Data ),
    
    fp:log(debug,"DEBUG wacs_ml :  last ts ~p",[LastTS]),
    
    Predict = 
        [ {LastTS + N*Granularity*1000, V} || {N, V } <- lists:zip( lists:seq(1,length(Predict0)), Predict0) ],
        
    ArchivePload = 
        <<Consumer/binary, "/futureP/", (unicode:characters_to_binary(hd(ArchiveNames)))/binary>>,
    
    fp:log(debug, "DEBUG wacs_ml :  ~ts future insert ~p",[ArchivePload, Predict]),
    fp_archive:insert_values(ArchivePload, Predict ),
    
    % Save predict to the archive
    case ArchiveNames of
        [_,Q_archive|_]->
            ArchiveQload = 
                <<Consumer/binary, "/futureP/", (unicode:characters_to_binary(Q_archive))/binary>>,
            fp_archive:insert_values(ArchiveQload, [ {T, V * QLoadFactor} || {T,V} <- Predict ] );
        _->
            ignore
    end,
    
  ok.
  
  
weather(Archive, #{
    "granularity" := Granularity0,
    "temp":=Value,
    "ts":=TS0
})->

    Granularity = if is_number(Granularity0)->Granularity0 * 1000; true-> 1 * ?HOUR * 1000 end,
    
    TS = [ begin fp:log(debug,"DEBUG wacs_ml: weather ts ~p",[T]),next_ts( fp_lib:parse_dt(T * 1000), Granularity ) - Granularity end  || T <- TS0 ],
    
    Values = lists:zip( TS, Value ),
    fp:log(debug,"DEBUG wacs_ml: values ~p",[Values]),
    
    Res = fp_archive:insert_values(Archive ,Values),
    fp:log(debug,"DEBUG wacs_ml: res ~p",[Res]),
    
    Values.

next_ts(TS, Cycle) ->
    (TS div Cycle) * Cycle + Cycle.    
    
