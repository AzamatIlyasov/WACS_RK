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

-module(wacs_util).

-include("fp_struct.hrl").

-export([
    on_event/2
]).

-export([
    import_archives/0,
    time_lag/3,time_lag/2,
    tlag/2,
    maps_get_params/4
]).

-define(HOST,"10.9.60.31").
-define(PORT,9000).
-define(LOGIN,<<"system">>).
-define(PASS,<<"111111">>).

-define(HOUR, 3600000).
-define(DAY, ?HOUR*24 ).

-define(LOGKEY(OID,TS),{ log, OID, TS }).
-define(DUMPKEY(OID,TS),{ day, OID, TS }).


-define(DEPTH, 2 * 60 * 24).   % 2 days in minutes
-define(SHIFT, 2 * ?DAY).       % 5 days in milliseconds
-define(ARCHIVES,[

    %<<"/Agadyr_F/_U220/archivesP/demand_p_load">>,
    <<"/Agadyr_F/_U220/archivesP/demand_p_load">>,
    <<"/Agadyr_F/_U220/futureP/predict_p_load">>,
    <<"/demand_p_load">>,
    <<"/Kentau_F/futureP/predict_p_load">>,
    <<"/demand_p_load">>,
    
    <<"/Kyzylorda_220_F/futureP/predict_p_load">>,
    <<"/demand_p_load">>,
    <<"/Mirgalimsai_F/futureP/predict_p_load">>,
    <<"/demand_p_load">>,
    <<"/Nura_F/u220/futureP/predict_p_load">>,
    <<"/demand_p_load">>,
    <<"/Oskarovka-220_F/futureP/predict_p_load">>,
    <<"/demand_p_load">>,
    <<"/Shu_F/u220/futureP/predict_p_load">>,
    
    <<"/u220/archivesP/demand_p_load">>, 
    <<"/u220/archivesS/p_load">>, 
    <<"">>,
    <<"/archivesP/demand_p_load">>,
    <<"/futureP/predict_p_load">>,
    <<"/u220/archivesP/demand_p_load">>,
    <<"/u220/futureP/predict_p_load">>,
    <<"/u220/archivesP/demand_p_load">>,
    <<"/u220/futureP/predict_p_load">>,
    <<"/VL5300_F/archivesP/p_start">>,
    <<"/VL5300_F/futureP/predict_p_load">>,
    <<"/VL5320_F/archivesP/p_start">>,
    <<"/VL5320_F/futureP/predict_p_load">>,
    <<"/VL5394_F/archivesP/p_start">>,
    <<"/VL5394_F/futureP/predict_p_load">>,
    <<"/VL5170_F/archivesS/p_start">>,
    <<"/VL5170_F/archivesS/p_end">>,
    <<"/VL5120_F/archivesS/p_start">>,
    
    <<"/VL5120_F/archivesS/p_end">>,
    <<"/VL5143_F/archivesS/p_start">>,
    <<"/VL5143_F/archivesS/p_end">>,
    <<"/VL2268_F/archivesS/p_start">>,
    <<"/VL2268_F/archivesS/p_end">>,
    <<"/archivesS/p_start">>,
    <<"/archivesS/p_end">>,
    <<"/VL2258_F/archivesS/p_start">>,
    <<"/VL2258_F/archivesS/p_end">>,
    <<"/archivesS/p_start">>,
    <<"/archivesS/p_end">>,
    <<"/archivesS/p_start">>,
    <<"/archivesS/p_end">>,
    <<"/ss1/archivesP/p_sech">>,
    <<"">>,
    <<"/ss2/archivesP/p_sech">>,
    <<"/ss2/futureP/predict_p_load">>
    %%, <<"/root/PROJECT/______">>,
    
]).

on_event(_Event, State)->
   State.

import_archives()->

    fp_lib:login(),
    
    {ok, Connection } = ws_connect( ?HOST, ?PORT, ?LOGIN, ?PASS ),
    
    ?LOGINFO("connected..."),
    
    Result = 
        try
            [ { A, import_archive( A, Connection )} || A <- ?ARCHIVES  ]
        catch
            _:Error->
                {error, Error}
        end,
    
    
    ws_close(Connection),
    
    Result.
    
import_archive( Archive, Connection )->
    
    ?LOGINFO("importing ~ts",[Archive]),
    TS = fp_lib:get_datetime(),
    
    case ws_transaction( Connection, <<"application">>, #{
        <<"module">> => <<"fp_json">>,
        <<"function">> => <<"read_archives">>,
        <<"function_params">> => #{
            <<"archives">> => [[ Archive, <<"avg">> ]],
            <<"periods">> => #{
                <<"end">> => TS - ?SHIFT,
                <<"step_size">> => 1,
                <<"step_count">> => ?DEPTH,
                <<"step_unit">> => <<"minute">>
            }
        }
        
    }) of
       {ok, Values0}->
            [Tstart0,_] = hd( Values0 ),
            [Tend0,_] = lists:last(Values0),
            ?LOGINFO("~ts received ~p values, start ~p, end ~p",[ 
                Archive ,
                length([ ok || [_,V] <- Values0, V=/=null ] ),
                fp_lib:dt_to_string(Tstart0),
                fp_lib:dt_to_string(Tend0)
            ]),
            
            Values = [ { T + ?SHIFT, if V =:= null-> none; true-> V end } || [T,V] <- Values0  ],
            
            {Tstart,_} = hd( Values ),
            {Tend,_} = lists:last(Values),
            
            ?LOGINFO("~ts insert from ~p, to ~p",[
                Archive, 
                fp_lib:dt_to_string(Tstart),
                fp_lib:dt_to_string(Tend)
            ]),
            InsertResult = fp_archive:insert_values( Archive, Values ),
            ?LOGINFO("~ts insert result ~p",[Archive , InsertResult ]),
            
            LogID = fp_archive:init_log(fp_db:open(Archive,none)),
            Storage = fp_archive:get_storage( LogID ),
            DumpTS = fp_archive:next_ts( TS, ?DAY ) - ?DAY,
            
            BatchSize = 100,
            Stop = ?DUMPKEY(LogID,DumpTS+1),
            % Remove dumps
            ?LOGINFO("~ts removing dump log ~p from ~p to ~p",[Archive, LogID,fp_lib:dt_to_string(DumpTS - ?DEPTH * 60000), fp_lib:dt_to_string(DumpTS) ]),
            dump_purge( dlss:dirty_range_select( Storage, ?DUMPKEY(LogID,DumpTS - ?DEPTH * 60000), Stop, BatchSize ), Storage, LogID, Stop, BatchSize ),
  
            
            ?LOGINFO("~ts dumping to ~p",[Archive, fp_lib:dt_to_string(DumpTS) ]),
            Result = fp_archive:dump_archive( LogID, DumpTS ),
            ?LOGINFO("~ts dump result ~p",[Archive , Result ]),
            
            Result;
        Error -> 
            Error
    end.    
    
dump_purge( Records, Storage, LogID, Stop, Batch )->
  [ dlss:dirty_delete( Storage, K ) || {K,_} <- Records ],
  if
    length(Records) >= Batch ->
      {Last,_} = lists:last(Records),
      dump_purge( dlss:dirty_range_select( Storage, Last, Stop, Batch ), Storage, LogID, Stop, Batch );
    true ->
      ok
  end.
  
ws_connect( Host, Port, Login, Pass )->
  ?LOGINFO("connecting to the host ~p, port ~p",[Host,Port]),
  case ws( Host, Port ) of
    { ok ,Connection }->
      ?LOGINFO("login ..."),
      case ws_login( Connection, Login, Pass ) of
        ok-> {ok, Connection};
        error->
          ws_close(Connection),
          ?LOGERROR( "invalid credentials" ),
          {error,invalid_credentials}
      end;
    { error, Error}->
      ?LOGERROR( "unable to establish a connection ~p",[Error] ),
      {error,Error}
  end.

ws( Host, Port )->
  {ok, PID} = gun:open( Host, Port ),
  {ok, _Protocol} = gun:await_up(PID, 5000 ),
  Stream = gun:ws_upgrade( PID, "/websocket" ),
  receive
    { gun_upgrade, PID, Stream, _Protocols, _Headers} ->

      {ok, {PID, Stream} };
    { gun_response, PID, _, _, _Status, _Headers} ->
      {error, invalid_upgrade_response };
    { gun_error, PID, Stream, Reason } ->
      {error, {websocket_upgrade_error, Reason} }
  after 5000 ->
    { error, timeout }
  end.

ws_close( {PID,_} )->
  gun:close( PID ),
  ok.

ws_login( Connection, Login, Pass )->
  case ws_transaction( Connection, <<"login">>, #{
    <<"login">> => Login,
    <<"pass">> => Pass
  }) of
    {ok, _ }->
      ok;
    _->
      error
  end.

ws_transaction( {PID, Stream}, Action, Params )->
    
  ID = fp_lib:get_datetime(),

  Request = fp_json:to_json(#{
    <<"id">>=>ID,
    <<"action">>=>Action,
    <<"params">>=>Params
  }),

  gun:ws_send(PID, Stream, { binary, Request }),

  receive
    {gun_ws, PID, Stream, Message} ->
      case Message of
        { text, Data }->
          ws_response( ID, Data );
        { binary, Data }->
          ws_response( ID, Data );
        _->
          { error, { invalid_message, Message } }
      end
  after
    6000000->
      { error, timeout }
  end.

ws_response( ID, Response )->
  case fp_json:from_json( Response ) of
    #{ <<"id">>:=ID,
      <<"type">>:= <<"ok">>,
      <<"result">> := Result
    }->
      { ok, Result };
    #{ <<"id">>:=ID,
      <<"type">>:= <<"error">>,
      <<"result">> := Error
    }->
      { error, Error };
    Unexpected->
      { error, {unexpected,Unexpected} }
  end.

time_lag( Value, #{ gain:= Gain, tlag := TLag }, #{
    ts := TS0,
    in := In0,
    out := Out0
} = State)->
    TS = fp_lib:get_datetime(),
    
    if
        is_number(Value),is_number(In0)->
            Cycle=(TS-TS0)/1000,
            A_K=Gain*Cycle/(2*TLag+Cycle),
            C_K=(-2*TLag+Cycle)/(2*TLag+Cycle),
            Out = A_K*Value + A_K*In0 - C_K*Out0,
            { Out, State#{ ts => TS, in => Value, out => Out } };
        true ->
            { Value, State#{ ts => TS, in => Value, out => Value } }
    end;
    
time_lag( Value, _Params, _Init )->
    State = #{
        ts => fp_lib:get_datetime(),
        in => Value,
        out => Value
    },
    { Value, State }.
    
time_lag( Archives, Params )->
    [TS | Archives1] = fp_lib:transpose( Archives ),
    Archives2 = 
        [ tlag( lists:zip(TS,A), Params) || A <- Archives1 ],
    Archives3 = 
        [[ V || {_,V} <- A ] || A <- Archives2],
    fp_lib:transpose( [TS| Archives3] ).
    
    
tlag( [{TS0,null}|Values], Params )->
    [{TS0,null}|tlag( Values, Params)];
tlag( [{TS0,V0}|Values], Params )->
    [{TS0,V0}|tlag( Values, Params, {TS0,V0,V0} )].
    
tlag( [{TS,null}|Rest], Params, Acc )->
    [{TS,null}|tlag( Rest, Params, Acc )];
tlag( [{TS,In}|Rest], #{ gain := Gain, time_lag:= TLag }=Params, {TS0,In0,Out0} )->
    Cycle=(TS-TS0)/1000,
    A_K=Gain*Cycle/(2*TLag+Cycle),
    C_K=(-2*TLag+Cycle)/(2*TLag+Cycle),
    Value = A_K*In+A_K*In0-C_K*Out0,
    [{TS,Value} | tlag(Rest,Params,{TS,In,Value}) ];
tlag([],_Params,_Acc)->
    [].
    
    
    
%%
%% maps:get - if we want invert value
%% for ex.: maps_get_params("v1",VARS,none,[{v1,-1}])
maps_get_params(Key,VARS,DefVal,Params)->
    Result0 = 
        case maps:get(Key, VARS, DefVal) of 
            none -> 0;
            undef -> 0;
            undefined -> 0;
            V -> V
        end,
        
    Vs = 
     case Key of 
        "v1" -> v1;
        "v2" -> v2;
        "v3" -> v3;
        "v4" -> v4;
        "v5" -> v5;
        "v6" -> v6;
        "v7" -> v7;
        "v8" -> v8;
        "v9" -> v9;
        "v10" -> v10;
        "v11" -> v11;
        "wv1" -> wv1
     end,
    
    Corr0 = 
     [ begin 
            %?LOGINFO("DEBUG: V ~p , BEFResCaseV= ~p ",[V, VARS]),
            Corr=
             case V1 of 
                v1 ->-1;
                        %   case Path of 
                        %     [{53,2142}] -> ?LOGINFO("DEBUG: v1 ----1 V= ~p TempV ~p VARSnew ~p VARS ~p ",[V, TempV, TVARSnew, VARS]);
                        %     _ -> ok
                        %   end;
                v2 ->-1;
                v3 ->-1;
                v4 ->-1;
                v5 ->-1;
                v6 ->-1;
                v7 ->-1;
                v9 ->-1;
                v10->-1;
                v11->-1;
                %wams
                wv1->-1;
                % %if param is NULL/NONE           
                _ -> 1
             end,
             
            %  case Path of 
            %     [{53,1302}]-> 
            %         ?LOGINFO("DEBUG: 0-1 AZA V ~p ",[V]),
            %         ?LOGINFO("DEBUG: 0-2 AZA VARS    ~p ",[VARS]),
            %         ?LOGINFO("DEBUG: 0-3 AZA VARSnew ~p ",[VARSnew]),
            %         ?LOGINFO("DEBUG: 0-3 AZA _Lcl    ~p ",[_Lcl]),
            %         ?LOGINFO("DEBUG: 0-3 AZA ARSnew1 ~p ",[TVARSnew1]);
            %     _ -> ok
            %  end,
            
            % ?LOGINFO("DEBUG: WACS_UTIL V= ~p Vs= ~p Corr= ~p ",[V1, Vs, Corr]),
             
            Corr
      end || {V1,_} <- Params, V1 == Vs ],
      
    K1 = 
        case Corr0 of 
            undef ->     1;
            undefined -> 1;
            none ->      1;
            [none] ->    1;
            [] ->        1;
            [-1] ->     -1;
            [1] ->       1;
            Vcor -> ?LOGINFO("DEBUG: WACS_UTIL SOMESOME  K = ~p  P = ~p  V= ~p ",[Key, Params, Vcor]), 1
        end,
      
    Result = K1 * Result0, 
    %?LOGINFO("DEBUG: Res= ~p ",[Result]),
    Result.
    
    
    