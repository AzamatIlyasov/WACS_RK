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

-module(wacs_archive_smooth_t).


-include("fp_struct.hrl").

-export([
    on_event/2
]).

-define(HOUR, 3600000).
-define(DAY, ?HOUR*24 ).


-define(DEPTH, 2 * 60 * 24).   % 2 days in minutes
-define(SHIFT, 2 * ?DAY).       % 5 days in milliseconds

on_event({on_cycle,_Cycle},State)-> 
    ?LOGDEBUG("DEBUG: G_SCRIPT->wacs_archive_smooth_t BEGIN"),
    AllTags = fp_db:query(<<"get .oid from root where andnot( .pattern=$oid('/root/.patterns/model_control'), disabled=true )">>),
    [begin
        ControlTagObject = fp_db:open(TagOID, none),
        {ok, FolderOID} = fp_db:read_field(ControlTagObject, <<".folder">>),
        Path = fp_db:to_path(FolderOID),
        try 
            [ModelSetOID|_] = fp_db:query(<<"get .oid from root where AND( .pattern=$oid('/root/.patterns/model_settings'), .folder=$oid('",Path/binary,"') )">>), 
            ModelSettingObject = fp_db:open(ModelSetOID, none),
            {ok, TimeDelta} = fp_db:read_field(ModelSettingObject, <<"history_length">>),
            AllArchives = fp_db:query(<<"get .oid from root where AND(.pattern=$oid('/root/.patterns/ARCHIVE'), .folder=$oid('",Path/binary,"/archivesS') )">>),
            [begin
                ArchiveObject = fp_db:open(OIDArchive, none),
                {ok, NameArchive} = fp_db:read_field(ArchiveObject, <<".name">>),
                smooth_archive(<<Path/binary,"/archivesS/",NameArchive/binary>>, <<Path/binary,"/archivesP/",NameArchive/binary>>,TimeDelta)
            end
            || OIDArchive <- AllArchives]
        catch 
            _:Error->
            ?LOGDEBUG("DEBUG wacs_archive_smooth_t ERROR: Path: ~p  Error: ~p",[
                Path, 
                Error
            ])
      end
    end
    || TagOID <- AllTags],
    ?LOGDEBUG("DEBUG: G_SCRIPT->wacs_archive_smooth_t END"),
    State;
on_event(_,_State)->
   undefined.

smooth_archive(FromArch, ToArch, TimeDelta)->
    Archives =  [[ FromArch, <<"avg">> ]],
    TS = fp_lib:get_datetime(),
    Periods = #{
                <<"start">> => TS - TimeDelta,
                <<"step_size">> => 1,                    %%%%% CHEECK
                <<"step_count">> => 100,%%?DEPTH,     %%%%% CHEECK
                <<"step_unit">> => <<"second">>          %%%%% CHEECK
            },
   Values0 = fp_archive:get_periods(Periods,Archives),
   Gain = 1.0,                                               %%%%% CHEECK
   TLag = 1.0,                                                   %%%%% CHEECK
   Params = #{
       gain => Gain, 
       time_lag => TLag 
   },
   Values = time_lag(Values0, Params),      % Here is a method smoothing data
%   ?LOGDEBUG("ToArch ~p   ",[ToArch]),
  % ?LOGDEBUG("Values ~p   ",[Values]),
  Values1 = [{T,V} ||[T,V]<-Values], %% CHEECK
  InsertResult = fp_archive:insert_values( ToArch, Values1 ), %% CHEECK
  [Tstart,_] = hd( Values ),
  [Tend,_] = lists:last(Values),
  ?LOGDEBUG("~ts insert from ~p, to ~p, insert result: ~p",[
                ToArch, 
                fp_lib:dt_to_string(Tstart),
                fp_lib:dt_to_string(Tend),
                InsertResult
            ]).

time_lag( Archives, Params )->
    [TS | Archives1] = fp_lib:transpose( Archives ),
    Archives2 = 
        [ tlag( lists:zip(TS,A), Params) || A <- Archives1 ],
    Archives3 = 
        [[ V || {_,V} <- A ] || A <- Archives2],
    fp_lib:transpose( [TS| Archives3] ).
    
tlag( [{TS0,none}|Values], Params )->
    [{TS0,none}|tlag( Values, Params)];
tlag( [{TS0,V0}|Values], Params )->
    [{TS0,V0}|tlag( Values, Params, {TS0,V0,V0} )].
    
tlag( [{TS,none}|Rest], Params, Acc )->
    [{TS,none}|tlag( Rest, Params, Acc )];
tlag( [{TS,In}|Rest], #{ gain := Gain, time_lag:= TLag }=Params, {TS0,In0,Out0} )->
    Cycle=(TS-TS0)/1000,
    A_K=Gain*Cycle/(2*TLag+Cycle),
    C_K=(-2*TLag+Cycle)/(2*TLag+Cycle),
    Value = A_K*In+A_K*In0-C_K*Out0,
    [{TS,Value} | tlag(Rest,Params,{TS,In,Value}) ];
tlag([],_Params,_Acc)->
    [].
    
    
    
    
    
    
    
    