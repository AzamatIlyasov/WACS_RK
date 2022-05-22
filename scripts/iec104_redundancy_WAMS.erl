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

-module(iec104_redundancy_WAMS).

-include("fp_struct.hrl").

-export([on_event/2]).
-export([get_avaliable_connections/0]).
-export([get_connection/0]).

%%==============================================================================
%% COMMON PARAMETERS
%%==============================================================================
-define(FOLDER,<<"/root/PROJECT/IOT/">>).

-define(INIT_TIMER,1000).
-define(TURNOVER_TIMER,1000).

-define(STATE(Object), fp_db_1:read_field( Object, <<"state">> ) ).

-define(READ_PARAMS(Source), fp_db:read_fields(fp_db:open(Source,none),[<<"IP">>,<<"Port">>]) ).
-define(SWITCH(Object,Params), fp_db_1:edit_object(Object, Params) ).

%%==============================================================================
%% USER SETTINGS
%%==============================================================================
-define(CONNECTION,<<"iec104_WAMS">>).
-define(AVALIABLE, #{
    {<<"">>, <<"str">>} => {"10.9.60.10", 2404},
    {<<"">>, <<"str">>} => {"10.9.60.11", 2404}
}).

get_avaliable_connections() ->
    ?AVALIABLE.

get_connection() ->
    <<?FOLDER/binary,?CONNECTION/binary>>.
   

on_event({on_cycle,Cycle},State)->
   % here should be your code that is executed each Cycle milliseconds
   State;
   
on_event({tag,TagID,Field,Value},State)->
   ?LOGINFO("EVENT ~p",[Value]),
   Connection = fp_db:open(fp_db:to_oid(<<?FOLDER/binary,?CONNECTION/binary>>), none),
   {ok, SettingsJson} = fp_db:read_field(Connection, <<"settings">>),
   if Value =:= <<"disconnected">> ->
        
        {ok, SettingsJson} = fp_db:read_field(Connection, <<"settings">>),
        Settings = fp_json:from_json(SettingsJson),
        CurrentHost = maps:get(<<"host">>, Settings, undefined_host),
        CurrentPort = maps:get(<<"port">>, Settings, undefined_port),
        Avaliable = maps:fold(fun({Tag, Field1}, {IP, Port}, Acc) -> 
        
          ConnectionTag = fp_db:open(fp_db:to_oid(Tag), none),
          {ok, ConnectionTagState} = fp_db:read_field(ConnectionTag, Field1),
          ?LOGINFO("Connection Tag ~p ~p ~p",[CurrentHost, CurrentPort, ConnectionTagState]),
          IPBin = list_to_binary(IP),
          if CurrentHost =:= IPBin, CurrentPort =:= Port ->
                fp_db:edit_object(ConnectionTag, #{Field1 => 0}),
                Acc;
             true -> 
                
                if ConnectionTagState =:= 1 ->
                    Acc ++ [{IPBin, Port}];
                true ->
                    Acc
                end
           end 
        end, [], ?AVALIABLE),
        ?LOGINFO("AVALIABLE ~p",[Avaliable]),
        case Avaliable of 
            [] -> ok;
            [{AvaliableIP, AvaliablePort}|_] ->
               fp_db:edit_object(Connection, #{<<"settings">> => fp_json:to_json(Settings#{<<"host">> => AvaliableIP, <<"port">> => AvaliablePort})})
        end;
        true -> ok
   end,
   State.

