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

-module(iec104_connection_test_WAMS).

-include("fp_struct.hrl").

-export([on_event/2]).

on_event({on_cycle,Cycle},State)->
   case State of 
        undefined -> 
            %?LOGINFO("Connection State undefined"),
            connecting;
        ConnectionState1 ->
           %?LOGINFO("Connection State1 ~p ", [ConnectionState1]),
           % here should be your code that is executed each Cycle milliseconds
           Avaliable = iec104_redundancy_WAMS:get_avaliable_connections(),
           Connection = fp_db:open(iec104_redundancy_WAMS:get_connection(), none),
           {ok, SettingsJson} = fp_db:read_field(Connection, <<"settings">>),
           Settings = fp_json:from_json(SettingsJson),
           {ok, ConnectionState} = fp_db:read_field(Connection, <<"state">>),
           CurrentHost = maps:get(<<"host">>, Settings, undefined_host),
           CurrentPort = maps:get(<<"port">>, Settings, undefined_port),
           NewState = maps:fold(fun({Tag, Field}, {IP, Port}, Acc) -> 
              ConnectionTag = fp_db:open(fp_db:to_oid(Tag), none),
              IPBin = list_to_binary(IP),
              %?LOGINFO("IPBIn ~p ",[IPBin]),
              if CurrentHost =:= IPBin, CurrentPort =:= Port ->
                    %?LOGINFO("CURRENT ~p ~p ",[ConnectionState, Acc]),
                     case ConnectionState of
                     <<"connected">> -> 
                        fp_db:edit_object(ConnectionTag, #{Field => 1}),
                        connected;
                     <<"disconnected">> -> 
                        fp_db:edit_object(ConnectionTag, #{Field => 0}),
                        disconnected;
                    _ ->  
                        fp_db:edit_object(ConnectionTag, #{Field => 0}),
                        Acc
                      end; 
                 true -> 
                    {ok, ConnectionTagState} = fp_db:read_field(ConnectionTag, Field),
                    %?LOGINFO("Connection Tag State ~p ~p",[ConnectionTagState, Acc]),
                    if ConnectionTagState =:= 1,  Acc =:= disconnected -> 
                         fp_db:edit_object(Connection, #{<<"settings">> => fp_json:to_json(Settings#{<<"host">> => IPBin, <<"port">> => Port})}),
                         connecting;
                        true ->
                            case gen_tcp:connect(IP, Port, [binary, {active, false}, {packet, raw}], 1000) of
                            {ok,Socket} -> 
                                gen_tcp:close(Socket),
                                fp_db:edit_object(ConnectionTag, #{Field => 1});
                            {error, Error} ->
                                fp_db:edit_object(ConnectionTag, #{Field => 0})
                            end,
                        Acc
                    end
                end 
               end, ConnectionState1, Avaliable),
           NewState
   end;
   
on_event({tag,TagID,Field,Value},State)->
   % here should be your code that is executed on change of the Field of the Tag
   State.
