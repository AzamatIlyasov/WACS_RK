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

-module(wacs_trigger).

-include("fp_struct.hrl").

-define(START_TIME, {12,0,0}). % UTC time
-define(TRIGGER_STEP, 1*60000).   % 1 min

-record(state,{ tags, time }).

-export([on_event/2]).

on_event(_Event, State0)->

   {_, Time}= calendar:local_time(),
   TS = fp_lib:get_datetime(),
   
   State = case State0 of #state{}-> State0; _-> #state{} end,
   Tags =
       case State of
            #state{ tags = [{Tag, TriggerTS}| Rest] }->
                if
                    TS >= TriggerTS->
                        trigger_tag( Tag ),
                        Rest;
                    true->
                        [{Tag, TriggerTS}| Rest]
                end;
            #state{ time = T } when T < ?START_TIME, Time > ?START_TIME->
                fp:log(debug,"DEBUG: wacs_trigger_MODELLING: start triggering model control tags"),
                schedule_triggers( find_tags(), TS );
            _ ->
                []
       end,
   State#state{ tags = Tags, time = Time }.

find_tags()->
    fp_db:query(<<"get .oid from root where andnot( .pattern=$oid('/root/.patterns/model_control'), disabled=true )">>).
    
schedule_triggers([T|Rest], TS)->
    [{T,TS}| schedule_triggers(Rest,TS+?TRIGGER_STEP)];
schedule_triggers([],_TS)->
    [].

trigger_tag( TagID )->
    Tag = fp_db:open(TagID,none),
    Value = 
        case fp_db:read_field( Tag, <<"trigger">> ) of
            {ok, _Value} when is_integer(_Value)-> _Value + 1;
            _-> 1
        end,
    fp:log(debug,"DEBUG: wacs_trigger_MODELLING: trigger tag ~ts",[ ?PATH(Tag) ]),
    fp_db:edit_object( Tag, #{ <<"trigger">> => Value } ).
    
    
    
    

