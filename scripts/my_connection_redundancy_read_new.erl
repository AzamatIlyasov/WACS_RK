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

-module(my_connection_redundancy_read).

-include("fp_struct.hrl").

-export([on_event/2]).

%%==============================================================================
%% COMMON PARAMETERS
%%==============================================================================
-define(FOLDER,<<"/root/PROJECT/CONNECTIONS/">>).

-define(INIT_TIMER,100).

-define(STATE(Object), ecomet:read_field( Object, <<"state">> ) ).

-define(SWITCH(Object,Params), ecomet:edit_object(Object,Params) ).

%-define( TS, fp_lib:get_datetime() ).

-record(state,{ step,  item, available, timer }).

%%==============================================================================
%% USER SETTINGS
%%==============================================================================
-define(CONNECTION,<<"MODBUS_RAPTOR_CS_READ">>).

-define(TIMEOUT,500).  % milliseconds

-define(AVAILABLE,[
    #{<<"IP">> => <<"10.9.160.13">>, <<"Port">> => "502" },  %основное - ЦС1.1
    %#{<<"IP">> => <<"10.9.159.13">>, <<"Port">> => "502" },  %основное - ЦС1.2
    #{<<"IP">> => <<"10.9.160.12">>, <<"Port">> => "502" }  %запасное - ЦC2.1
    %#{<<"IP">> => <<"10.9.159.12">>, <<"Port">> => "502" }   %запасное - ЦС2.2
]).


%-----------The first run-------------------------------------------------------
on_event(_, undefined = _State )->

    % Obtain the OID of the connection
    { ok, ItemID }= ecomet:path_to_oid(<<?FOLDER/binary,?CONNECTION/binary>>),
    
    % Open the connection object
    { ok, Item } = ecomet:open_nolock( ItemID ),
    
    % Wait for an init timer, the connection needs time to start
    StepTimer = ?TS + ?INIT_TIMER,
    
    ?FP_LOGINFO("~ts start redundancy script, stepTime = ~p ",[ ?CONNECTION, ?INIT_TIMER ]),
    
    % Enter the init step
    #state{ step = init, timer = StepTimer, item = Item, available = ?AVAILABLE  };
 
%-----------Initialization------------------------------------------------------- 
on_event(_, #state{ step = init, timer = Timer, item = Item } = State )->

    % Check the timer
    TS = ?TS,
    if
        TS >= Timer ->
        
            % Initialization has finished
            % Obtain the current state of the connection
            { ok, ConnectionState } = ?STATE( Item ),
            
            % Next state
            State#state{ step = ConnectionState, timer = TS };
        true->
            % The timer has not finished yet, wait
            State
            
    end;

%-----------Connected-----------------------------------------------------------
on_event(_, #state{ step = <<"true">>, item = Item } = State )->

    % Check if the connection state is not true
    case ?STATE(Item) of
        {ok, <<"true">> }->
            % Everything is fine
            State#state{ timer = undefined };
        {ok, Other }->
            % The connection is broken
            ?FP_LOGWARNING("connection ~ts is in the ~p state",[ ?CONNECTION, Other ]),
            
            % Set the timer
            State#state{ step = Other, timer = ?TS + ?TIMEOUT }
    end;
    
%-----------Connected-----------------------------------------------------------
on_event(_, #state{ step = _Other, item = Item, timer = Timer, available = Available } = State )->

    TS = ?TS,
    
    % Check if the connection has recovered
    case ?STATE(Item) of
        {ok, <<"true">> }->
            
            ?FP_LOGINFO("connection ~ts has recovered , param is ~p ",[ ?CONNECTION, Item ]),
            
            State#state{ step = <<"true">>, timer = undefined };
            
        {ok, Other } when TS >= Timer ->
        
            % The connection is still select the next params
            [ Next | Rest ] = 
                if
                    length(Available) > 0->
                        Available;
                    true->
                        ?AVAILABLE
                end,
            TS_td = calendar:system_time_to_rfc3339(TS,[{unit,millisecond},{offset,"+06:00"}]),
            ?FP_LOGINFO("swith connection ~ts to params ~p, TS_td = ~p ",[ ?CONNECTION, Next, TS_td ]),
            case ?SWITCH( Item, Next ) of
                {ok,_}->ok;
                {error,SwitchError}->
                    ?FP_LOGINFO("switch error ~ts error ~p",[ ?CONNECTION, SwitchError ])
            end,
            
            % Set the timer
            State#state{ step = Other, timer = TS + ?TIMEOUT, available = Rest };
        {ok, Other }->
            % The timer has not finished yet, wait
            State#state{ step = Other }
    end;

%-----------Backgurad. We shuld never get here-----------------------------------------------------------
on_event(_, InvalidState )->
    ?FP_LOGERROR("~ts invalid state for redundancy script ~p",[ ?CONNECTION, InvalidState ]),
    % Continue from the start
    none.
    
