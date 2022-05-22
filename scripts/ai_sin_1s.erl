%%-----------------------------------------------------------------
%% Copyright (c) 2017, Invent Technology. All Rights Reserved.
%% Author: Vozzhenikov Roman, vzroman@gmail.com
%%-----------------------------------------------------------------
-module(ai_sin_1s).

%% ====================================================================
%% API functions
%% ====================================================================
-export([on_event/1]).


on_event({on_cycle,_Cycle})->
  Time=calendar:datetime_to_gregorian_seconds(calendar:local_time())/1000,
  {_,Objects}=ecomet:find([<<".pattern">>],{<<".pattern">>,{link,<<"/root/.patterns/AI">>}}),
  lists:foreach(fun({OID,[{_,_PatternID}]})->
    {ok,Object}=ecomet:open_nolock(OID),
    H=get_value(Object,<<"valueH">>,100.0),
    L=get_value(Object,<<"valueL">>,0.0),
    Value=new_value(Time,Object,H,L),
    ecomet:edit_object(Object,[{<<"value">>,Value}])
  end,Objects).

get_value(Object,Field,Default)->
  case ecomet:read_field(Object,Field) of
    {ok,none}->Default;
    {ok,Value}->Value
  end.

new_value(Time,Object,H,L)->
    Amp1=(erlang:phash2(Object,1000)/1000)*(H-L),
    Amp2=(erlang:phash2(Object,100)/(100*50))*(H-L),
    T1=(Time/3600)*erlang:phash2(Object,3600)*math:pi(),
    T2=(Time/60)*erlang:phash2(Object,60*60)*math:pi(),
    Sin1=math:sin(T1)+1,
    Sin2=math:sin(T2)+1,
    Sin1V=((L+(H-L)*(Sin1*0.5*Amp1))/(H-L)),
    Sin2V=((L+(H-L)*(Sin2*0.5*Amp2))/(H-L)),
    Sin1V+Sin2V.
