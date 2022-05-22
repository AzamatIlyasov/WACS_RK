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

-module(iec104_script).

-include("fp_struct.hrl").

-export([on_event/2]).

-export([
    check_value/2,
    check_value/3,
    check_value/4,
    check_value_ts/2,
    check_value_ts/3
]).


%for qds
-define(CORRECT, 0).
-define(R1, 1).
-define(R2, 2).
-define(BL, 4).
-define(SB, 8).
-define(NT, 16).
-define(IV, 32).
-define(TIMEOUT, 10). %second - for QDS


on_event(_vent, State)->
   % DUMMY
   State.

% %% для достоверизации данных  - для Реакторов!
% %% (адрес, сигналы)
check_value(Addr, Input) -> 
    %fp:log(info,"in:~p",[Input]),
    Val_in    = maps:get( unicode:characters_to_list([Addr,"_value"]) ,Input, 0),
    Qds_in    = maps:get( unicode:characters_to_list([Addr,"_qds"]),   Input, ?IV),
    TS_in     = maps:get( unicode:characters_to_list([Addr,"_ts"]),    Input, 1),
    %TSc = fp_lib:get_datetime(),
    %TSd = TSc - TS_in,
    %fp:log(info, "DEBUG: test_iec104_script: val:~p, qds:~p, ts:~p , tsfp:~p , tsD:~p",[Val_in,Qds_in,TS_in, TSc, TSd]),
    %fp:log(info, "DEBUG: test_iec104_script: val_current:~p",[Val_current]),
    Qds = 
        if 
            Val_in =:= none -> ?IV; %выставляем флаг IV
            %TSd > ?TIMEOUT -> ?NT; %выставляем флаг NT
            true -> Qds_in
        end, 
    %fp:log(info, "DEBUG: test_iec104_script: qdsSel:~p",[Qds]),
    Value2 = 
        if 
            Qds =:= none -> 0.0; 
            true -> Val_in
        end, 
    %fp:log(info, "DEBUG: test_iec104_script: valSel:~p",[Value]),
    
    %для подмены данных адресами - разкоментировать. Закоментировать верхнюю
    % [_,Adr1]=string:split(Addr,"I"), %делим строку, получаем адресс
    % {AdrInt,[]}=string:to_integer(Adr1),
    % Value = AdrInt,
      Value = Value2,
    {Value, Qds}.


%% для достоверизации данных 
%% (значение в адресе, qds, ts, текущее значение в тэге)
%check_value(Addr, Input, Context) -> 


%% для достоверизации данных 
%% (адрес, сигналы, Invert)
check_value(Addr, Input, Invert) -> 
    %fp:log(info,"in:~p",[Input]),
    Val_in    = maps:get( unicode:characters_to_list([Addr,"_value"]) ,Input, 0),
    Qds_in    = maps:get( unicode:characters_to_list([Addr,"_qds"]),   Input, ?IV),
    TS_in     = maps:get( unicode:characters_to_list([Addr,"_ts"]),    Input, 1),
    %TSc = fp_lib:get_datetime(),
    %TSd = TSc - TS_in,
    %fp:log(info, "DEBUG: test_iec104_script: val:~p, qds:~p, ts:~p , tsfp:~p , tsD:~p",[Val_in,Qds_in,TS_in, TSc, TSd]),
    %fp:log(info, "DEBUG: test_iec104_script: val_current:~p",[Val_current]),
    
    Qds = 
        if 
            Val_in =:= none -> ?IV; %выставляем флаг IV
            %TSd > ?TIMEOUT -> ?NT; %выставляем флаг NT
            true -> Qds_in
        end, 
    %fp:log(info, "DEBUG: test_iec104_script: qdsSel:~p",[Qds]),
    %fp:log(info,"CHECK_VALUE in Val_in : ~p",[Val_in]),
    Val_in_ch = 
        if 
            Val_in =:= none -> 0.0; 
            true -> Val_in
        end, 
    % Value1 = 
    %     if 
    %         Qds =:= none -> ?IV; 
    %         true -> Val_in_ch
    %     end, 
    %fp:log(info,"~p",[Invert]),
    Value2 = 
        if
            Invert =:= true -> -Val_in_ch;
            true -> Val_in_ch
        end,
    %fp:log(info, "DEBUG: test_iec104_script: valSel:~p ~p",[Value2, Qds]),
    
    %%для подмены данных адресами - разкоментировать. Закоментировать верхнюю
    % [_,Adr1]=string:split(Addr,"I"), %делим строку, получаем адресс
    % {AdrInt,[]}=string:to_integer(Adr1),
    % Value = AdrInt,
    Value = Value2,
    %fp:log(info,"CHECK_VALUE in VALUE RES : ~p",[Value]),
    {Value, Qds}.
    
    
    
%% для достоверизации данных - с возможность сохранения тек знач.
%% (адрес, сигналы, Invert, Context)
check_value(Addr, Input, Invert, Context) -> 
    %fp:log(info,"in:~p",[Input]),
    Val_in    = maps:get( unicode:characters_to_list([Addr,"_value"]) ,Input, 0),
    Qds_in    = maps:get( unicode:characters_to_list([Addr,"_qds"]),   Input, ?IV),
    TS_in     = maps:get( unicode:characters_to_list([Addr,"_ts"]),    Input, 1),
    Val_current= maps:get("value",Context),
    
    %TSc = fp_lib:get_datetime(),
    %TSd = TSc - TS_in,
    %fp:log(info, "DEBUG: test_iec104_script: val:~p, qds:~p, ts:~p , tsfp:~p , tsD:~p",[Val_in,Qds_in,TS_in, TSc, TSd]),
    % fp:log(info, "DEBUG: test_iec104_script: val_c: ~p ",[Val_current]),
    % fp:log(info, "DEBUG: test_iec104_script: contx: ~p ",[Context]),
    % fp:log(info, "DEBUG: test_iec104_script: input: ~p ",[Input]),
    Qds = 
        if 
            Val_in =:= none -> ?IV; %выставляем флаг IV
            %TSd > ?TIMEOUT -> ?NT; %выставляем флаг NT
            true -> Qds_in
        end, 
    %fp:log(info, "DEBUG: test_iec104_script: qdsSel:~p",[Qds]),
    %fp:log(info,"CHECK_VALUE in Val_in : ~p",[Val_in]),
    Val_in_ch = 
        if 
            Val_in =:= none -> 0.0; 
            Qds /= 0 -> Val_current; %%сохраняем предыдущее значение, если qds !=0
            true -> Val_in
        end, 
    % Value1 = 
    %     if 
    %         Qds =:= none -> ?IV; 
    %         true -> Val_in_ch
    %     end, 
    %fp:log(info,"~p",[Invert]),
    Value2 = 
        if
            Invert =:= true -> -Val_in_ch;
            true -> Val_in_ch
        end,
    %fp:log(info, "DEBUG: test_iec104_script: valSel:~p ~p",[Value2, Qds]),
    
    %%для подмены данных адресами - разкоментировать. Закоментировать верхнюю
    % [_,Adr1]=string:split(Addr,"I"), %делим строку, получаем адресс
    % {AdrInt,[]}=string:to_integer(Adr1),
    % Value =AdrInt,
    Value = Value2,
    %fp:log(info,"CHECK_VALUE in VALUE RES : ~p",[Value]),
    {Value, Qds}.
    

    %% для достоверизации данных - для ТС
%% (адрес, сигналы, Invert)
check_value_ts(Addr, Input) -> 
    %fp:log(info,"in:~p",[Input]),
    Val_in    = maps:get( unicode:characters_to_list([Addr,"_value"]) ,Input, 0),
    Qds_in    = maps:get( unicode:characters_to_list([Addr,"_qds"]),   Input, ?IV),
    TS_in     = maps:get( unicode:characters_to_list([Addr,"_ts"]),    Input, 1),
    %TSc = fp_lib:get_datetime(),
    %TSd = TSc - TS_in,
    %fp:log(info, "DEBUG: test_iec104_script: val:~p, qds:~p, ts:~p , tsfp:~p , tsD:~p",[Val_in,Qds_in,TS_in, TSc, TSd]),
    %fp:log(info, "DEBUG: test_iec104_script: val_current:~p",[Val_current]),
    Qds = 
        if 
            Val_in =:= none -> ?IV; %выставляем флаг IV
            %TSd > ?TIMEOUT -> ?NT; %выставляем флаг NT
            true -> Qds_in
        end, 
    %fp:log(info, "DEBUG: test_iec104_script: qdsSel:~p",[Qds]),
    Value = 
        if 
            Val_in =:= none -> 0; 
            true -> Val_in
        end, 
    %fp:log(info,"~p",[Invert]),
    % Value = 
    %     if
    %         Invert =:= true -> -Value1;
    %         true -> Value1
    %     end,
    %fp:log(info, "DEBUG: test_iec104_script: valSel:~p ~p",[Value, Qds]),
    
    %для подмены данных адресами - разкоментировать. Закоментировать верхнюю
    % [_,Adr1]=string:split(Addr,"I"), %делим строку, получаем адресс
    % {AdrInt,[]}=string:to_integer(Adr1),
    % Value = AdrInt,
    
    {Value, Qds}.
   
    
    %% для достоверизации данных - для ТС
%% (адрес, сигналы, Invert)
check_value_ts(Addr, Input, Context) -> 
    %fp:log(info,"in:~p",[Input]),
    Val_in    = maps:get( unicode:characters_to_list([Addr,"_value"]) ,Input, 0),
    Qds_in    = maps:get( unicode:characters_to_list([Addr,"_qds"]),   Input, ?IV),
    TS_in     = maps:get( unicode:characters_to_list([Addr,"_ts"]),    Input, 1),
    %TSc = fp_lib:get_datetime(),
    %TSd = TSc - TS_in,
    %fp:log(info, "DEBUG: test_iec104_script: val:~p, qds:~p, ts:~p , tsfp:~p , tsD:~p",[Val_in,Qds_in,TS_in, TSc, TSd]),
    %fp:log(info, "DEBUG: test_iec104_script: val_current:~p",[Val_current]),
    Qds = 
        if 
            Val_in =:= none -> ?IV; %выставляем флаг IV
            %TSd > ?TIMEOUT -> ?NT; %выставляем флаг NT
            true -> Qds_in
        end, 
    %fp:log(info, "DEBUG: test_iec104_script: qdsSel:~p",[Qds]),
    Value = 
        if 
            Val_in =:= none -> 0; 
            true -> Val_in
        end, 
    %fp:log(info,"~p",[Invert]),
    % Value = 
    %     if
    %         Invert =:= true -> -Value1;
    %         true -> Value1
    %     end,
    %fp:log(info, "DEBUG: test_iec104_script: valSel:~p ~p",[Value, Qds]),
    
    %для подмены данных адресами - разкоментировать. Закоментировать верхнюю
    % [_,Adr1]=string:split(Addr,"I"), %делим строку, получаем адресс
    % {AdrInt,[]}=string:to_integer(Adr1),
    % Value = AdrInt,
    
    {Value, Qds}.
