%% скрипт для достоверизации данных по 104 протоколу
%%
%%

module(iec104script).

export(
    check_value/3
    ).

check_value(Val_in, Qds_in, TS_in) -> 

    %for qds
    Correct = 0,
    R1 = 1,
    R2 = 2,
    BL = 4,
    SB = 8,
    NT = 16,
    IV = 32,
    Timeout = 10, %second

    %fp:log(info, "DEBUG: test_iec104script: val:~p, qds:~p, ts:~p",[Val_in,Qds_in,TS_in]),
    TSc = erl:time(),
    TSd = TSc - TS_in,
    Qds = 
        if 
            Val_in =:= none -> IV; %выставляем флаг IV
            %TSd > Timeout -> NT; %выставляем флаг NT
            true -> Qds_in
        end,    
    Val = 
        if 
            Qds =:= none -> 0; 
            true -> Val_in
        end, 
    

{Value, Qds}.
