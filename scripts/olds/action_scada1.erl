%% Script must be an Erlang fun of arity 3.
%% It accepts next arguments:
%%   * Input - map of parsed variables from the request
%%       key - variable name (list)
%%   * Output - map or undefined. If the binding has a SEND-RECEIVE
%%       then the Input is a result of the request and Output is a map
%%       containing variables for the request message, key - name (list).
%%       otherwise the Output is undefined,
%%   * Context - map of defined for the script variables.
%% As a result the function may return a map of the variables which are
%% defined in the Context with new values for them.

fun(Input,Output,Context)->
   % прописываем адреса
   AdrP     =  "T36_I1",     %адрес-36тип
   AdrQ     =  "T36_I2",     %адрес-36тип
   AdrState =  "T31_I1002",  %адрес-31тип
    
   {P, Pqds} = iec104_script:check_value(AdrP,Input),
   {Q, Qqds} = iec104_script:check_value(AdrQ,Input),
   {StateB, StateBqds} = iec104_script:check_value(AdrState,Input),

   %fp:log(info, "DEBUG: test_iec104_qds_selVal_qds: p:~p, q:~p, state:~p",[P,Q,StateB]),
  
   #{
       %% p
       "p"     =>           P,
       "p_status" =>        Pqds,

       %% q
       "q"     =>           Q,
       "q_status" =>        Qqds,
       
       %% state_b
       "state_b"     =>     StateB,
       "state_b_status" =>  StateBqds
       
   }
end.
