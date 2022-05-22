%% Script must be an Erlang fun of arity 3.
%% It accepts next arguments:
%% * Input - map of parsed variables from the request
%% key - variable name (list)
%% * Output - map or undefined. If the binding has a SEND-RECEIVE
%% then the Input is a result of the request and Output is a map
%% containing variables for the request message, key - name (list).
%% otherwise the Output is undefined,
%% * Context - map of defined for the script variables.
%% As a result the function may return a map of the variables which are
%% defined in the Context with new values for them.

fun(Input,Output,Context)->
   %u_n1
    AdrU1 = "T36_I580", %адрес-36тип
    AdrU2 = "T36_I700", %адрес-36тип
    
    {U_n1, U_n1_qds} = iec104_script:check_value(AdrU1,Input),
    {U_n2, U_n2_qds} = iec104_script:check_value(AdrU2,Input),
    {U_n, U_n_qds} = wacs_oti:selU(U_n1,U_n1_qds,U_n2,U_n2_qds),
    %fp:log(info, "DEBUG: test_selValU: U:~p, q:~p",[U_n,U_n_qds]),
   #{
       "u_n" => U_n,
       "u_n_status" => U_n_qds
   }
end.
