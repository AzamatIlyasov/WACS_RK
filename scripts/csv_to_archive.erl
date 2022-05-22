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

-module(csv_to_archive).

-include("fp_struct.hrl").

-define(ROW_DELIMITER, <<"\r\n">>).
-define(COLUMN_DELIMITER, <<";">>).

-define(TS1970,62167219200000).
-define(FILEPATH,"/tmp/out_data_ss1_predict.csv").

-define(ARCHIVE_NAME,<<"/ss1/futureP/predict_p_load">>).
-export([on_event/2]).

on_event({on_cycle,_Cycle},State)->
  {ok,Data} = file:read_file(?FILEPATH),
  fp:log(info,"DEBUG: Data ~p",[Data]),
  Lines=binary:split(Data,?ROW_DELIMITER,[global]),
  fp:log(info,"DEBUG: lines ~p",[length(Lines)]),
  fp:log(info, "Inisde Lines ~p", [Lines]),
  ReadyData=parse_lines(lists:nthtail(1,Lines),[]),
  fp:log(info,"DEBUG: ReadyData ~p",[ReadyData]),
  fp_archive:insert_values(?ARCHIVE_NAME,ReadyData),
  State;
on_event(_,State)->
  State.


parse_lines([L|Rest],Acc)->
  Acc1=
    try
      L1=parse_line(L),
      [L1|Acc]
    catch
      _:_Error->
        Acc
    end,
  parse_lines(Rest,Acc1);
parse_lines([],Acc)->
  lists:reverse(Acc).

parse_line(Line)->
  [DT,Value | _]=binary:split(Line,?COLUMN_DELIMITER,[global]),
  TS=parse_ts(DT),
  fp:log(info,"DEBUG: value ~p",[Value]),
  Value1=parse_value(Value),
  {TS,Value1}.

parse_ts(DT)->
    fp_lib:parse_dt(DT).

parse_value(Value)->
  
  try binary_to_integer(Value)
  catch
    _:_->
      try binary_to_float(Value)
      catch
        _:_->none
      end
  end.