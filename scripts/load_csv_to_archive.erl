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

-module(load_csv_to_archive).

-include("fp_struct.hrl").

-define(TS1970,62167219200000).
-define(FILEPATH,"/tmp/out_data_shymkent_predict_3.csv").
-define(ARCHIVE_NAME,<<"/u220/futureP/predict_p_load">>).
-export([on_event/2]).

on_event({on_cycle,_Cycle},State)->
  {ok,Data} = file:read_file(?FILEPATH),
  Lines=binary:split(Data,<<"\n">>,[global]),
  ReadyData=parse_lines(lists:nthtail(1,Lines),[]),
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
  [DT|[Value|_]]=binary:split(Line,<<",">>,[global]),
  TS=parse_ts(DT),
  Value1=parse_value(Value),
  {TS,Value1}.

parse_ts(DT)->
  [DMY,HMS|_]=binary:split(DT,<<" ">>,[global]),
  [Year|[Month|[Day|_]]]=binary:split(DMY,<<"-">>,[global]),
  [Hour|[Min|_]]=binary:split(HMS,<<":">>,[global]),
  Sec = 0,
  DT0={
    {binary_to_integer(Year),binary_to_integer(Month),binary_to_integer(Day)},
    {binary_to_integer(Hour),binary_to_integer(Min),Sec}
  },
  DT1=calendar:local_time_to_universal_time(DT0),
  calendar:datetime_to_gregorian_seconds(DT1)*1000-?TS1970.

parse_value(V)->
  [Value|_] = binary:split(V,<<"\r">>,[global]),
  try binary_to_integer(Value)
  catch
    _:_->
      try binary_to_float(Value)
      catch
        _:_->none
      end
  end.