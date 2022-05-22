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

-module(upload_to_csv).

-include("fp_struct.hrl").

-export([
    get_report/2,
    on_event/2
]).

-define(DIR,"C:/FACEPLATE/faceplate/lib/fp-2.1.1/priv/UI/reports/report_arpch12.csv").

-define(DELIMITER,"\t").

on_event(_,State)->
   % here should be your code that is executed each Cycle milliseconds
   State.
   
get_report(_ID,#{
    <<"from">>:=From,
    <<"to">>:=To,
    <<"archives">>:=Archives,
    <<"step_unit_OUT">>:=_Step_unit_OUT
    %<<>>:=
})->
    Archives1 = [ [A,avg] || A <- Archives ],
    Interval=#{
        <<"start">>=>From,
        <<"step_unit">>=><<"second">>,
        <<"step_size">>=>1,
        <<"step_count">>=> (To - From) div 1000
    },
    Values = fp_archive:get_periods(Interval,Archives1),
    Header = [ <<"datetime">> | Archives ],
    {ok,File}=open_file(?DIR),
    write_line(Header,File),
    write_data(Values, File, [{0.0, 0.0, 0.0, 0.0} ||  _A <-Archives ] ),
    {ok,ok}.
    

open_file(FileName)->
  case delete_file(FileName) of {error,Error}->{error,Error};
    ok->
      file:open(FileName,write)
  end.
  
  
delete_file(FileName)->
  case file:delete(FileName) of
    ok->ok;
    {error,enoent}->ok;
    {error,Error}->?ERROR(Error)
  end.
  
write_line(Line,File)->
    Line1 = [unicode:characters_to_list(E) || E <- Line],
    Line2 = string:join(Line1,?DELIMITER) ++ "\r\n",
    file:write(File,unicode:characters_to_binary(Line2)).

write_data([[TS|Values]|Rest],File,Acc)->
    TS1 = unicode:characters_to_binary(calendar:system_time_to_rfc3339(TS,[{unit,millisecond},{offset,"+06:00"}])),
    %TS1 = fp_lib:dt_to_string(TS),
    Values1 = 
        [ if
            is_float(V)->float_to_binary(V, [{decimals, 6}]);
            is_atom(V)->atom_to_binary(V,utf8);
            is_integer(V)->integer_to_binary(V)
        end || V <- Values ],
    write_line([TS1|Values1],File),
    Acc1 = 
        [ if
            is_number(V)->
                if 
                    Max =:= 0.0, Min =:= 0.0 ->  
                        if 
                            V > 0 -> {V+SumPos,SumNeg,V,V};
                            true -> {SumPos,SumNeg+V,V,V}
                        end;
                    V > Max ->
                        if 
                            V > 0 -> {V+SumPos,SumNeg,V,Min};
                            true -> {SumPos,SumNeg+V,V,Min}
                        end;
                    V < Min -> 
                        if 
                            V > 0 -> {V+SumPos,SumNeg,Max,V};
                            true -> {SumPos,SumNeg+V,Max,V}
                        end;
                    true -> 
                        if 
                            V > 0 -> {V+SumPos,SumNeg,Max,Min};
                            true -> {SumPos,SumNeg+V,Max,Min}
                        end
                end;
            true->{SumPos,SumNeg,Max,Min}
        end|| {V,{SumPos,SumNeg,Max,Min}} <- lists:zip(Values,Acc) ],
    write_data(Rest,File,Acc1);
    
write_data([],File,Acc)->
    SumPos = [ float_to_binary(V, [{decimals, 6}]) || {V,_,_,_} <- Acc ],
    SumNeg = [ float_to_binary(V1, [{decimals, 6}]) || {_,V1,_,_} <- Acc ],
    Max = [ float_to_binary(Mx, [{decimals, 6}]) || {_,_,Mx,_} <- Acc ],
    Min = [ float_to_binary(Mn, [{decimals, 6}]) || {_,_,_,Mn} <- Acc ],
    write_line([<<"Total (+):">>|SumPos],File),
    write_line([<<"Total (-):">>|SumNeg],File),
    write_line([<<"MAX:">>|Max],File),
    write_line([<<"MIN:">>|Min],File),
    close_file(File),
    ok.
    
    
close_file(File)->
    file:close(File).