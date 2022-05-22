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

-module(database).

-include("fp_struct.hrl").

-export([on_event/2]).

%% API
-export([
  tags_to_db/5,
  db_to_tags/4,
  csv_to_tags/4
]).


-define(SQL_INT, sql_integer).
-define(SQL_FLOAT, sql_real).
-define(SQL_TEXT, {sql_varchar, 50}).

% NODES %
-define(FIELD2TYPE1, #{
  <<"id">> => ?SQL_INT,
  <<"name">> => ?SQL_TEXT,
  <<"unom">> => ?SQL_TEXT,
  <<"u">> => ?SQL_TEXT,
  <<"shn">> => ?SQL_TEXT,
  <<"kombin">> => ?SQL_TEXT,
  <<"node_number">> => ?SQL_TEXT,
  <<"current">> => ?SQL_TEXT,
  <<"currentmax">> => ?SQL_TEXT,
  <<"currentmin">> => ?SQL_TEXT,
  <<"umax">> => ?SQL_TEXT,
  <<"res">> => ?SQL_TEXT,
  <<"x">> => ?SQL_TEXT,
  <<"y">> => ?SQL_TEXT,
  <<"ntype">> => ?SQL_INT,
  <<"gen_num">> => ?SQL_INT,
  <<"current_im">> => ?SQL_FLOAT,
  <<"qmax">> => ?SQL_FLOAT,
  <<"priority">> => ?SQL_INT,
  <<"pdef">> => ?SQL_FLOAT,
  <<"ex_nodes">> => ?SQL_TEXT,
  <<"last_nodes">> => ?SQL_TEXT,
  <<"p_gen_unused">> => ?SQL_FLOAT,
  <<"origin_node_number">> => ?SQL_INT,
  <<"changed_node_ident">> => ?SQL_TEXT,
  <<"time_of_change">> => ?SQL_TEXT,
  <<"p">> => ?SQL_TEXT,
  <<"q">> => ?SQL_TEXT,
  <<"v">> => ?SQL_TEXT,
  <<"qgenmin">> => ?SQL_TEXT,
  <<"qgenmax">> => ?SQL_TEXT,
  <<"pgen">> => ?SQL_TEXT,
  <<"qgen">> => ?SQL_TEXT,
  <<"b_r">> => ?SQL_TEXT,
  <<"delta">> => ?SQL_TEXT,
  <<"p_load">> => ?SQL_TEXT,
  <<"q_load">> => ?SQL_TEXT,
  <<"u_re">> => ?SQL_TEXT,
  <<"u_im">> => ?SQL_TEXT,
  <<"state">> => ?SQL_TEXT
}).
 
% BRANCHES %
-define(FIELD2TYPE2, #{
  <<"id">> => ?SQL_INT,
  <<"startnum">> => ?SQL_TEXT,
  <<"endnum">> => ?SQL_TEXT,
  <<"r">> => ?SQL_TEXT,
  <<"x">> => ?SQL_TEXT,
  <<"b">> => ?SQL_TEXT,
  <<"ktr">> => ?SQL_TEXT,
  <<"current">> => ?SQL_TEXT,
  <<"name">> => ?SQL_TEXT,
  <<"numpar">> => ?SQL_TEXT,
  <<"status">> => ?SQL_TEXT,
  <<"maxcurrent">> => ?SQL_TEXT,
  <<"x1">> => ?SQL_TEXT,
  <<"y1">> => ?SQL_TEXT,
  <<"x2">> => ?SQL_TEXT,
  <<"y2">> => ?SQL_TEXT,
  <<"vl_nodes">> => ?SQL_TEXT,
  <<"npar">> => ?SQL_TEXT,
  <<"current_im">> => ?SQL_FLOAT,
  <<"time_of_change">> => ?SQL_TEXT,
  <<"old_power_flow">> => ?SQL_FLOAT,
  <<"p_st">> => ?SQL_TEXT,
  <<"q_st">> => ?SQL_TEXT,
  <<"p_end">> => ?SQL_TEXT,
  <<"q_end">> => ?SQL_TEXT,
  <<"state">> => ?SQL_TEXT,
  <<"ktr_im">> => ?SQL_TEXT,
  <<"start_num">> => ?SQL_TEXT,
  <<"end_num">> => ?SQL_TEXT
}).

-define(TABLETYPES, #{
  "nodes" => ?FIELD2TYPE1,
  "branches" => ?FIELD2TYPE2
}).


on_event({on_cycle,Cycle},State)->
   % here should be your code that is executed each Cycle milliseconds
   State;
on_event({tag,TagID,Field,Value},State)->
   % here should be your code that is executed on change of the Field of the Tag
   State.


convert(integer, ?SQL_INT, Value) ->
  Value;
convert(integer, ?SQL_FLOAT, Value) ->
  float(Value);
convert(integer, ?SQL_TEXT, Value) ->
  integer_to_list(Value);
convert(float, ?SQL_INT, Value) ->
  round(Value);
convert(float, ?SQL_FLOAT, Value) ->
  Value;
convert(float, ?SQL_TEXT, Value) ->
  lists:sublist(float_to_list(Value), 10);
convert(string, ?SQL_INT, Value) ->
  binary_to_integer(Value);
convert(string, ?SQL_FLOAT, Value) ->
  binary_to_float(Value);
convert(string, ?SQL_TEXT, Value) ->
  binary_to_list(Value).


%Tags -> Database%
tags_to_db(ODBSstr, TableName, Folder, Fields, PatternName) ->
  BinFields = [list_to_binary(F) || F <- Fields],
  FolderPath = "/root/PROJECT/TAGS/" ++ Folder,
  OidList = fp_db:query("GET .oid FROM root WHERE .folder=$oid('"++ FolderPath ++"')"),
  Data1 = [fp_db:read_fields(fp_db:open(Oid), BinFields) || Oid <- OidList],

  InsertData =
    [begin
       FieldToType = maps:get(TableName, ?TABLETYPES),
       DBType = maps:get(F, FieldToType),
       {ok,TagType} = fp_db:read_field(fp_db:open(?OID(<<"/root/.patterns/", PatternName/binary,"/",F/binary>>)), <<"type">>),
       Values = [
         begin
           Val = maps:get(F, MapLine),
           convert(TagType, DBType, Val)
         end || MapLine <- Data1],
       {DBType, Values}
     end
    ||F <- BinFields],

  Param = "(" ++ lists:append(lists:join(",", ["?"||_ <- lists:seq(1, length(Fields))])) ++ ")",
  InsertQuery = "INSERT INTO " ++ TableName ++ " (" ++ lists:append(lists:join(",", Fields)) ++ ") VALUES " ++ Param,

  odbc:start(),
  {ok, Ref} = odbc:connect(ODBSstr, []),
  odbc:param_query(Ref, InsertQuery, InsertData),
  odbc:disconnect(Ref),
  odbc:stop().

% Database -> Tags
db_to_tags(ODBCstr, Query, Folder, PatternName) ->
  odbc:start(),
  {ok, Ref} = odbc:connect(ODBCstr,[]),
  {selected, Fields1, Data1} = odbc:sql_query(Ref, Query),
  odbc:disconnect(Ref),
  odbc:stop(),

  Fields = [list_to_binary(F) ||F <- Fields1],
  FolderPath = list_to_binary("/root/PROJECT/TAGS/" ++ Folder),
  Data = [
    [case is_list(Value) of
        true ->
          list_to_binary(Value);
        false ->
          integer_to_binary(Value)
      end
      || Value <- tuple_to_list(Line)]
    ||Line <- Data1],

  to_tags(FolderPath, Data, Fields, PatternName).

%CSV -> TAGS%
csv_to_tags(Folder, PathToCsv, Del, PatternName) ->
  FolderPath = list_to_binary("/root/PROJECT/TAGS/" ++ Folder),
  {ok, Content} = file:read_file(PathToCsv),
  [Fields1 | Data1] = string:tokens(binary_to_list(Content), "\r\n"),
  Data = [ [list_to_binary(Value) || Value <-string:split(Line, Del, all)] || Line <- Data1],
  Fields = [list_to_binary(Value) || Value <- string:tokens(Fields1, Del)],
  to_tags(FolderPath, Data, Fields, PatternName).

%Data -> TAGS%
to_tags(FolderPath, Data, TagFields, Pattern) ->
  [begin
     Fields = lists:zip(TagFields, Line),
     Name = lists:nth(1, Line),
     fp_db_1:edit_or_create([
       {<<".name">>, Name},
       {<<".folder">>, ?OID(FolderPath)},
       {<<".pattern">>, ?OID(<<"/root/.patterns/", Pattern/binary>>)} | Fields
     ], fun fp_db:from_string/2)
   end
  || Line <- Data],
  ok.
