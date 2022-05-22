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

-module(wacs_snap_drts).

-include("fp_struct.hrl").

-define(HOUR,3600000).
-define(DAY,86400000).
-define(TIMEZONE,6).

-export([
  on_event/2
]).

-export([
  current/0,
  load_plan/2
]).

on_event(_Event, State)->
  State.

load_plan(_ID, Archives)->
    [ begin

        fp:log(debug,"DEBUG wacs_snap_drts: load plan ~ts, values ~p",[A, [{T,V} || [T,V] <- Values ] ]),
        ok = fp_archive:insert_values(A, [{T,V} || [T,V] <- Values ])
    end || [ A | Values ] <- Archives],
    ok.


current()->
    T = fp_lib:get_datetime(),
    Snapshot = wacs_snapshot:get_snapshot_drts( T ),
    update_tags(Snapshot),
    ok.

update_tags( Snapshot )->
    [try
       Value = fp_lib:json_path(Path, Snapshot, none),
       Object = fp_db:open(<<"/root/PROJECT/", Tag/binary>>),
       fp_db:edit_object( Object, #{Field => Value } )
     catch
        _:Error:Stack->
            fp:log(error, "update tag from shapshot error ~p, ~p",[ Stack, Error ])
     end || { {Tag, Field}, Path } <- maps:to_list( wacs_snap_drts_tags:config() ) ], %% конфиг в отдельном файле
    ok.


