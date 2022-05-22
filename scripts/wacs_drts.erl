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

-module(wacs_drts).

-include("fp_struct.hrl").


-define(DRTS_FOLDER, "/mnt/poolFP/files/drts").
-define(DRTS_FTP_PATH, "/drts/logs/log"). %пока статичные файлы. живые файлы когда он считывает, то почему то не ждет когда файл освободится.

-define(DRTS_FTP_HOST_41, "10.9.60.41"). %41 - сервер1 - осн
-define(DRTS_FTP_HOST_42, "10.9.60.42"). %42 - сервер1 - рез (после отключ 41)
-define(DRTS_FTP_HOST_43, "10.9.60.43"). %43 - сервер2 - осн (после отключ 41 и 42)
-define(DRTS_FTP_HOST_44, "10.9.60.44"). %44 - сервер2 - рез (после отключ 41 и 42 и 43)

-define(WAIT_LOCK, 2500). % время ожидания файла блокировки

-define(DRTS_LOCK_FILE,"time.txt").

-define(DRTS_USER, "root").
-define(DRTS_PASSWORD, "root").

-define(DRTS_BRANCH_MAP,#{
    "nodes.csv" => <<"nodes">>,
    "links.csv" => <<"links">>
    %, "linksReact.csv" => <<"lr">>
    %, "nodesReact.csv" => <<"nr">>
}).

-export([on_event/2]).


on_event(_Event, State0 )->
    fp:set_value("TAGS/WACS_(FP)/drts/debug_trigger_drts","str", unicode:characters_to_binary(io_lib:format("~p", [start])) ),
    fp:set_value("TAGS/WACS_(FP)/drts/debug_trigger_drts","title", unicode:characters_to_binary(io_lib:format("~p", [start])) ),
    
    %{_, Time}= calendar:local_time(),
    %TS = fp_lib:get_datetime(),
    
    State1 = 
        if
            is_map( State0 )-> State0;
            true -> 
                % Initialize the state
                #{
                    ?DRTS_FTP_HOST_41 => #{
                        pid => undefined,
                        ts => undefined
                    }
                    ,
                    ?DRTS_FTP_HOST_42 => #{
                        pid => undefined,
                        ts => undefined
                    }
                    % , %% это второй сервер, еще не протестирован
                    % ?DRTS_FTP_HOST_43 => #{
                    %     pid => undefined,
                    %     ts => undefined
                    % }
                    % ,
                    % ?DRTS_FTP_HOST_44 => #{
                    %     pid => undefined,
                    %     ts => undefined
                    % }
                }
        end,
    %?LOGDEBUG("DEBUG: DRTS state1 ~p", [State1]),
    % Set lock on the files
    State =
        case find_master( maps:to_list( State1 ) ) of
            undefined->
                fp:log(warning,"DEBUG: DRTS UNDEFINED state1 ~p . to fun lock_ts", [State1]),
                maps:map( fun lock_ts/2, State1 );
            PreviousMaster->
                NewTS = lock_ts( PreviousMaster, maps:get(PreviousMaster, State1)),
                case NewTS of
                    #{ts:=undefined}->
                        NewMaster = hd( [I || {I,_} <- maps:to_list( State1 ), I=/= PreviousMaster ] ),
                        fp:log(warning,"DEBUG: drts switch to the new master ~p",[NewMaster]),
                        State1#{
                            PreviousMaster => NewTS,
                            NewMaster => lock_ts( NewMaster, maps:get(NewMaster, State1))
                        };
                    _->
                        State1#{
                            PreviousMaster => NewTS
                        }
                end
        end,
    
    
    % Read files from the master
    case find_master( maps:to_list( State ) ) of
        undefined->
            fp:log(error,"DEBUG: drts unable to load because no servers are available");
        Master->
            fp:log(debug,"DEBUG: drts the master is ~p",[Master]),
            
            % Loading the files
            LoadResults = load_files( maps:get(Master, State) ),
            %fp:log(debug,"DEBUG: drts load_files ~p",[ LoadResults ]),
            
            % Parse the files
            Data = read_files( LoadResults, #{} ),
            %fp:log(debug,"DEBUG: drts read_files ~p",[ Data ]),
            
            % Release lock
            unlock_ts( Master, maps:get(Master, State) ),
            
            % Update tags with the new data
            update_tags( Data ),
            fp:set_value("TAGS/WACS_(FP)/drts/debug_trigger_drts","title", <<"ok_65">> )
    end,
    
    
    State.
    
%%--------------------------------------------------------------------------
%%    API
%%--------------------------------------------------------------------------
lock_ts( IP, Data )->
    lock_ts(IP, Data, fp_lib:get_datetime() + ?WAIT_LOCK).
    
lock_ts( IP, #{ pid := PID } = Data, TS ) when is_pid(PID)->
    case ftp:recv_bin(PID, ?DRTS_LOCK_FILE) of
        {ok,TS0}->
            try 
                %%TS0 = <<"1628771031\r\n">>
                LockTS = binary_to_integer( binary:replace(TS0,<<"\r\n">>,<<>>) ), 
                %fp:log(debug,"DEBUG: drts set lock at ~p",[ IP ]),
                Data#{ ts => LockTS }
            catch
                _m1:_m2->
                    %fp:log(debug,"DEBUG: drts waiting for valid lock at ~p, t0: ~p", [IP,TS0]),
                    % repeat an attemp
                    timer:sleep(100), %delay
                    case fp_lib:get_datetime() of
                        DT when DT >= TS->
                            fp:log(warning,"DEBUG: drts unable to set lock at ~p , DT=~p , TS=~p , TS0=~p , _m1=~p , _m2=~p", [IP, DT, TS, TS0, _m1, _m2]),
                            Data#{ ts => undefined };
                        _->
                            lock_ts( IP, Data, TS )
                    end
            end;
        {error, Error}->
            %fp:log(debug,"DEBUG: drts waiting for lock at ~p", [IP]),
            timer:sleep( 1000 ),
            case fp_lib:get_datetime() of
                DT when DT >= TS->
                    fp:log(warning,"DEBUG: WARNING drts unable to set lock at ~p, error ~p", [IP,Error]),
                    Data#{ ts => undefined };
                _->
                    lock_ts( IP, Data, TS )
            end;
        _Unexpected->
            timer:sleep(100),
            case fp_lib:get_datetime() of
                DT when DT >= TS->
                    %fp:log(warning,"DEBUG: WARNING drts unable to set lock at ~p, unexpetced ~p", [IP,Unexpected]),
                    Data#{ ts => undefined };
                _->
                    lock_ts( IP, Data, TS )
            end
    end;
lock_ts( IP, _Data, TS ) ->
    case open_ftp( IP ) of
        {ok, PID}->
            fp:log(debug,"DEBUG: drts open_ftp in ~p",[ IP ]),
            lock_ts( IP, #{ pid => PID, ts => undefined }, TS);
        _Error->
            fp:log(error,"DEBUG: ERROR drts open_ftp in ~p",[ IP ]),
            #{ pid => undefined, ts => undefined }
            
    end.
    
unlock_ts(IP, #{ pid := PID } ) when is_pid(PID)->
    case ftp:delete( PID, ?DRTS_LOCK_FILE ) of
        ok ->
            ok;
        {error,Error}->
            fp:log(warning,"DEBUG: WARNING drts unable DELETE from FTP to unlock files at ~p, error ~p", [IP, Error])
    end;
unlock_ts(_IP, _Data )->
    % The connection is not active
    ignore.
    
    
open_ftp( IP )->
    case ftp:open( IP ) of
        {ok, PID}->
            case fp_lib:pipe([
                fun(_)-> ftp:user(PID, ?DRTS_USER, ?DRTS_PASSWORD) end,
                fun(_)-> ftp:cd(PID, ?DRTS_FTP_PATH) end,
                fun(_)-> ftp:lcd(PID, ?DRTS_FOLDER) end
            ],undefined) of
                {ok, _ }->
                    fp:log(debug,"DEBUG: drts ~p connected", [IP]),
                    {ok, PID};
                {error, _Step, InitError}->
                    fp:log(error,"DEBUG: drts init connection to ~p error ~p", [IP, InitError]),
                    close_ftp(IP, PID ),
                    {error, InitError}
            end;
        {error, ConnectError}->
            fp:log(error,"DEBUG: drts ftp connection to ~p error ~p", [IP, ConnectError]),
            {error, ConnectError}
    end.
    
close_ftp(IP, PID )->
    ftp:close(PID),
    receive
        {'EXIT',PID,normal}-> 
            fp:log(info,"drts ftp connection to ~p has closed",[IP]),
            ok
    after
        1000->fp:log(warning,"drts ftp connection to ~p timeout on close")
    end.

find_master( [{IP1, #{ts := TS1 } }, {IP2, #{ts := TS2}}] )->
    % The master is the server with the lates timestamp in the lock file
    if
        is_integer(TS1),is_integer(TS2)->
            if
                TS1 >= TS2->
                    IP1;
                true->
                    IP2
            end;
        is_integer(TS1)->
            IP1;
        is_integer(TS2)->
            IP2;
        true->
            undefined
    end.
    
load_files(#{pid := PID})->
    [ {F, ftp:recv(PID, F)} || {F,_} <- maps:to_list( ?DRTS_BRANCH_MAP ) ].
    
    
%%--------------------------------------------------------------------------
%%    Utilities
%%--------------------------------------------------------------------------
read_files( [{File, ok}| Rest], Data0 )->
    Branch = maps:get(File, ?DRTS_BRANCH_MAP),
    Data = Data0#{ Branch => wacs_csv:from_csv( File, ?DRTS_FOLDER ++ "/" ++ File ) },
    read_files( Rest, Data );
read_files( [{File, Error}| Rest], Data )->
    fp:log(error, "unable to looad drts file ~p, error: ~p",[ File, Error ]),
    read_files( Rest, Data );
read_files( [], Data )->
    Data.


update_tags( Data )->
    % The config is in the dedicated script module
    Config = ordsets:from_list(maps:to_list( wacs_snap_drts_tags:config() )),
    
    [try
        Value = fp_lib:json_path(Path, Data, none),
        Value1 =
            if
                Value =:= none->
                    fp:log(error,"invalid path in drts config ~p",[Path]),
                    none;
                Value =:= <<"-">>->
                    none;
                true->
                    Value
            end,
        case fp:set_value( Tag, Field, Value1 ) of
            ok->ok;
            {error,SetError}->
                fp:log(error, "unable to update drts tag ~ts, with value ~p, error ~p",[
                    Tag,
                    Value,
                    SetError
                ])
        end
     catch
        _:Error:Stack->
            fp:log(error, "update drts tag from shapshot error ~p, ~p",[ Stack, Error ])
     end || { {Tag, Field}, Path } <- Config ],
     
     fp:set_value("TAGS/WACS_(FP)/drts/debug_trigger_drts","str", unicode:characters_to_binary(io_lib:format("~p", [ok_141])) ),
     
     ok.

    
    