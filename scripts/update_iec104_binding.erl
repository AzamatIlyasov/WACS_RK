
-module(update_iec104_bindings).

-include("fp_struct.hrl").

-export([on_event/2]).

on_event({on_cycle,_Cycle},State)->
	fp:log(debug, "UPDATE_104_START"),
    AllIec104Connections = fp_db:query(<<"get .oid from root where AND( .pattern=$oid('/root/.patterns/iot_connection'), protocol='iec104' )">>),
    [begin
        Iec104ConObj = fp_db:open(OIDCon, none),
        {ok, FolderOID} = fp_db:read_field(Iec104ConObj, <<".folder">>),
        Path = fp_db:to_path(FolderOID),
        {ok, ConnName} = fp_db:read_field(Iec104ConObj, <<".name">>),
        AllBinidings = fp_db:query(<<"get .oid from root where AND( .folder=$oid('",Path/binary, "/", ConnName/binary,"'), .pattern=$oid('/root/.patterns/iot_binding') )">>),
        [begin
            Iec104BindObj = fp_db:open(BindOID, none),
            % {ok, BindName} = fp_db:read_field(Iec104BindObj, <<".name">>),
            {ok, In} = fp_db:read_field(Iec104BindObj, <<"in">>),
            {ok, Out} = fp_db:read_field(Iec104BindObj, <<"out">>),
            InMap = fp_lib:from_json(In),
            OutMap = fp_lib:from_json(Out),
            InMap1 = update_field(InMap),
            OutMap1 = update_field(OutMap),
            InMap2 = fp_lib:to_json(InMap1),
            OutMap2 = fp_lib:to_json(OutMap1),
            fp_db:edit_object(Iec104BindObj,
                #{
                    <<"in">> => InMap2,
                    <<"out">> => OutMap2
                })
            % fp:log(info, "SSS InMap :: ~p", [InMap]),
            % fp:log(info, "SSS InMap1 :: ~p", [InMap1]),
            % fp:log(info, "SSS OutMap :: ~p", [OutMap]),
            % fp:log(info, "SSS OutMap1 :: ~p", [OutMap1]),
            % fp:log(info, "SSS NAME :: ~p", [BindName])
        end
        || BindOID <- AllBinidings]
    end
    || OIDCon <- AllIec104Connections],
	fp:log(info, "UPDATE_104_END"),
   State;
on_event(_,State)-> State.

update_field(M) when is_map(M)->
    maps:map(
        fun(K,V) -> 
            case K of
                <<"com_address">> -> 
                    if
                        is_binary(V) -> bin2numb(V);
                        true -> V
                    end;
                <<"initial_req100">> -> 
                    if
                        is_binary(V) -> binary_to_atom(V, utf8);
                        true -> V
                    end;
                <<"qoi">> -> 
                    if
                        is_binary(V) -> bin2numb(V);
                        true -> V
                    end;
                _ ->
                    update_field(V)
            end
        end
        ,M);
update_field(L) when is_list(L)->
    lists:map(fun(El) -> update_field(El) end ,L);
update_field(V)->V.

bin2numb(<<"">>)-> 0;
bin2numb(Bin)-> 
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.
    


