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

-module(wacs_csv).

-include("fp_struct.hrl").

% CSV files settings
-define(COLUMN_DELIMITER,<<";">>).
-define(ROW_DELIMITER,<<"\r\n">>).
-define(ROW_DELIMITER_snapshot,<<"\n">>). %% for wacs_snapshot

-export([
    on_event/2
]).

-export([
    to_csv/3,
    from_csv/2,
    to_csv/4, %% for wacs_snapshot - because in wacs_snapshot_template do not have some columns in template than wacs_csv template
    from_csv/3 %% for wacs_snapshot
]).

on_event(_Event,State)->
   State.

%%--------------------------------------------------------------------------
%%    API
%%--------------------------------------------------------------------------

%% for wacs_drts
to_csv( Data, Template, Filename )->
    Content  = data2csv( wacs_csv_templates:get( Template ), Data ),
    %?LOGINFO("DEBUG: Content ~p Filename ~p Template ~p ",[Content, Filename, Template ]),
    ResWrite = write_csv( Filename, Content ),
    ?LOGINFO("DEBUG: ResWrite ~p ",[ResWrite]),
    case flush_errors() of
        Errors when is_list(Errors), length(Errors)>0->
            {error, Errors};
        _->
            ok
    end.
    
from_csv( Template, Filename )->
    %?LOGINFO("DEBUG CSV: drts read data from csv file ~p, template ~p",[Filename, Template]),
    Content = read_csv( Filename, ?COLUMN_DELIMITER , ?ROW_DELIMITER ),
    Data = csv2data( wacs_csv_templates:get( Template ), Content, #{} ),
    to_json( Data ).


% %% for wacs_snapshot
% to_csv( Data, Template, Filename, CutTailLength)->
%     TemplateData0 = wacs_csv_templates:get( Template ),
%     Length = length(hd(TemplateData0)) - CutTailLength,
%     TemplateData = 
%         [ lists:sublist( T, Length ) || T <- TemplateData0 ],
    
%     Content  = data2csv( TemplateData, Data ),
%     write_csv( Filename, Content ),
%     case flush_errors() of
%         Errors when is_list(Errors), length(Errors)>0->
%             {error, Errors};
%         _->
%             ok
%     end.
% %% for wacs_snapshot    
% from_csv( Template, Filename, CutTailLength)->
%     ?LOGINFO("DEBUG CSV: snapshot read data from csv file ~p, template ~p",[Filename, Template]),
%     Content = read_csv( Filename, ?COLUMN_DELIMITER , ?ROW_DELIMITER_snapshot ), %% for wacs_snapshot - because snapshot csv difference than drts csv
%     TemplateData0 = wacs_csv_templates:get( Template ),
%     Length = length(hd(TemplateData0)) - CutTailLength,
%     TemplateData = 
%         [ lists:sublist( T, Length ) || T <- TemplateData0 ],
%     Data = csv2data( TemplateData, Content, #{} ),
%     to_json( Data ).


%% for wacs_snapshot
to_csv( Data, Template, Filename, snapshot)->
    %TODO - change to wacs_csv_templates:get( Template ) whith delete columns - because in wacs_snapshot_template do not have some columns in template
    Content  = data2csv( wacs_snapshot_template:csv_template( Template ), Data ),
    %?LOGINFO("DEBUG: Snapshot Content ~p Filename ~p Template ~p ",[Content, Filename, Template ]),
    ResWrite = write_csv( Filename, Content ),
    ?LOGINFO("DEBUG: Snapshot ResWrite ~p ",[ResWrite]),
    case flush_errors() of
        Errors when is_list(Errors), length(Errors)>0->
            {error, Errors};
        _->
            ok
    end.
%% for wacs_snapshot    
from_csv( Template, Filename, snapshot)->
    ?LOGINFO("DEBUG CSV: Snapshot read data from csv file ~p, template ~p",[Filename, Template]),
    Content = read_csv( Filename, ?COLUMN_DELIMITER , ?ROW_DELIMITER_snapshot ), %% for wacs_snapshot - because snapshot csv difference than drts csv
    %TODO - change to wacs_csv_templates:get( Template ) whith delete columns - because in wacs_snapshot_template do not have some columns in template
    %?LOGINFO("DEBUG CSV: Content ~p ",[Content]),
    Data = csv2data( wacs_snapshot_template:csv_template( Template ), Content, #{} ),
    to_json( Data ).

    
    
%%--------------------------------------------------------------------------
%%    Export to CSV
%%--------------------------------------------------------------------------

data2csv( List, Data ) when is_list( List )->
  H = hd( List ),
  if
    is_list( H ); is_tuple( H )->
      [ data2csv( I, Data ) || I <- List ];
    true->
      % This is the path
      Path = parse_path( List ),
      Value = fp_lib:json_path( Path, Data, undefined ),
      if
        Value=:=undefined ->
          on_error({invalid_data_path, Path}),
          % TODO. What to return???
          <<"0">>;
        true ->
          value2csv( Value )
      end
  end;
data2csv({const, Value}, _Data)->
  value2csv( Value ).


value2csv( Value ) when is_integer( Value )->
  integer_to_binary( Value );
value2csv( Value ) when is_float( Value )->
  float_to_binary( Value ,[{decimals, 3}]);
value2csv( Value ) when is_atom( Value )->
  atom_to_binary( Value, utf8 );
value2csv( Value ) when is_list( Value )->
  % CSV encoding is DOS cyrillic
  translate( Value );
value2csv( Value ) when is_binary( Value )->
  Value.

write_csv( FileName, Data )->
  Content =
    fp_lib:join_binary( [ fp_lib:join_binary( Row, ?COLUMN_DELIMITER ) || Row <- Data ], ?ROW_DELIMITER ),
  case file:write_file( FileName, Content, [write] ) of
    ok -> ok;
    {error, Error}-> on_error( Error )%erlang:error( Error )
  end.

%%--------------------------------------------------------------------------
%%    Import from CSV
%%--------------------------------------------------------------------------
read_csv( FileName, COLUMN_DELIMITER, ROW_DELIMITER)->
  %?LOGINFO("DEBUG CSV: snapshot read csv file ~p, c ~p , r ~p ",[FileName, COLUMN_DELIMITER, ROW_DELIMITER]),
  case file:read_file( FileName ) of
    {ok, Content}->
        Data = 
            [ [ csv2value(V) || V <- binary:split( Row, COLUMN_DELIMITER, [global] )] || Row <- binary:split( Content, ROW_DELIMITER, [global] ) ],
        % Remove empty rows
        [ Row || Row <- Data, length(Row) > 1 ];
    {error, Error}->
      on_error( Error )%erlang:error( Error )
  end.

csv2data( Template, Content, Data )->
    if
        length(Template) =:= length( Content )-> ok;
        true->
            %%%loginfo length problems
            % fp:set_value("TAGS/WACS_(FP)/drts/debug_trigger_drts","str", unicode:characters_to_binary(io_lib:format("template ~p, file ~p", [
            %     length(Template),
            %     length( Content )
            %     ]) )),
            ?LOGINFO("DEBUG CSV: snapshot read csv file lTemp ~p lCont ~p temp ~p cont ~p data ~p ",[length(Template), length( Content ), Template, Content, Data ]),
            on_error( invalid_csv_template_csv191 )
            %erlang:error( invalid_csv_template )
    end,
    
    Adjusted = 
        [ lists:sublist( C, length(T) ) || {C,T} <- lists:zip(Content, Template) ],
    
    Values =    lists:zip( lists:append(Template), lists:append( Adjusted) ),
    csv2data( Values, Data ).
csv2data([{{const, _}, _}| Rest], Data )->
  csv2data( Rest, Data );
csv2data([{Path, Value}| Rest], Data )->
    Data1 = 
        case parse_path( Path ) of
            [] -> Data;
            Path1->
                set_json_leaf( Path1, Value, Data )
        end,
    csv2data( Rest, Data1 );
csv2data([], Data )->
    Data.

csv2value( Value )->
  try
    fp_lib:from_json( Value )
  catch
    _:_->
      Value
  end.

set_json_leaf([Branch | Rest], Value, Data ) when is_map( Data )->
  Map = maps:get( Branch, Data, #{}),
  NewMap =   if is_map(Map) -> Map;
      true -> #{}
    end,
  Data#{ Branch => set_json_leaf( Rest, Value, NewMap ) };
set_json_leaf([], Value, _Data )->
  Value.

to_json( Data )->
    Data1 = fp_db:to_json(term, Data),
    check_json( Data1 ).

check_json( Map ) when is_map( Map )->
    maps:fold(fun(K,V,Acc)->
        Acc#{ unicode:characters_to_binary( K ) => check_json( V ) }
    end,#{}, Map);
check_json( List ) when is_list(List)->
    [ check_json( I ) || I <- List ];
check_json( Item )->
    Item.
    
%%--------------------------------------------------------------------------
%%    Common utilities
%%--------------------------------------------------------------------------
parse_path( Path )->
  [ Name || Name <- string:split(Path, "/",all), length(Name) > 0 ].
  
  
translate( Key )->
  maps:get( Key, #{

    % nodes.csv
    "node_number" => <<252,32,227,167,171,160>>,
    "node_state" => <<145,174,225,226,174,239,173,168,165>>,
    "node_Pn" => <<80,173>>,
    "node_Qn" => <<81,173>>,
    "node_Pg" => <<80,163>>,
    "node_Qg" => <<81,163>>,
    % "node_mPn" => <<80,173>>, %%% m - новый столбцы
    % "node_mQn" => <<81,173>>,
    % "node_mPg" => <<80,163>>,
    % "node_mQg" => <<81,163>>,

    % links.csv
    "link_start" => <<141,160,231,46>>,
    "link_end" => <<138,174,173,46>>,
    "link_num" => <<252,32,230,165,175,168>>,
    "link_state" => <<145,174,225,226,174,239,173,168,165>>,
    "link_Pn" => <<80,173>>,
    "link_Qn" => <<81,173>>,
    "link_Pk" => <<80,170>>,
    "link_Qk" => <<81,170>>,
    % ,
    % "link_mPn" => <<80,173>>,%%%m - новый столбцы
    % "link_mQn" => <<81,173>>,
    % "link_mPk" => <<80,170>>,
    % "link_mQk" => <<81,170>>
    
    

    % nodesReact.csv
    "nr_n_number" => <<78,32,227,167,171,160>>,
    "nr_r_number" => <<78,32,224,165,160,170,226,174,224,160>>,
    "nr_conduct" => <<143,224,174,162,174,164,168,172,174,225,226,236>>,
    "nr_state" => <<145,174,225,226,174,239,173,168,165>>,
    "nr_weight" => <<130,165,225>>,

    % linksReact.csv
    "lr_start" => <<78,112>>,
    "lr_end" => <<78,113>>,
    "lr_num" => <<78,32,230,165,175,168>>,
    "lr_r_num" => <<78,32,224,165,160,170,226,174,224,160>>,
    "lr_conduct" => <<143,224,174,162,174,164,168,172,174,225,226,236>>,
    "lr_state" => <<145,174,225,226,174,239,173,168,165>>,
    "lr_weight" => <<130,165,225>>


  }, unicode:characters_to_binary(Key) ).
  
%%--------------------------------------------------------------------------
%%    Error handling utilities
%%--------------------------------------------------------------------------
on_error( Error )->
    ?LOGINFO("DEBUG CSV: on error wacs_csv ~p",[ Error ]),
    case get('@wacs_csv_errors@') of
        EE when is_list(EE)->
            put('@wacs_csv_errors@', EE ++ [ Error ]);
        _ 
            -> put('@wacs_csv_errors@',[ Error ])
    end.
    % ?LOGINFO("DEBUG CSV: the number of rows in the csv template differs from the number of rows in the file, template ~p, file ~p",[
    %             length(Template),
    %             length( Content )
    %         ]),

flush_errors()->
  erase('@wacs_csv_errors@').
 