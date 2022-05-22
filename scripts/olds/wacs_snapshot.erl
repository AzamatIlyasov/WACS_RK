
-module(wacs_snapshot).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  ALGORITHM SETTINGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The folder with data files for the integration
-define(CSV_FOLDER, "/mnt/poolFP/files/radv").
%-define(DRTS_FOLDER, "/home/wacs/RADV/radv/drts").
%-define(DRTS_FTP_PATH, "/drts_wacs/logs/log").
%-define(DRTS_FTP_FILES, ["nodes.csv", "links.csv", "linksReact.csv", "nodesReact.csv"]).
%-define(DRTS_FTP_HOST, "10.9.60.41").
%-define(DRTS_USER, "root").
%-define(DRTS_PASSWORD, "root").

% The command to perform calculations
-define(UTILITY_CMD," cd /mnt/poolFP/files/radv/ && timeout 5 ./radv -l & echo \"$!\" && sleep 6 ").

% CSV files settings
-define(COLUMN_DELIMITER,<<";">>).
-define(ROW_DELIMITER,<<"\n">>).

%% A error will be raised if the value in the requested archive is much older than the requested timestamp.
%% Error if the value is older that 2 hours
-define( MAX_DT, 2 * 3600 * 1000 ).

-export([
  on_event/2
]).

-export([
  get_snapshot/1,
  get_snapshot_ras/1,
  read_tag/2,
  read_archive/2
  %, get_snapshot_drts/1
]).

on_event(_, State)->
  % here should be your code that is executed each Cycle milliseconds
  State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  THE ENTRY POINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% load_drts_files() ->
%   case ftp:open(?DRTS_FTP_HOST) of
%     {ok, PID} ->
%       case ftp:user(PID, ?DRTS_USER, ?DRTS_PASSWORD) of
%         ok ->
%           case ftp:cd(PID, ?DRTS_FTP_PATH) of
%             ok ->
%               case ftp:lcd(PID, ?DRTS_FOLDER) of
%                 ok ->
%                   lists:foldl(fun(F, Result) ->
%                      case ftp:recv(PID, F)  of
%                       ok ->
%                          if Result =:= ok -> ok;
%                           true -> error
%                          end;
%                       {error, Reason4}  ->
%                           fp:log(error, "FTP RECEIVE FILE ~p ERROR ~p",[F, Reason4]),
%                           error
%                      end end, ok, ?DRTS_FTP_FILES),
%                      ftp:close(PID);
%                 {error, Reason3} ->
%                   fp:log(error, "FTP LOCAL CD ERROR ~p",[Reason3]),
%                   ftp:close(PID),
%                   error
%               end;
%              {error, Reason2} ->
%               fp:log(error, "FTP CD ERROR ~p",[Reason2]),
%               ftp:close(PID),
%               error
%           end;
%         {error, Reason1} ->
%           ftp:close(PID),
%           fp:log(error, "FTP LOGIN ERROR ~p",[Reason1])
%       end;

%     {error, Reason} ->
%       fp:log(error, "FTP OPEN ERROR ~p",[Reason]),
%       error
%   end.

get_snapshot( TS )->

  % Build the initial data tree
  fp:log(info, "DEBUG wacs_snapshot RADV: evaluate the template ~p",[TS]),
  Data0 = evaluate_template( wacs_snapshot_template:template(), TS ),
  fp:log(info, "DEBUG wacs_snapshot RADV: evaluate data ~p",[Data0]),

  % Map data to the csv format
  fp:log(info, "DEBUG wacs_snapshot RADV: map csv templates"),
  Files =
    [ { F, data2csv( wacs_snapshot_template:csv_template( F ), Data0 ) } || F <- [
      "nodes.csv",
      "links.csv",
      "nodesReact.csv",
      "linksReact.csv"
    ]],

  % Write csv files
  fp:log(info, "DEBUG wacs_snapshot RADV: write to csv"),
  [ write_csv( File, Content ) || {File, Content} <- Files ],

  %%-------------------Run the calculations---------------------------------
  fp:log(info, "DEBUG wacs_snapshot RADV: run utility"),
  run_utility(),

  % read the results
  fp:log(info, "DEBUG wacs_snapshot RADV: read pred.csv"),
  PredCSV = read_csv( "out/pred.csv" ),

  fp:log(info, "DEBUG wacs_snapshot RADV: parse pred.csv"),
  Data1 = csv2data( wacs_snapshot_template:csv_template( "pred.csv" ), PredCSV, Data0, [] ),

%   DrtsNodesCSV = read_csv( "data/nodes.csv" ),
%   fp:log(debug, "DEBUG wacs_snapshot RADV: parse drts/nodes.csv"),
%   Data2 = csv2data( csv_template( "nodes.csv" ), DrtsNodesCSV, Data1, ["drts","nodes"] ),

%   DrtsLinksCSV = read_csv( "data/links.csv" ),
%   fp:log(debug, "DEBUG wacs_snapshot RADV: parse drts/links.csv"),
%   Data3 = csv2data( csv_template( "nodes.csv" ), DrtsLinksCSV, Data2, ["drts","links"] ),

  to_json( Data1#{ errors => flush_errors() }).

% get_snapshot_drts( TS )->

%   % Build the initial data tree
%   fp:log(info, "DEBUG wacs_snapshot RADV: DRTS evaluate the template ~p",[TS]),
%   Data0 = evaluate_template( wacs_snapshot_template:template(), TS ),
%   %fp:log(debug, "DEBUG wacs_snapshot RADV: DRTS evaluate data ~p",[Data0]),


%   case load_drts_files() of
%     ok ->
%       DrtsNodesCSV = read_csv( "drts/nodes.csv" ),
%       fp:log(info, "DEBUG wacs_snapshot RADV: DRTS parse drts/nodes.csv"),
%       Data2 = csv2data( wacs_snapshot_template:csv_template( "nodes.csv" ), DrtsNodesCSV, Data0, ["drts"] ),
%       DrtsLinksCSV = read_csv( "drts/links.csv" ),
%       fp:log(info, "DEBUG wacs_snapshot RADV: DRTS parse drts/links.csv"),
%       Data3 = csv2data( wacs_snapshot_template:csv_template( "links.csv" ), DrtsLinksCSV, Data2, ["drts"] ),
%       DrtsNodesReactCSV = read_csv( "drts/nodesReact.csv" ),
%       fp:log(info, "DEBUG wacs_snapshot RADV: DRTS parse drts/nodesReact.csv"),
%       Data4 = csv2data( wacs_snapshot_template:csv_template( "nodesReact.csv" ), DrtsNodesReactCSV, Data3, ["drts"] ),
%       DrtsLinksReactCSV = read_csv( "drts/linksReact.csv" ),
%       fp:log(info, "DEBUG wacs_snapshot RADV: DRTS parse drts/links.csv"),
%       Data5 = csv2data( wacs_snapshot_template:csv_template( "linksReact.csv" ), DrtsLinksReactCSV, Data4, ["drts"] ),
%       to_json( Data5#{ errors => flush_errors() });
%     true ->
%       to_json( Data0#{ errors => flush_errors() })
%   end.


get_snapshot_ras( TS )->

  % Build the initial data tree
  fp:log(info, "DEBUG wacs_snapshot RADV _RAS: evaluate the template ~p",[TS]),
  Data0 = evaluate_template( wacs_snapshot_template_ras:template(), TS ),
  fp:log(info, "DEBUG wacs_snapshot RADV _RAS: evaluate data ~p",[Data0]),

  % Map data to the csv format
  fp:log(info, "DEBUG wacs_snapshot RADV _RAS: map csv templates"),
  Files =
    [ { F, data2csv( wacs_snapshot_template_ras:csv_template( F ), Data0 ) } || F <- [
      "nodes.csv",
      "links.csv",
      "nodesReact.csv",
      "linksReact.csv"
    ]],

  % Write csv files
  fp:log(info, "DEBUG wacs_snapshot RADV _RAS: write to csv"),
  [ write_csv( File, Content ) || {File, Content} <- Files ],

  %%-------------------Run the calculations---------------------------------
  fp:log(info, "DEBUG wacs_snapshot RADV _RAS: run utility"),
  run_utility(),

  % read the results
  fp:log(info, "DEBUG wacs_snapshot RADV _RAS: read and parse result out/*.csv"),
  
   NodesCSV = read_csv( "out/nodes.csv" ),
%   fp:log(debug, "DEBUG wacs_snapshot RADV: parse out/nodes.csv"),
   Data1 = csv2data( wacs_snapshot_template:csv_template( "nodes.csv" ), NodesCSV, Data0, ["out","nodes"] ),

   LinksCSV = read_csv( "out/links.csv" ),
%   fp:log(debug, "DEBUG wacs_snapshot RADV: parse out/links.csv"),
   Data2 = csv2data( wacs_snapshot_template:csv_template( "links.csv" ), LinksCSV, Data1, ["out","links"] ),

  PredCSV = read_csv( "out/pred.csv" ),
  Data3 = csv2data( wacs_snapshot_template:csv_template( "pred.csv" ), PredCSV, Data2, ["out","predcsv"] ),


  fp:log(info, "DEBUG wacs_snapshot RADV _RAS: data3 ~p ",[Data3]),

  to_json( Data3#{ errors => flush_errors() }).

% get_snapshot_drts( TS )->

%   % Build the initial data tree
%   fp:log(info, "DEBUG wacs_snapshot RADV: DRTS evaluate the template ~p",[TS]),
%   Data0 = evaluate_template( wacs_snapshot_template:template(), TS ),
%   %fp:log(debug, "DEBUG wacs_snapshot RADV: DRTS evaluate data ~p",[Data0]),


%   case load_drts_files() of
%     ok ->
%       DrtsNodesCSV = read_csv( "drts/nodes.csv" ),
%       fp:log(info, "DEBUG wacs_snapshot RADV: DRTS parse drts/nodes.csv"),
%       Data2 = csv2data( wacs_snapshot_template:csv_template( "nodes.csv" ), DrtsNodesCSV, Data0, ["drts"] ),
%       DrtsLinksCSV = read_csv( "drts/links.csv" ),
%       fp:log(info, "DEBUG wacs_snapshot RADV: DRTS parse drts/links.csv"),
%       Data3 = csv2data( wacs_snapshot_template:csv_template( "links.csv" ), DrtsLinksCSV, Data2, ["drts"] ),
%       DrtsNodesReactCSV = read_csv( "drts/nodesReact.csv" ),
%       fp:log(info, "DEBUG wacs_snapshot RADV: DRTS parse drts/nodesReact.csv"),
%       Data4 = csv2data( wacs_snapshot_template:csv_template( "nodesReact.csv" ), DrtsNodesReactCSV, Data3, ["drts"] ),
%       DrtsLinksReactCSV = read_csv( "drts/linksReact.csv" ),
%       fp:log(info, "DEBUG wacs_snapshot RADV: DRTS parse drts/links.csv"),
%       Data5 = csv2data( wacs_snapshot_template:csv_template( "linksReact.csv" ), DrtsLinksReactCSV, Data4, ["drts"] ),
%       to_json( Data5#{ errors => flush_errors() });
%     true ->
%       to_json( Data0#{ errors => flush_errors() })
%   end.




%%--------------------------------------------------------------------------
%%    Read the data from Faceplate sources
%%--------------------------------------------------------------------------
evaluate_template( Level, TS ) when is_map( Level )->
  maps:map( fun(_Key, Value)-> evaluate_template( Value, TS ) end, Level );
evaluate_template( Leaf, TS ) when is_function( Leaf )->
  try
    Leaf( TS )
  catch
    _:Error ->
      on_error( Error ),
      none
  end;
evaluate_template( Term, _TS )->
  Term.

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
      %fp:log(info, "DEBUG wacs_snapshot RADV: BValue=~p", [Value]),
      if
        Value=:=undefined ->
          %fp:log(info, "DEBUG wacs_snapshot RADV: UValue=~p", [Value]),
          %on_error({invalid_data_path, Path}),
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

  case file:write_file( ?CSV_FOLDER ++"/" ++ FileName, Content, [write] ) of
    ok -> ok;
    {error, Error}-> erlang:error( Error )
  end.

%%--------------------------------------------------------------------------
%%    Import from CSV
%%--------------------------------------------------------------------------
read_csv( FileName )->
  case file:read_file( ?CSV_FOLDER ++ "/" ++ FileName ) of
    {ok, Content}->
      [ binary:split( Row, ?COLUMN_DELIMITER, [global] ) || Row <- binary:split( Content, ?ROW_DELIMITER, [global] ) ];
    {error, Error}->
      erlang:error( Error )
  end.

csv2data([T | TRest ], [V | VRest], Data, Branch )->
  csv2data( TRest, VRest, csv2data( T, V, Data, Branch ), Branch );
csv2data([], [], Data, _Branch )->
  Data;
csv2data({const, _}, _Value, Data, _Branch )->
  Data;
csv2data(Path, Value, Data, Branch )->
    case parse_path( Path ) of
        [] -> Data;
        Path1->
            Value1 = csv2value( Value ),
            %fp:log(info,"DEBUG wacs_snapshot RADV: set json value, path ~p, value ~p",[ Path1, Value1 ]),
            set_json_leaf( Branch ++ Path1, Value1, Data )
    end.

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

read_tag( Tag, Field )->
  % TODO. Default value is 0?
  read_tag( Tag, Field, 0 ).
read_tag( Tag, Field, Default )->
    case fp:get_value( Tag, Field, Default ) of
        {ok, Value} -> Value;
        {error, Error}->
            fp:log(info, "DEBUG wacs_snapshot RADV: READ TAG ERROR"),
            on_error( {Tag, Field, Error} ),
            Default
    end.

read_archive( Archive, TS )->
  % TODO. Default value is 0?
  read_archive( Archive, TS, 0 ).
read_archive( Archive, TS, Default )->
    %% Archive = <<"/root/PROJECT/TAGS/_TEST (aues)/test/archive1">>,
    % TODO. REMOVE IT WHEN ARCHIVES ARE READY
   
    case fp_archive:get_point( Archive, TS ) of
        {_TS, undef} ->
          %fp:log(info,"DEBUG wacs_snapshot RADV: ~ts UNDEF at ~p , ts=~p , ts1=~p ",[ Archive, TS, _TS ]),
          %on_error( {undefined_value_archive, Archive, fp_lib:dt_to_string( TS )} ),
          Default;
        {_TS, none} ->
          %fp:log(info,"DEBUG wacs_snapshot RADV: ~ts none value at ~p",[ Archive, TS ]),
          %on_error( {none_value_archive, Archive, fp_lib:dt_to_string( TS )} ),
          Default;
        { LastTS, Value }->
          %fp:log(info,"DEBUG wacs_snapshot RADV: ~ts value ~p at ~p",[ Archive, Value, LastTS ]),
          % TODO. REMOVE %% when wacs ready
        %   if
        %     TS - LastTS > ?MAX_DT->
        %       on_error( {outdated_value_archive, Archive, fp_lib:dt_to_string( LastTS) } );
        %     true-> ok
        %   end,
          Value
        
    end.

run_utility()->
  %%ResPID = os:getpid(),
  Res = os:cmd( ?UTILITY_CMD ),
%   fp:log(info,"DEBUG wacs_snapshot RADV: utility sleep begin ~p ",[ Res ]),
%   timer:sleep(10000),
  fp:log(info,"DEBUG wacs_snapshot RADV: utility result ~p ",[ Res ]),
  ok.

translate( Key )->
  maps:get( Key, #{

    % nodes.csv
    "node_number" => <<252,32,227,167,171,160>>,
    "node_state" => <<145,174,225,226,174,239,173,168,165>>,
    "node_Pn" => <<80,173>>,
    "node_Qn" => <<81,173>>,
    "node_Pg" => <<80,163>>,
    "node_Qg" => <<81,163>>,

    % links.csv
    "link_start" => <<141,160,231,46>>,
    "link_end" => <<138,174,173,46>>,
    "link_num" => <<252,32,230,165,175,168>>,
    "link_state" => <<145,174,225,226,174,239,173,168,165>>,
    "link_Pn" => <<80,173>>,
    "link_Qn" => <<81,173>>,
    "link_Pk" => <<80,170>>,
    "link_Qk" => <<81,170>>,

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
    case Error of
        {Path,Leaf,{badmatch,{error,{invalid_path,Path}}}}->
            fp:log(error,"DEBUG wacs_snapshot RADV: on ERROR err ~p",[ Error ]);
            %fp:log(error,"DEBUG wacs_snapshot RADV: on ERROR, leaf ~p, invalid path ~ts , err ~p",[ Leaf, Path, Error ]);
        % {Path,Leaf,{invalid_data_path,{error,{invalid_data_path,Path}}}}->
        %     fp:log(error,"DEBUG wacs_snapshot RADV: on ERROR, leaf ~p, invalid_data_path  ~ts",[ Leaf, Path ]);    
        % {Path,Leaf,{invalid_data_path,{error,{invalid_data_path,Path}}}}->
        %     fp:log(error,"DEBUG wacs_snapshot RADV: on ERROR, leaf ~p, invalid_data_path  ~ts",[ Leaf, Path ]); 
        _->
            fp:log(error,"DEBUG wacs_snapshot RADV: on ERROR ~p ",[ Error ])
    end,

  case get('@errors@') of
    undefined -> put('@errors@',[ Error ]);
    EE -> put('@errors@', EE ++ [ Error ])
  end.

flush_errors()->
  erase('@errors@').






