
-module(wacs_snapshot).
-include("fp_struct.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  ALGORITHM SETTINGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The folder with data files for the integration
-define(CSV_FOLDER, "/mnt/poolFP/files/radv/main/").

-define(CSV_BRANCH_MAP,#{
    "nodes.csv" => <<"nodes">>
    , "links.csv" => <<"links">>
    , "linksReact.csv" => <<"lr">>
    , "nodesReact.csv" => <<"nr">>
}).

% The command to perform calculations
-define(UTILITY_CMD_DIR," cd /mnt/poolFP/files/radv/main/ ").
-define(UTILITY_CMD_RUN," timeout 6 ./radv -l & echo \"$!\" && sleep 4 ").
%-define(UTILITY_CMD1," cd /mnt/poolFP/files/radv/H").
% -define(UTILITY_CMD1," cd /mnt/poolFP/files/radv/main/").
% -define(UTILITY_CMD2," && timeout 5 ./radv -l & echo \"$!\" && sleep 3 ").
-define(UTILITY_CMD1," ./1_clean.sh ").
-define(UTILITY_CMD2," ./2_mv_to_hour.sh ").  %need params H01 ... 
-define(UTILITY_CMD3," ./3_clean_main.sh "). 

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
  read_tag/2,
  read_archive/2,
  read_archive/3,
  read_for_util/3
]).

on_event(_, State)->
  % here should be your code that is executed each Cycle milliseconds
  State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  THE ENTRY POINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_snapshot( TS )->
%   % Build the initial data tree
%   fp:log(info, "DEBUG wacs_snapshot RADV _RAS: evaluate the template ~p",[TS]),
%   DataTemp = evaluate_template( wacs_snapshot_template:template(), TS ), %%TODO. Remove it when ready predict
%   %DataTemp = evaluate_template( wacs_snapshot_template_ras:template(), TS ), %%%% _RAS
%   %fp:log(info, "DEBUG wacs_snapshot RADV: evaluate data ~p",[DataTemp]),

%   % Map data to the csv format
%   fp:log(info, "DEBUG wacs_snapshot RADV: map csv templates"),
%   Files =
%     [ { F, data2csv( wacs_snapshot_template:csv_template( F ), DataTemp ) } || F <- [
%       "nodes.csv",
%       "links.csv",
%       "nodesReact.csv",
%       "linksReact.csv"
%     ]],

%   % Write csv files - TODO - uncomment when load ready
%   %fp:log(info, "DEBUG wacs_snapshot RADV: write to csv"),
%   %[ write_csv( File, Content ) || {File, Content} <- Files ],

%   %%-------------------Run the calculations---------------------------------
%   fp:log(info, "DEBUG wacs_snapshot RADV: run utility"),
%   run_utility(),
    
%   % read OUT.TXT
%   fp:log(info, "DEBUG wacs_snapshot RADV: read   OUT.TXT"),
%   OutTxt = read_txt( "out.txt" ),   
%   %fp:log(info, "DEBUG wacs_snapshot RADV: read   OUT.TXT is ~p ", [OutTxt]), 
%   fp:set_value("TAGS/WACS_(FP)/radv/trigger_radv","str", unicode:characters_to_binary(io_lib:format("~p", [OutTxt])) ),
  
%   % read the results
%   fp:log(info, "DEBUG wacs_snapshot RADV: read and parse result out/*.csv"),
      
%   NodesCSV = read_csv( "out/nodes.csv" ),
%   %fp:log(info, "DEBUG wacs_snapshot RADV: parse out/nodes.csv , ~p ", [NodesCSV]),
%   DataNodes = csv2data( wacs_snapshot_template:csv_template( "nodes.csv" ), NodesCSV, DataTemp, ["nodes"] ),
%   %fp:log(info, "DEBUG wacs_snapshot RADV: DataNodes data: ~p ",[DataNodes]),
  
%   LinksCSV = read_csv( "out/links.csv" ),
%   %fp:log(info, "DEBUG wacs_snapshot RADV: parse out/links.csv , ~p ", [LinksCSV]),
%   DataLinks = csv2data( wacs_snapshot_template:csv_template( "links.csv" ), LinksCSV, DataNodes, ["links"] ),
%   %fp:log(info, "DEBUG wacs_snapshot RADV: DataLinks data: ~p ",[DataLinks]),
  
%   PredCSV = read_csv( "out/pred.csv" ),
%   fp:log(info, "DEBUG wacs_snapshot RADV: parse out/pred.csv , ~p ", [PredCSV]),
%   DataPred = csv2data( wacs_snapshot_template:csv_template( "pred.csv" ), PredCSV, DataLinks, ["pred"] ),
%   fp:log(info, "DEBUG wacs_snapshot RADV: DataPred data: ~p ",[DataPred]),
  
%   to_json( DataPred#{ errors => flush_errors() }).

%%% AZA - change 
get_snapshot( TS )->
  % Build the initial data tree
  ?LOGINFO("DEBUG wacs_snapshot RADV : evaluate the template ts ~p ",[TS]),
  {{_Y,_M,_D},{Hour,_,_}} = calendar:system_time_to_universal_time(TS,1000),
  <<YYYY:4/binary,"-",MM:2/binary,"-",DD:2/binary," ",HH:2/binary,_/binary>> = fp_lib:dt_to_local( TS ),
  HHStr = binary_to_list(HH),%integer_to_list(Hour),
  DDStr = binary_to_list(DD),
  MMStr = binary_to_list(MM),
  YYYYStr = binary_to_list(YYYY),
  YMDStr = YYYYStr++MMStr++DDStr,
  ?LOGINFO("DEBUG wacs_snapshot RADV : Ys ~p Ms ~p Ds ~p Hs ~p YMDStr ~p ,HL ~p ",[YYYYStr,MMStr,DDStr,HHStr,YMDStr, integer_to_list(Hour) ]),
  %%FFolder = ?CSV_FOLDER ++ "test_write_csv/H"++ HourStr ++ "/",
  %FFolder = ?CSV_FOLDER ++ "/H"++ HourStr ++ "/", 
  FFolder = ?CSV_FOLDER,
  ?LOGINFO("DEBUG wacs_snapshot RADV: YMDH ~p-~p-~p-~p ~p FF ~p",[YYYYStr, MMStr, DDStr, HHStr, YMDStr, FFolder]),
  DataTemp = evaluate_template( wacs_snapshot_template:template(), TS ), %%TODO. Remove it when ready predict
  %%DataTemp = evaluate_template( wacs_snapshot_template_ras:template(), TS ), %%%% _RAS
  %fp:log(info, "DEBUG wacs_snapshot RADV: evaluate data ~p",[DataTemp]),

%   % Map data to the csv format
  %?LOGINFO("DEBUG wacs_snapshot RADV: map csv templates and write files ~p ", [DataTemp]),
  [ wacs_csv:to_csv( DataTemp, File, FFolder ++ File, snapshot )  || File <- [
       "nodes.csv"
      ,"links.csv"
      ,"nodesReact.csv"
      ,"linksReact.csv"
  ]],
    
  %?LOGINFO("DEBUG wacs_snapshot RADV: map csv templates and write files END "),
  %-------------------Run the calculations---------------------------------
  ?LOGINFO("DEBUG wacs_snapshot RADV: run utility"),
  run_utility(HHStr, YMDStr),
    
  % read OUT.TXT
  %fp:log(info, "DEBUG wacs_snapshot RADV: read   OUT.TXT"),
  OutTxt = read_txt( "out.txt", HHStr ),   
  %fp:log(info, "DEBUG wacs_snapshot RADV: read   OUT.TXT is ~p ", [OutTxt]), 
  fp:set_value("TAGS/WACS_(FP)/radv/trigger_radv","str", unicode:characters_to_binary(io_lib:format("H~p ~p", [Hour, OutTxt])) ),
  
  % read the results
  %fp:log(info, "DEBUG wacs_snapshot RADV: read and parse result out/*.csv"),   
  %% ++ "/H"++ HHStr ++ "/out/"
  FileNodesCSV = ?CSV_FOLDER ++ "/out/" ++ "nodes.csv" ,
  FileLinksCSV = ?CSV_FOLDER ++ "/out/" ++ "links.csv" ,
  FilePredCSV = ?CSV_FOLDER ++ "/out/" ++ "pred.csv" ,
  
  DataNodes =
    try wacs_csv:from_csv("nodes.csv", FileNodesCSV , snapshot ) 
    catch
      _: ErrorNCSV: _-> on_error( ErrorNCSV, 155 ), error
    end,
  
  DataLinks = 
    try
        wacs_csv:from_csv( "links.csv",  FileLinksCSV, snapshot )
    catch
      _: ErrorLCSV: _-> on_error( ErrorLCSV, 156 ), error
    end,
  
  DataPred = 
    try
        wacs_csv:from_csv( "pred.csv",  FilePredCSV, snapshot )
    catch
      _: ErrorPCSV: _-> on_error( ErrorPCSV, 157 ), error
    end,
  
  save_delete_files_utilit(HHStr, YMDStr), 
    
  Errors = 
    case flush_errors() of
        _Errors when is_list(_Errors)->
            [ unicode:characters_to_binary(io_lib:format("~p",[E])) || E <- _Errors ];
        _-> null
    end,
    #{
      <<"nodes">> => maps:get(<<"nodes">>, DataNodes)
     ,<<"links">> => maps:get(<<"links">>, DataLinks)
     ,<<"pred">> =>  DataPred
     ,<<"errors">> => Errors
    }.
  %to_json( DataPred#{ errors => flush_errors() }).



%%--------------------------------------------------------------------------
%%    Read the data from Faceplate sources
%%--------------------------------------------------------------------------
evaluate_template( Level, TS ) when is_map( Level )->
  %fp:log(info,"DEBUG: evaluate_template1 Level ~p ", [Level]),
  maps:map( fun(_Key, Value)-> evaluate_template( Value, TS ) end, Level );
evaluate_template( Leaf, TS ) when is_function( Leaf )->
  %fp:log(info,"DEBUG: evaluate_template2 Leaf ~p ", [Leaf]),  
  try
    Leaf( TS )
  catch
    _:Error ->
      ?LOGINFO("DEBUG: evaluate_template ERROR 179 line Leaf  ~p Err ~p ", [Leaf, Error]), 
      on_error( Error, 179 ),
      none
  end;
evaluate_template( Term, _TS )->
  Term.



%%--------------------------------------------------------------------------
%%    Export to CSV
%%--------------------------------------------------------------------------
% data2csv( List, Data ) when is_list( List )->
%   H = hd( List ),
%   if
%     is_list( H ); is_tuple( H )->
%       [ data2csv( I, Data ) || I <- List ];
%     true->
%       % This is the path
%       Path = parse_path( List ),
%       Value = fp_lib:json_path( Path, Data, undefined ),
%       %fp:log(info, "DEBUG wacs_snapshot RADV: BValue=~p", [Value]),
%       if
%         Value=:=undefined ->
%           %fp:log(info, "DEBUG wacs_snapshot RADV: UValue=~p", [Value]),
%           %on_error({invalid_data_path, Path}),
%           % TODO. What to return???
%           <<"0">>;
%         true ->
%           value2csv( Value )
%       end
%   end;
% data2csv({const, Value}, _Data)->
%   value2csv( Value ).



% value2csv( Value ) when is_integer( Value )->
%   integer_to_binary( Value );
% value2csv( Value ) when is_float( Value )->
%   float_to_binary( Value ,[{decimals, 3}]);
% value2csv( Value ) when is_atom( Value )->
%   atom_to_binary( Value, utf8 );
% value2csv( Value ) when is_list( Value )->
%   % CSV encoding is DOS cyrillic
%   translate( Value );
% value2csv( Value ) when is_binary( Value )->
%   Value.



% write_csv( FileName, Data )->
%   Content =
%     fp_lib:join_binary( [ fp_lib:join_binary( Row, ?COLUMN_DELIMITER ) || Row <- Data ], ?ROW_DELIMITER ),
%   case file:write_file( ?CSV_FOLDER ++"/" ++ FileName, Content, [write] ) of
%     ok -> ok;
%     {error, Error}-> on_error( Error )
%   end.



%%--------------------------------------------------------------------------
%%    Import from CSV
%%--------------------------------------------------------------------------
% read_csv( FileName )->
%   case file:read_file( ?CSV_FOLDER ++ "/" ++ FileName ) of
%     {ok, Content}->
%       %fp:log(info, "DEBUG wacs_snapshot RADV: parse out/*.csv , file ~p , Content ~p ", [FileName, Content]),
%       Data = 
%         [ binary:split( Row, ?COLUMN_DELIMITER, [global] ) || Row <- binary:split( Content, ?ROW_DELIMITER, [global] ) ],
%       % Remove empty rows
%       [ Row || Row <- Data, length(Row) > 1 ];
%     {error, Error}->
%       on_error(Error)
%   end.

%% Read OUT.TXT files after run_utilit - results log of radv write to out.txt
read_txt( FileName, HourStr )->
    %?CSV_FOLDER ++ "/H"++HourStr++"/"
  case file:read_file( ?CSV_FOLDER ++ FileName ) of
    {ok, Content}->
      [ Row || Row <- binary:split( Content, ?ROW_DELIMITER, [global] ) ];
    %   Content1 = unicode:characters_to_list(Content, utf8),
    %   fp:log(info, "DEBUG wacs_snapshot RADV: read TXT file ~p , Content ~p , Content1 ~p ", [FileName, Content, Content1]),
    %   Content1;
    {error, Error}->
      on_error(Error, 263)
  end.

%%--------------------------------------------------------------------------
% %%    Utilities
% %%--------------------------------------------------------------------------
% read_files( [{File, ok}| Rest], Data0 )->
%     Branch = maps:get(File, ?DRTS_BRANCH_MAP),
%     Data = Data0#{ Branch => wacs_csv:from_csv( File, ?DRTS_FOLDER ++ "/" ++ File ) },
%     read_files( Rest, Data );
% read_files( [{File, Error}| Rest], Data )->
%     fp:log(error, "unable to looad drts file ~p, error: ~p",[ File, Error ]),
%     read_files( Rest, Data );
% read_files( [], Data )->
%     Data.



% csv2data([T | TRest ], [V | VRest], Data, Branch )->
%   csv2data( TRest, VRest, csv2data( T, V, Data, Branch ), Branch );
% csv2data([], [], Data, _Branch )->
%   Data;
% csv2data({const, _}, _Value, Data, _Branch )->
%   Data;
% csv2data(Path, Value, Data, Branch )->
%     %TODO
%     %wacs_csv:from_csv( File, ?DRTS_FOLDER ++ "/" ++ File )
%     case parse_path( Path ) of
%         [] -> Data;
%         Path1->
%             Value1 = csv2value( Value ),
%             %fp:log(info,"DEBUG wacs_snapshot RADV: set json value, path ~p, value ~p",[ Path1, Value1 ]),
%             set_json_leaf( Branch ++ Path1, Value1, Data )
%     end.



% csv2value( Value )->
%   try
%     fp_lib:from_json( Value )
%   catch
%     _:_->
%       Value
%   end.



% set_json_leaf([Branch | Rest], Value, Data ) when is_map( Data )->
%   Map = maps:get( Branch, Data, #{}),
%   NewMap =   if is_map(Map) -> Map;
%       true -> #{}
%     end,
%   Data#{ Branch => set_json_leaf( Rest, Value, NewMap ) };
% set_json_leaf([], Value, _Data )->
%   Value.



% to_json( Data )->
%     fp:log(info, "DEBUG wacs_snapshot RADV: Data: ~p ",[Data]),
%     Data1 = fp_db:to_json(term, Data),
%     check_json( Data1 ).



% check_json( Map ) when is_map( Map )->
%     maps:fold(fun(K,V,Acc)->
%         Acc#{ unicode:characters_to_binary( K ) => check_json( V ) }
%     end,#{}, Map);
% check_json( List ) when is_list(List)->
%     [ check_json( I ) || I <- List ];
% check_json( Item )->
%     Item.



%%--------------------------------------------------------------------------
%%    Common utilities
%%--------------------------------------------------------------------------
% parse_path( Path )->
%   [ Name || Name <- string:split(Path, "/",all), length(Name) > 0 ].

read_tag( Tag, Field )->
  % TODO. Default value is 0?
  read_tag( Tag, Field, 0 ).
read_tag( Tag, Field, Default )->
    case fp:get_value( Tag, Field, Default ) of
        {ok, none} -> Default;
        {ok, Value} -> Value;
        {error, Error}->
            ?LOGINFO("DEBUG wacs_snapshot RADV: READ TAG ERROR ~p ~p ", [Tag, Field]),
            on_error( {Tag, Field, Error}, 353 ),
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
          %on_error( {undefined_value_archive, Archive, fp_lib:dt_to_string( TS )}, 369 ),
          Default;
        {_TS, none} ->
          %fp:log(info,"DEBUG wacs_snapshot RADV: ~ts none value at ~p",[ Archive, TS ]),
          %on_error( {none_value_archive, Archive, fp_lib:dt_to_string( TS )}, 373 ),
          Default;
        { _LastTS, Value }->
          %fp:log(info,"DEBUG wacs_snapshot RADV: ~ts value ~p at ~p",[ Archive, Value, _LastTS ]),
          % TODO. REMOVE %% when wacs ready
        %   if
        %     TS - LastTS > ?MAX_DT->
        %       on_error( {outdated_value_archive, Archive, fp_lib:dt_to_string( LastTS) } );
        %     true-> ok
        %   end,
          Value
    end.



%
read_for_util( Tag, Field, TS )->
    %Tag = <<"/root/PROJECT/TAGS/Nodes/EK-1150_F/ntag">>, 	
    %Field = <<"p_gen">>,
    case Field of 
        <<"v_zd">> ->   Set_zd = <<"set_v_zd">>,    Archive = binary:replace(<<Tag/binary>>,<<"/ntag">>, <<"/archivesP/u_n_ras">>); %тут это всегда
        <<"p_load">> -> Set_zd = <<"set_load_zd">>, Archive = binary:replace(<<Tag/binary>>,<<"/ntag">>, <<"/archivesP/p_load_sc">>); %<<"/futureP/predict_p_load_sc">>);
        <<"q_load">> -> Set_zd = <<"set_load_zd">>, Archive = binary:replace(<<Tag/binary>>,<<"/ntag">>, <<"/archivesP/q_load_sc">>); %<<"/futureP/predict_q_load_sc">>);
        <<"p_gen">>  -> Set_zd = <<"set_gen_zd">>,  Archive = binary:replace(<<Tag/binary>>,<<"/ntag">>, <<"/futureP/predict_p_gen">>); %<<"/futureP/predict_p_gen">>);
      %  <<"q_gen">>  -> Set_zd = <<"set_gen_zd">>,  Archive = binary:replace(<<Tag/binary>>,<<"/ntag">>, <<"/archivesP/q_gen_sc">>); %<<"/futureP/predict_q_gen">>);
        _ -> Set_zd = none, Archive = none
    end,

    %for predict maybe...
    % Qfactor = 
    %   case fp:get_value( Tag, <<"q_load_k1">>, 0.01 ) of 
    %     {ok, none} -> 0.01;
    %     {ok, 0} -> 0.01;
    %     {ok, V} -> 
    %         %?LOGINFO("DEBUG: Qfactor-> ~p  Tag ~p",[ V , Tag ]),
    %         V;
    %     ErrorQf ->
    %           ?LOGINFO("DEBUG wacs_snapshot RADV: READ TAG ERROR ~p Err ~p", [Tag, ErrorQf]),
    %             on_error( {Tag, Field, ErrorQf}, 412 ),
    %             0.01
    %   end,
    
    

    %{Set_zd, Archive}.
    %%Default value is 0?
    Default = 0,
    TSch = TS-3600*24*1000,
    Value =
      case Set_zd of 
        <<"set_v_zd">> -> 
          case fp:get_value( Tag, Set_zd, false ) of 
            {ok, true} -> 
                %?LOGINFO("DEBUG: _VS1-> ~p  Tag ~p F ~p ",[ true , Tag, Field ]),
                read_tag(Tag, Field, Default);
            {ok, _VS} -> 
              %?LOGINFO("DEBUG: _VS-> ~p  Tag ~p F ~p ",[ false , Tag, Field ]),
              RtSc= read_archive(Archive, TSch, Default); %read last day archives
              %   read_tag(Tag, <<Field/binary,"_sc">>, Default),
              % ResSc = case Field of
              %           <<"q_load">> -> 
              %               %?LOGINFO("DEBUG: Qfactor-> ~p  Tag ~p F ~p ",[ Qfactor , Tag, Field ]),
              %               RtSc*Qfactor;
              %        _ ->  RtSc
              %       end,
              % ResSc;
            %%read_archive(Archive, TS, Default); %% TODO uncomment when predic done
            
            {error, ErrorR}->
                ?LOGINFO("DEBUG wacs_snapshot RADV: READ TAG ERROR ~p ~p ", [Tag, ErrorR]),
                on_error( {Tag, Field, ErrorR}, 414 ),
                Default
          end;

        <<"set_load_zd">> -> 
          case fp:get_value( Tag, Set_zd, false ) of 
            {ok, true} -> 
                %?LOGINFO("DEBUG: _VS1-> ~p  Tag ~p F ~p ",[ true , Tag, Field ]),
                read_tag(Tag, <<Field/binary,"_zd">>, Default);
            {ok, _VS} -> 
              %?LOGINFO("DEBUG: _VS-> ~p  Tag ~p F ~p ",[ false , Tag, Field ]),
              RtSc= read_archive(Archive, TSch, Default), %read last day archives
              %   read_tag(Tag, <<Field/binary,"_sc">>, Default),
              % ResSc = case Field of
              %           <<"q_load">> -> 
              %               %?LOGINFO("DEBUG: Qfactor-> ~p  Tag ~p F ~p ",[ Qfactor , Tag, Field ]),
              %               RtSc*Qfactor;
              %        _ ->  RtSc
              %       end,              
              ResSc = 
                if ( (Field == <<"p_load">>) and (Tag == <<"/root/PROJECT/TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>) )
                       -> ?LOGINFO("DEBUG: p_load KARGRES_NURA LOAD +480 "), RtSc+480;                   
                  true -> RtSc
                end,    
              ResSc;
            %%read_archive(Archive, TS, Default); %% TODO uncomment when predic done
            
            {error, ErrorR}->
                ?LOGINFO("DEBUG wacs_snapshot RADV: READ TAG ERROR ~p ~p ", [Tag, ErrorR]),
                on_error( {Tag, Field, ErrorR}, 414 ),
                Default
          end;

        <<"set_gen_zd">> ->
           case fp:get_value( Tag, Set_zd, false ) of
            {ok, true} -> 
              RtZd = read_tag(Tag, <<Field/binary,"_zd">>, Default),

              %for 1636 - Sibir_F 
              {{_Y,_M,_D},{Hour,_,_}} = calendar:system_time_to_universal_time(TS,1000),
              if ( (Field == <<"p_gen">>) and (Tag == <<"/root/PROJECT/TAGS/Nodes/Sibir_F/ntag">>) )->
                  %%%and (Hour >=0) and (Hour =<6) ) ->                
                    ArchiveSibirF = binary:replace(<<Tag/binary>>,<<"/ntag">>,<<"/archivesP/p_gen_sc">>),
                    CorrFactor = read_archive(ArchiveSibirF, TSch-3600*24*1000, Default), %archive last day
                    ResZd = CorrFactor+RtZd;
                    %?LOGINFO("DEBUG: Pgen Sibir-> ~p  Hour ~p CorrFactor ~p Res ~p Tag ~p",[ RtZd, Hour, CorrFactor, ResZd, Tag ]);

                true -> ResZd = RtZd
              end,
              ResZd;            
            {ok, _VS} -> read_archive(Archive, TS, Default); %read plan archives - next day             
            {error, ErrorZ}->
                ?LOGINFO("DEBUG wacs_snapshot RADV: READ TAG ERROR ~p ~p ", [Tag, Field]),
                on_error( {Tag, Field, ErrorZ}, 426 ),
                Default
           end;

        ErrorS->
            ?LOGINFO("DEBUG wacs_snapshot RADV: READ TAG ERROR ~p ~p ", [Tag, Field]),
            on_error( {Tag, Field, ErrorS}, 432 ),
            Default    
      end,     
      
    Value.



run_utility(HourStr, YMDStr)->
  %%ResPID = os:getpid(),
  %UTILITY_CMD = ?UTILITY_CMD1 ++ HourStr ++ ?UTILITY_CMD2,
  UTILITY_CMD = ?UTILITY_CMD_DIR ++ " && " ++ ?UTILITY_CMD_RUN,
  ?LOGINFO("DEBUG wacs_snapshot RADV: HourStr ~p , YMDHStr ~p utility CMD ~p ",[ HourStr, YMDStr, UTILITY_CMD ]),
  ResRun = os:cmd( UTILITY_CMD ),
  ?LOGINFO("DEBUG wacs_snapshot RADV: utility result ~p ",[ ResRun ]),
  ok.



save_delete_files_utilit(HourStr, YMDStr)->
  %%ResPID = os:getpid(),
  %UTILITY_CMD = ?UTILITY_CMD1 ++ HourStr ++ " F" ++ YMDStr ++ ?UTILITY_CMD2,
  UTILITY_CMD2 = ?UTILITY_CMD_DIR ++ " && " ++ ?UTILITY_CMD2 ++ "H" ++ HourStr ++ " F" ++ YMDStr, 
  UTILITY_CMD3 = ?UTILITY_CMD_DIR ++ " && " ++ ?UTILITY_CMD3 ++ "H" ++ HourStr ++ " F" ++ YMDStr, 
  ?LOGINFO("DEBUG wacs_snapshot RADV: HourStr ~p ",[ HourStr ]),
  ?LOGINFO("DEBUG wacs_snapshot RADV: CMD2 mv_to_hour ~p ",[ UTILITY_CMD2 ]),
  Res2 = os:cmd( UTILITY_CMD2 ),
  %?LOGINFO("DEBUG wacs_snapshot RADV: ResCMD2 ~p ",[ Res2 ]),
  timer:sleep(1000),
  ?LOGINFO("DEBUG wacs_snapshot RADV: CMD3 clean_main ~p ",[ UTILITY_CMD3 ]),
  Res3 = os:cmd( UTILITY_CMD3 ),
  %?LOGINFO("DEBUG wacs_snapshot RADV: ResCMD3 ~p ",[ Res3 ]),
  ok.


% translate( Key )->
%   maps:get( Key, #{

%     % nodes.csv
%     "node_number" => <<252,32,227,167,171,160>>,
%     "node_state" => <<145,174,225,226,174,239,173,168,165>>,
%     "node_Pn" => <<80,173>>,
%     "node_Qn" => <<81,173>>,
%     "node_Pg" => <<80,163>>,
%     "node_Qg" => <<81,163>>,

%     % links.csv
%     "link_start" => <<141,160,231,46>>,
%     "link_end" => <<138,174,173,46>>,
%     "link_num" => <<252,32,230,165,175,168>>,
%     "link_state" => <<145,174,225,226,174,239,173,168,165>>,
%     "link_Pn" => <<80,173>>,
%     "link_Qn" => <<81,173>>,
%     "link_Pk" => <<80,170>>,
%     "link_Qk" => <<81,170>>,

%     % nodesReact.csv
%     "nr_n_number" => <<78,32,227,167,171,160>>,
%     "nr_r_number" => <<78,32,224,165,160,170,226,174,224,160>>,
%     "nr_conduct" => <<143,224,174,162,174,164,168,172,174,225,226,236>>,
%     "nr_state" => <<145,174,225,226,174,239,173,168,165>>,
%     "nr_weight" => <<130,165,225>>,

%     % linksReact.csv
%     "lr_start" => <<78,112>>,
%     "lr_end" => <<78,113>>,
%     "lr_num" => <<78,32,230,165,175,168>>,
%     "lr_r_num" => <<78,32,224,165,160,170,226,174,224,160>>,
%     "lr_conduct" => <<143,224,174,162,174,164,168,172,174,225,226,236>>,
%     "lr_state" => <<145,174,225,226,174,239,173,168,165>>,
%     "lr_weight" => <<130,165,225>>

%   }, unicode:characters_to_binary(Key) ).



%%--------------------------------------------------------------------------
%%    Error handling utilities
%%--------------------------------------------------------------------------
% on_error( Error )->
%     case Error of
%         {Path,Leaf,{badmatch,{error,{invalid_path,Path}}}}->
%             fp:log(error,"DEBUG wacs_snapshot RADV: on ERROR err ~p",[ Error ]);
%             %fp:log(error,"DEBUG wacs_snapshot RADV: on ERROR, leaf ~p, invalid path ~ts , err ~p",[ Leaf, Path, Error ]);
%         % {Path,Leaf,{invalid_data_path,{error,{invalid_data_path,Path}}}}->
%         %     fp:log(error,"DEBUG wacs_snapshot RADV: on ERROR, leaf ~p, invalid_data_path  ~ts",[ Leaf, Path ]);    
%         % {Path,Leaf,{invalid_data_path,{error,{invalid_data_path,Path}}}}->
%         %     fp:log(error,"DEBUG wacs_snapshot RADV: on ERROR, leaf ~p, invalid_data_path  ~ts",[ Leaf, Path ]); 
%         _->
%             fp:log(error,"DEBUG wacs_snapshot RADV: on ERROR ~p ",[ Error ])
%     end,

%   case get('@errors@') of
%     undefined -> put('@errors@',[ Error ]);
%     EE -> put('@errors@', EE ++ [ Error ])
%   end. %%erlang:error( Error )

% flush_errors()->
%   fp:log(error,"DEBUG wacs_snapshot RADV: FLUSH ERRORs "),
%   erase('@errors@').


%%--------------------------------------------------------------------------
%%    Error handling utilities
%%--------------------------------------------------------------------------
on_error( Error, WhereIs )->
    ?LOGINFO("DEBUG: ERROR wacs_snapshot on error IN-> ~p  Err ~p",[ WhereIs , Error ]),
    case get('@wacs_snapshot_errors@') of
        EE when is_list(EE)->
            put('@wacs_snapshot_errors@', EE ++ [ Error ]);
        _ 
            -> put('@wacs_snapshot_errors@',[ Error ])
    end.

flush_errors()->
  erase('@wacs_snapshot_errors@').
  
  
