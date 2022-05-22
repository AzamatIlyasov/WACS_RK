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

-module(meteologica).

-include("fp_struct.hrl").

-export([on_event/2]).

-export([login/2, get_all_facilitites/1, get_forecast_multi/2]).

-define(LOGIN_BODY,  
"<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:met=\"https://webservice.meteologica.com/api/MeteologicaDataExchangeService.php\">
   <soapenv:Header/>
   <soapenv:Body>
      <met:login>
         <request>
            <!--You may enter the following 2 items in any order-->
            <username>TT_KZ</username>
            <password>g!w4D9EL</password>
         </request>
      </met:login>
   </soapenv:Body>
</soapenv:Envelope>").
   
-define(ALL_FACILITIES,
"<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:met=\"https://webservice.meteologica.com/api/MeteologicaDataExchangeService.php\">
   <soapenv:Header/>
   <soapenv:Body>
      <met:getAllFacilities>
         <request>
            <header>
               <sessionToken>123</sessionToken>
            </header>
         </request>
      </met:getAllFacilities>
   </soapenv:Body>
</soapenv:Envelope>").

-define(FORECAST_MULTI, 
"<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:met=\"https://webservice.meteologica.com/api/MeteologicaDataExchangeService.php\">
   <soapenv:Header/>
   <soapenv:Body>
      <met:getForecastMulti>
         <request>
            <variableId>prod</variableId>
            <predictorId>aggregated</predictorId>
            
            <fromDate>123</fromDate>
            <toDate>123</toDate> 
            <granularity>60</granularity>
            <percentiles>50</percentiles>            
            <facilitiesId>
               <!--1 or more repetitions: 
               <item>TetraTech-fotovoltaica_Agadyr</item> -->
            </facilitiesId>
            <header>
               <sessionToken>123</sessionToken>
            </header>
         </request>
      </met:getForecastMulti>
   </soapenv:Body>
</soapenv:Envelope>").


-define(API_URL, "https://webservice.meteologica.com/api/MeteologicaDataExchangeService.php").
-define(DAY, 86400).

on_event({on_cycle,_Cycle},State)->
   % here should be your code that is executed each Cycle milliseconds
   State;
on_event({tag,_TagID,_Field,_Value},State)->
   % here should be your code that is executed on change of the Field of the Tag
   State.


parse_xml(XML, {tag, Tag}) -> 
    RE = <<"<", Tag/binary, ">(.+?)</", Tag/binary, ">" >>,
    case re:run(XML, RE) of 
        {match, [_, {Offset, Len}] } ->
            lists:sublist(XML, Offset + 1, Len);
        Nomatch ->
            {error, Nomatch}
    end.

get_token(XMLBody) -> 
    case parse_xml(XMLBody, {tag, <<"errorCode">>}) of
        "OK" ->
            NewToken = parse_xml(XMLBody, {tag, <<"sessionToken">>}),
            {ok, list_to_binary(NewToken)};
        Error -> 
            {error, Error}
    end.
    

basic_request(XMLReqBody) -> 
    {ok, Response} = httpc:request(post, {?API_URL, [], "application/xml", XMLReqBody}, [], [{body_format, string}]),
    {StatusLine, _, RespBody} = Response,
    case StatusLine of 
        {_, Code, _} when  Code >= 100, Code =< 300 ->
            case get_token(RespBody) of
                {ok, Token} -> {Token, RespBody};
                {error, Error} ->
                    fp:log(error, "Error occuried while getting token, error ~p", [Error]),
                    {error, Error}
            end;
        _ -> 
            fp:log(warning, "Unexpected response ~p", [Response]),
            {error, {unexpected_response, Response}}
    end.

ts_to_date(Timestamp) ->
    BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds       = BaseDate + Timestamp,
    {{Year1, Month1, Day1}, _} = calendar:gregorian_seconds_to_datetime(Seconds),
    Month = 
        if 
            Month1 < 10 -> <<"0", (integer_to_binary(Month1))/binary>>;
            true -> integer_to_binary(Month1)
        end,
    Day = 
         if 
            Day1 < 10 -> <<"0", (integer_to_binary(Day1))/binary>>;
            true -> integer_to_binary(Day1)
        end,
    Year =  integer_to_binary(Year1),
    <<Year/binary, "-", Month/binary, "-", Day/binary, "T00:00:00">>.
    



%% Forecast == [{timestamp , Value}, ...]
write_forecast_to_archive(_Folder, []) -> 
    ok;
write_forecast_to_archive(Folder, [{FacilityID, Forecast} | Data]) ->
    fp_archives:insert_values(<<Folder/binary, "/", FacilityID/binary>>, Forecast),
    write_forecast_to_archive(Folder, Data).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           API         %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% meteologica:login(<<"TT_KZ">>, <<"g!w4D9EL">>)
% Функция вернет токен, который надо использовать в дальнейшем
login(Login, Password) ->
    Body1 = re:replace(?LOGIN_BODY, <<"<username>(.+?)</username>">>, <<"<username>", Login/binary,"</username>">>, [{return, list}]),
    XMLlogin = re:replace(Body1, <<"<password>(.+?)</password>">>, <<"<password>", Password/binary,"</password>">>, [{return, list}]),
    %fp:log(info, XMLlogin),
    {Token, _ResponseBody} = basic_request(XMLlogin),
    Token.

% {NewToken, FacilityIdList} = meteologica:get_all_facilitites(Token)
% FacilityIdList = [FaciltyName1, FaciltyName2, ...]
% функция возвращает новый токен(либо старый, если он не менялся) и список названий электростанций
get_all_facilitites(SessionToken) ->
    XMLBody = re:replace(?ALL_FACILITIES, <<"<sessionToken>(\\w+)</sessionToken>">>, <<"<sessionToken>",SessionToken/binary,"</sessionToken>">>, [{return, list}]),
    case basic_request(XMLBody) of
        {error, Error} -> 
            fp:log(error, "Error occured during request, error ~p", [Error]),
            {error, Error};
        {NewToken, ResponseBody} -> 
            {match, IdList} = re:run(ResponseBody, <<"<facilityId>(.+?)</facilityId>">>, [global]),
            FacilityIdList = [lists:sublist(ResponseBody, Offset + 1, Len) || [_,  {Offset, Len}]<- IdList],
            {NewToken, FacilityIdList}
    end.

get_forecast(SessionToken, FacilityID) ->
    
    ok.


% meteologica:get_forecast_multi(<<"34rgheg34tgejrkg3h8g433f4hjf3">>, #{<<"fromDate">> => <<"2021-09-21T00:00:00">>, <<"toDate">> => <<"2021-09-22T00:00:00">>})
% Функция вернет новый токен(либо тот же, что и был) и предсказания в формате {Token, Forecast}
get_forecast_multi(SessionToken, UserParams) -> 
    Body1 = re:replace(?FORECAST_MULTI, <<"<sessionToken>(\\w+)</sessionToken>">>, <<"<sessionToken>",SessionToken/binary,"</sessionToken>">>, [{return, list}]),
    Current = erlang:system_time(seconds),
    FromDate = ts_to_date( Current ),
    ToDate = ts_to_date(Current + ?DAY), 
    DefaultParams = #{
        <<"granularity">> => <<"60">>,
        <<"fromDate">> => FromDate,
        <<"toDate">> => ToDate
    },
    Params = maps:merge(DefaultParams, UserParams),
    %fp:log(info, "Params ~p~n", [Params]),
    MakeXML =
        fun(K, V, Acc) ->
            re:replace(Acc, << "<", K/binary, ">(.+?)</", K/binary, ">">>, <<"<", K/binary, ">",V/binary ,"</", K/binary, ">">>, [{return, list}])
        end,
    XMLBody = maps:fold(MakeXML, Body1, Params),
    %fp:log(info, "Body ~p~n", [XMLBody]),
    case basic_request(XMLBody) of
        {error, Error} -> 
            fp:log(error, "Error occured during request, error ~p", [Error]),
            {error, Error};
        {NewToken, ResponseBody} ->
            fp:log(info, "ResponseBody ~p~n", [ResponseBody]),
            {match, IdList} = re:run(ResponseBody, <<"<facilityId>(.+?)</facilityId>">>, [global]),
            {match, ForecastList1} = re:run(ResponseBody, <<"<forecastData>(.+?)</forecastData>">>, [global]),
            FacilityIdList = [lists:sublist(ResponseBody, Offset + 1, Len) || [_,  {Offset, Len}]<- IdList],
            ForecastList = 
                [begin
                    Predict = lists:sublist(ResponseBody, Offset + 2, Len - 1),
                    SplittedPredict =  string:split(Predict, ":", all),
                    [begin
                        [Timestamp, Value] = string:split(TV, "~"),
                        {list_to_integer(Timestamp), list_to_integer(Value)}
                    end || TV <- SplittedPredict]
                end
                || [_, {Offset, Len}] <- ForecastList1],
                
            fp:log(info, "FacilityIDlen ~p PredictLen ~p~n", [ length(FacilityIdList), length(ForecastList) ]),
            {NewToken, lists:zip(FacilityIdList, ForecastList)}
    end.

