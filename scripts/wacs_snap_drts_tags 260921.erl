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

-module(wacs_snap_drts_tags).

-include("fp_struct.hrl").

-export([on_event/2]).

-export([
  config/0
]).


on_event(_Event,State)->
   State.

config() ->
  #{
    %, {<<"TAGS/Branches/Avrora_500_FAT2_F/vtag">>, <<"p_start_ras">>} => [<<"links">>, <<"147_148_2">>, <<"Pn">>]
    %, {<<"TAGS/Branches/Avrora_500_FAT2_F/vtag">>, <<"p_end_ras">>} => [<<"links">>, <<"147_148_2">>, <<"Pk">>]
    %, {<<"TAGS/Branches/Avrora_500_FAT2_F/vtag">>, <<"q_start_ras">>} => [<<"links">>, <<"147_148_2">>, <<"Qn">>]
    %, {<<"TAGS/Branches/Avrora_500_FAT2_F/vtag">>, <<"q_end_ras">>} => [<<"links">>, <<"147_148_2">>, <<"Qk">>]
    
    
%% LINKS begin -----------------------------------------------------------------------------------------------------------------------------	

	%% 
	% , {<<"TAGS/Branches/____________________/vtag">>, <<"______ras">>} => [<<"links">>, <<"__________">>, <<"___">>]
	% , {<<"TAGS/Branches/____________________/vtag">>, <<"______ras">>} => [<<"links">>, <<"__________">>, <<"___">>]
	% , {<<"TAGS/Branches/____________________/vtag">>, <<"______ras">>} => [<<"links">>, <<"__________">>, <<"___">>]
	% , {<<"TAGS/Branches/____________________/vtag">>, <<"______ras">>} => [<<"links">>, <<"__________">>, <<"___">>]    
    % , {<<"TAGS/Branches/___________________/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"_________">>, <<"mPn">>]
	% , {<<"TAGS/Branches/___________________/vtag">>, <<"q_start_m">>} =>  	[<<"links">>, <<"_________">>, <<"mQn">>]
	% , {<<"TAGS/Branches/___________________/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"_________">>, <<"mPk">>]
	% , {<<"TAGS/Branches/___________________/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"_________">>, <<"mQk">>]
	

	%% 26_25_1
	%, 
	  {<<"TAGS/Branches/VL5107_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"26_25_1">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5107_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"26_25_1">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5107_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"26_25_1">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5107_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"26_25_1">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5107_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"26_25_1">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5107_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"26_25_1">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5107_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"26_25_1">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5107_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"26_25_1">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5107_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"26_25_1">>, <<"mQk">>]

	%% 26_25_2
	, {<<"TAGS/Branches/VL5117_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"26_25_2">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5117_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"26_25_2">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5117_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"26_25_2">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5117_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"26_25_2">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5117_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"26_25_2">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5117_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"26_25_2">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5117_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"26_25_2">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5117_F/vtag">>, <<"p_end_m">>} =>    		[<<"links">>, <<"26_25_2">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5117_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"26_25_2">>, <<"mQk">>]
	
	
	%% 25_325_3
	, {<<"TAGS/Branches/VL5120_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"25_325_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5120_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"25_325_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5120_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"25_325_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5120_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"25_325_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5120_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"25_325_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5120_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"25_325_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5120_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"25_325_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5120_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"25_325_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5120_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"25_325_3">>, <<"mQk">>]


	%% 325_469_3
	, {<<"TAGS/Branches/VL5138_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"325_469_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5138_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"325_469_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5138_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"325_469_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5138_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"325_469_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5138_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"325_469_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5138_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"325_469_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5138_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"325_469_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5138_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"325_469_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5138_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"325_469_3">>, <<"mQk">>]


	%% 26_469_3
	, {<<"TAGS/Branches/VL5170_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"26_469_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5170_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"26_469_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5170_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"26_469_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5170_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"26_469_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5170_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"26_469_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5170_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"26_469_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5170_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"26_469_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5170_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"26_469_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5170_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"26_469_3">>, <<"mQk">>]


	%% 469_900_1
	, {<<"TAGS/Branches/VL5300_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"469_900_1">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5300_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"469_900_1">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5300_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"469_900_1">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5300_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"469_900_1">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5300_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"469_900_1">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5300_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"469_900_1">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5300_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"469_900_1">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5300_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"469_900_1">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5300_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"469_900_1">>, <<"mQk">>]


	%% 469_900_2
	, {<<"TAGS/Branches/VL5320_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"469_900_2">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5320_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"469_900_2">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5320_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"469_900_2">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5320_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"469_900_2">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5320_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"469_900_2">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5320_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"469_900_2">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5320_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"469_900_2">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5320_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"469_900_2">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5320_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"469_900_2">>, <<"mQk">>]



	% %% 900_902_3
	, {<<"TAGS/Branches/VL5313_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"900_902_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5313_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"900_902_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5313_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"900_902_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5313_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"900_902_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5313_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"900_902_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5313_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"900_902_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5313_F/vtag">>, <<"q_start_m">>} =>    	[<<"links">>, <<"900_902_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5313_F/vtag">>, <<"p_end_m">>} => 	    [<<"links">>, <<"900_902_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5313_F/vtag">>, <<"q_end_m">>} => 	    [<<"links">>, <<"900_902_3">>, <<"mQk">>]


	%% 900_938_3
	, {<<"TAGS/Branches/VL5363_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"900_938_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5363_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"900_938_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5363_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"900_938_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5363_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"900_938_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5363_F/vtag">>, <<"q_end_ras">>} =>	 	[<<"links">>, <<"900_938_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5363_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"900_938_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5363_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"900_938_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5363_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"900_938_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5363_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"900_938_3">>, <<"mQk">>]


	%% 900_9932_3
	, {<<"TAGS/Branches/VL5333_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"900_9932_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5333_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"900_9932_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5333_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"900_9932_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5333_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"900_9932_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5333_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"900_9932_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5333_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"900_9932_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5333_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"900_9932_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5333_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"900_9932_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5333_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"900_9932_3">>, <<"mQk">>]


	%% 902_9932_3
	, {<<"TAGS/Branches/VL5343_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"902_9932_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5343_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"902_9932_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5343_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"902_9932_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5343_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"902_9932_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5343_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"902_9932_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5343_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"902_9932_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5343_F/vtag">>, <<"q_start_m">>} =>	 	[<<"links">>, <<"902_9932_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5343_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"902_9932_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5343_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"902_9932_3">>, <<"mQk">>]


	%% 902_938_3
	, {<<"TAGS/Branches/VL5353_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"902_938_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5353_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"902_938_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5353_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"902_938_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5353_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"902_938_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5353_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"902_938_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5353_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"902_938_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5353_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"902_938_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5353_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"902_938_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5353_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"902_938_3">>, <<"mQk">>]


	%% 9932_2919_3
	, {<<"TAGS/Branches/VL5143_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"9932_2919_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5143_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"9932_2919_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5143_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"9932_2919_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5143_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"9932_2919_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5143_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"9932_2919_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5143_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"9932_2919_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5143_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"9932_2919_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5143_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"9932_2919_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5143_F/vtag">>, <<"q_end_m">>} => 	    [<<"links">>, <<"9932_2919_3">>, <<"mQk">>]


	%% 2919_800_3
	, {<<"TAGS/Branches/VL5159_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"2919_800_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5159_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"2919_800_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5159_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"2919_800_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5159_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"2919_800_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5159_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"2919_800_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5159_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"2919_800_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5159_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"2919_800_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5159_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"2919_800_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5159_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"2919_800_3">>, <<"mQk">>]


	%% 800_830_3
	, {<<"TAGS/Branches/VL5169_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"800_830_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5169_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"800_830_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5169_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"800_830_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5169_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"800_830_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5169_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"800_830_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5169_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"800_830_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5169_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"800_830_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5169_F/vtag">>, <<"p_end_m">>} => 	    [<<"links">>, <<"800_830_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5169_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"800_830_3">>, <<"mQk">>]


	%% 980_26_3
	, {<<"TAGS/Branches/VL5370_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"980_26_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5370_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"980_26_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5370_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"980_26_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5370_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"980_26_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5370_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"980_26_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5370_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"980_26_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5370_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"980_26_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5370_F/vtag">>, <<"p_end_m">>} =>    		[<<"links">>, <<"980_26_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5370_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"980_26_3">>, <<"mQk">>]


	%% 980_986_3
	, {<<"TAGS/Branches/VL5394_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"980_986_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5394_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"980_986_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5394_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"980_986_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5394_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"980_986_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5394_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"980_986_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5394_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"980_986_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5394_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"980_986_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5394_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"980_986_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5394_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"980_986_3">>, <<"mQk">>]


	%% 986_987_3
	, {<<"TAGS/Branches/VL5400_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"986_987_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5400_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"986_987_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5400_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"986_987_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5400_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"986_987_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5400_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"986_987_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5400_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"986_987_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5400_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"986_987_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5400_F/vtag">>, <<"p_end_m">>} => 	    [<<"links">>, <<"986_987_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5400_F/vtag">>, <<"q_end_m">>} =>    		[<<"links">>, <<"986_987_3">>, <<"mQk">>]
	

	%% 987_938_3
	, {<<"TAGS/Branches/VL5413_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"987_938_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5413_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"987_938_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5413_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"987_938_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5413_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"987_938_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5413_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"987_938_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5413_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"987_938_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5413_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"987_938_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5413_F/vtag">>, <<"p_end_m">>} => 	    [<<"links">>, <<"987_938_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5413_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"987_938_3">>, <<"mQk">>]


	%% 980_240_3
	, {<<"TAGS/Branches/VL5384_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"980_240_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5384_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"980_240_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5384_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"980_240_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5384_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"980_240_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5384_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"980_240_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5384_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"980_240_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5384_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"980_240_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5384_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"980_240_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5384_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"980_240_3">>, <<"mQk">>]


	%% 224_830_3
	, {<<"TAGS/Branches/VL5019_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"224_830_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5019_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"224_830_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5019_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"224_830_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5019_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"224_830_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5019_F/vtag">>, <<"q_end_ras">>} => 	    [<<"links">>, <<"224_830_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5019_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"224_830_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5019_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"224_830_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5019_F/vtag">>, <<"p_end_m">>} => 	    [<<"links">>, <<"224_830_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5019_F/vtag">>, <<"q_end_m">>} => 	    [<<"links">>, <<"224_830_3">>, <<"mQk">>]


	%% 25_129_3
	, {<<"TAGS/Branches/VL5050_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"25_129_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5050_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"25_129_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5050_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"25_129_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5050_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"25_129_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5050_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"25_129_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5050_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"25_129_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5050_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"25_129_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5050_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"25_129_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5050_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"25_129_3">>, <<"mQk">>]


	%% 25_31_3
	, {<<"TAGS/Branches/VL5017_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"25_31_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5017_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"25_31_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5017_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"25_31_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5017_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"25_31_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5017_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"25_31_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5017_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"25_31_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5017_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"25_31_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5017_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"25_31_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5017_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"25_31_3">>, <<"mQk">>]


	%% 31_1621_3
	, {<<"TAGS/Branches/VL5527_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"31_1621_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5527_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"31_1621_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5527_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"31_1621_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5527_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"31_1621_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5527_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"31_1621_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5527_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"31_1621_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5527_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"31_1621_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5527_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"31_1621_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5527_F/vtag">>, <<"q_end_m">>} =>   		[<<"links">>, <<"31_1621_3">>, <<"mQk">>]

	%% 1621_240_3
	, {<<"TAGS/Branches/VL5544_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"1621_240_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5544_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"1621_240_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5544_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"1621_240_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5544_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"1621_240_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5544_F/vtag">>, <<"q_end_ras">>} =>		[<<"links">>, <<"1621_240_3">>, <<"Qk">>]
	, {<<"TAGS/Branches/VL5544_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"1621_240_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5544_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"1621_240_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5544_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"1621_240_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5544_F/vtag">>, <<"q_end_m">>} => 	    [<<"links">>, <<"1621_240_3">>, <<"mQk">>]

	%% 26_1660_3
	, {<<"TAGS/Branches/VL1104_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"26_1660_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL1104_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"26_1660_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL1104_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"26_1660_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL1104_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"26_1660_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL1104_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"26_1660_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL1104_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"26_1660_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL1104_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"26_1660_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL1104_F/vtag">>, <<"p_end_m">>} =>   		[<<"links">>, <<"26_1660_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL1104_F/vtag">>, <<"q_end_m">>} => 	    [<<"links">>, <<"26_1660_3">>, <<"mQk">>]


	%% 240_241_1
	, {<<"TAGS/Branches/Ust-Kam_220_AT1_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"240_241_1">>, <<"state">>]	
	, {<<"TAGS/Branches/Ust-Kam_220_AT1_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"240_241_1">>, <<"Pn">>]
	, {<<"TAGS/Branches/Ust-Kam_220_AT1_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"240_241_1">>, <<"Qn">>]
	, {<<"TAGS/Branches/Ust-Kam_220_AT1_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"240_241_1">>, <<"Pk">>]
	, {<<"TAGS/Branches/Ust-Kam_220_AT1_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"240_241_1">>, <<"Qk">>]
    , {<<"TAGS/Branches/Ust-Kam_220_AT1_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"240_241_1">>, <<"mPn">>]
	, {<<"TAGS/Branches/Ust-Kam_220_AT1_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"240_241_1">>, <<"mQn">>]
	, {<<"TAGS/Branches/Ust-Kam_220_AT1_F/vtag">>, <<"p_end_m">>} =>  		[<<"links">>, <<"240_241_1">>, <<"mPk">>]
	, {<<"TAGS/Branches/Ust-Kam_220_AT1_F/vtag">>, <<"q_end_m">>} => 	    [<<"links">>, <<"240_241_1">>, <<"mQk">>]


	%% 240_241_2
	, {<<"TAGS/Branches/Ust-Kam_220_AT2_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"240_241_2">>, <<"state">>]	
	, {<<"TAGS/Branches/Ust-Kam_220_AT2_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"240_241_2">>, <<"Pn">>]
	, {<<"TAGS/Branches/Ust-Kam_220_AT2_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"240_241_2">>, <<"Qn">>]
	, {<<"TAGS/Branches/Ust-Kam_220_AT2_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"240_241_2">>, <<"Pk">>]
	, {<<"TAGS/Branches/Ust-Kam_220_AT2_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"240_241_2">>, <<"Qk">>]
    , {<<"TAGS/Branches/Ust-Kam_220_AT2_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"240_241_2">>, <<"mPn">>]
	, {<<"TAGS/Branches/Ust-Kam_220_AT2_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"240_241_2">>, <<"mQn">>]
	, {<<"TAGS/Branches/Ust-Kam_220_AT2_F/vtag">>, <<"p_end_m">>} =>  		[<<"links">>, <<"240_241_2">>, <<"mPk">>]
	, {<<"TAGS/Branches/Ust-Kam_220_AT2_F/vtag">>, <<"q_end_m">>} => 	    [<<"links">>, <<"240_241_2">>, <<"mQk">>]


	%% 980_981_3
	, {<<"TAGS/Branches/Semei_220_AT1_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"980_981_3">>, <<"state">>]	
	, {<<"TAGS/Branches/Semei_220_AT1_F/vtag">>, <<"p_start_ras">>} =>	 	[<<"links">>, <<"980_981_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/Semei_220_AT1_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"980_981_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/Semei_220_AT1_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"980_981_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/Semei_220_AT1_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"980_981_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/Semei_220_AT1_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"240_241_2">>, <<"mPn">>]
	, {<<"TAGS/Branches/Semei_220_AT1_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"240_241_2">>, <<"mQn">>]
	, {<<"TAGS/Branches/Semei_220_AT1_F/vtag">>, <<"p_end_m">>} => 	   		[<<"links">>, <<"240_241_2">>, <<"mPk">>]
	, {<<"TAGS/Branches/Semei_220_AT1_F/vtag">>, <<"q_end_m">>} => 	    	[<<"links">>, <<"240_241_2">>, <<"mQk">>]


	%% 31_33_3
	, {<<"TAGS/Branches/AksuGRES_220_AT3_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"31_33_3">>, <<"state">>]	
	, {<<"TAGS/Branches/AksuGRES_220_AT3_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"31_33_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/AksuGRES_220_AT3_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"31_33_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/AksuGRES_220_AT3_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"31_33_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/AksuGRES_220_AT3_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"31_33_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/AksuGRES_220_AT3_F/vtag">>, <<"p_start_m">>} => 	    [<<"links">>, <<"31_33_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/AksuGRES_220_AT3_F/vtag">>, <<"q_start_m">>} => 	    [<<"links">>, <<"31_33_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/AksuGRES_220_AT3_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"31_33_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/AksuGRES_220_AT3_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"31_33_3">>, <<"mQk">>]


	%% 25_39_3
	, {<<"TAGS/Branches/EGRES1_220_AT11_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"25_39_3">>, <<"state">>]	
	, {<<"TAGS/Branches/EGRES1_220_AT11_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"25_39_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/EGRES1_220_AT11_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"25_39_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/EGRES1_220_AT11_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"25_39_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/EGRES1_220_AT11_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"25_39_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/EGRES1_220_AT11_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"25_39_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/EGRES1_220_AT11_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"25_39_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/EGRES1_220_AT11_F/vtag">>, <<"p_end_m">>} =>  		[<<"links">>, <<"25_39_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/EGRES1_220_AT11_F/vtag">>, <<"q_end_m">>} =>  		[<<"links">>, <<"25_39_3">>, <<"mQk">>]


	%% 129_130_1
	, {<<"TAGS/Branches/CGPP_500_FAT3_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"129_130_1">>, <<"state">>]	
	, {<<"TAGS/Branches/CGPP_500_FAT3_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"129_130_1">>, <<"Pn">>]
	, {<<"TAGS/Branches/CGPP_500_FAT3_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"129_130_1">>, <<"Qn">>]
	, {<<"TAGS/Branches/CGPP_500_FAT3_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"129_130_1">>, <<"Pk">>]
	, {<<"TAGS/Branches/CGPP_500_FAT3_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"129_130_1">>, <<"Qk">>]
    , {<<"TAGS/Branches/CGPP_500_FAT3_F/vtag">>, <<"p_start_m">>} => 	    [<<"links">>, <<"129_130_1">>, <<"mPn">>]
	, {<<"TAGS/Branches/CGPP_500_FAT3_F/vtag">>, <<"q_start_m">>} => 	    [<<"links">>, <<"129_130_1">>, <<"mQn">>]
	, {<<"TAGS/Branches/CGPP_500_FAT3_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"129_130_1">>, <<"mPk">>]
	, {<<"TAGS/Branches/CGPP_500_FAT3_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"129_130_1">>, <<"mQk">>]


	%% 129_130_2
	, {<<"TAGS/Branches/CGPP_500_FAT4_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"129_130_2">>, <<"state">>]	
	, {<<"TAGS/Branches/CGPP_500_FAT4_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"129_130_2">>, <<"Pn">>]
	, {<<"TAGS/Branches/CGPP_500_FAT4_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"129_130_2">>, <<"Qn">>]
	, {<<"TAGS/Branches/CGPP_500_FAT4_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"129_130_2">>, <<"Pk">>]
	, {<<"TAGS/Branches/CGPP_500_FAT4_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"129_130_2">>, <<"Qk">>]
    , {<<"TAGS/Branches/CGPP_500_FAT4_F/vtag">>, <<"p_start_m">>} =>  		[<<"links">>, <<"129_130_2">>, <<"mPn">>]
	, {<<"TAGS/Branches/CGPP_500_FAT4_F/vtag">>, <<"q_start_m">>} =>  		[<<"links">>, <<"129_130_2">>, <<"mQn">>]
	, {<<"TAGS/Branches/CGPP_500_FAT4_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"129_130_2">>, <<"mPk">>]
	, {<<"TAGS/Branches/CGPP_500_FAT4_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"129_130_2">>, <<"mQk">>]


	%% 325_310_3
	, {<<"TAGS/Branches/Nura_500_AT-1_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"325_310_3">>, <<"state">>]	
	, {<<"TAGS/Branches/Nura_500_AT-1_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"325_310_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/Nura_500_AT-1_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"325_310_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/Nura_500_AT-1_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"325_310_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/Nura_500_AT-1_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"325_310_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/Nura_500_AT-1_F/vtag">>, <<"p_start_m">>} => 	    [<<"links">>, <<"325_310_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/Nura_500_AT-1_F/vtag">>, <<"q_start_m">>} => 	    [<<"links">>, <<"325_310_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/Nura_500_AT-1_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"325_310_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/Nura_500_AT-1_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"325_310_3">>, <<"mQk">>]


	%% 469_468_3
	, {<<"TAGS/Branches/Agadyr_500_AT1H_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"469_468_3">>, <<"state">>]	
	, {<<"TAGS/Branches/Agadyr_500_AT1H_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"469_468_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/Agadyr_500_AT1H_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"469_468_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/Agadyr_500_AT1H_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"469_468_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/Agadyr_500_AT1H_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"469_468_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/Agadyr_500_AT1H_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"469_468_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/Agadyr_500_AT1H_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"469_468_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/Agadyr_500_AT1H_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"469_468_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/Agadyr_500_AT1H_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"469_468_3">>, <<"mQk">>]
    


	%% 900_901_3
	, {<<"TAGS/Branches/UKGRES_220_AT1_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"900_901_3">>, <<"state">>]	
	, {<<"TAGS/Branches/UKGRES_220_AT1_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"900_901_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/UKGRES_220_AT1_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"900_901_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/UKGRES_220_AT1_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"900_901_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/UKGRES_220_AT1_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"900_901_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/UKGRES_220_AT1_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"900_901_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/UKGRES_220_AT1_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"900_901_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/UKGRES_220_AT1_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"900_901_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/UKGRES_220_AT1_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"900_901_3">>, <<"mQk">>]


	%% 9932_932_3
	, {<<"TAGS/Branches/Shu500_220_FAT3_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"9932_932_3">>, <<"state">>]	
	, {<<"TAGS/Branches/Shu500_220_FAT3_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"9932_932_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/Shu500_220_FAT3_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"9932_932_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/Shu500_220_FAT3_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"9932_932_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/Shu500_220_FAT3_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"9932_932_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/Shu500_220_FAT3_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"9932_932_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/Shu500_220_FAT3_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"9932_932_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/Shu500_220_FAT3_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"9932_932_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/Shu500_220_FAT3_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"9932_932_3">>, <<"mQk">>]


	%% 932_1916_3
	, {<<"TAGS/Branches/VL2163_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"932_1916_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2163_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"932_1916_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2163_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"932_1916_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2163_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"932_1916_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2163_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"932_1916_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2163_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"932_1916_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2163_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"932_1916_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2163_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"932_1916_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2163_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"932_1916_3">>, <<"mQk">>]


	%% 1916_903_3
	, {<<"TAGS/Branches/VL2193_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"1916_903_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2193_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"1916_903_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2193_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"1916_903_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2193_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"1916_903_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2193_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"1916_903_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2193_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"1916_903_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2193_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"1916_903_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2193_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"1916_903_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2193_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"1916_903_3">>, <<"mQk">>]


	%% 902_903_1
	, {<<"TAGS/Branches/Almaty_220_AT-1_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"902_903_1">>, <<"state">>]	
	, {<<"TAGS/Branches/Almaty_220_AT-1_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"902_903_1">>, <<"Pn">>]
	, {<<"TAGS/Branches/Almaty_220_AT-1_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"902_903_1">>, <<"Qn">>]
	, {<<"TAGS/Branches/Almaty_220_AT-1_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"902_903_1">>, <<"Pk">>]
	, {<<"TAGS/Branches/Almaty_220_AT-1_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"902_903_1">>, <<"Qk">>]
    , {<<"TAGS/Branches/Almaty_220_AT-1_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"902_903_1">>, <<"mPn">>]
	, {<<"TAGS/Branches/Almaty_220_AT-1_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"902_903_1">>, <<"mQn">>]
	, {<<"TAGS/Branches/Almaty_220_AT-1_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"902_903_1">>, <<"mPk">>]
	, {<<"TAGS/Branches/Almaty_220_AT-1_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"902_903_1">>, <<"mQk">>]


	%% 902_903_2
	, {<<"TAGS/Branches/Almaty_220_AT-2_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"902_903_2">>, <<"state">>]	
	, {<<"TAGS/Branches/Almaty_220_AT-2_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"902_903_2">>, <<"Pn">>]
	, {<<"TAGS/Branches/Almaty_220_AT-2_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"902_903_2">>, <<"Qn">>]
	, {<<"TAGS/Branches/Almaty_220_AT-2_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"902_903_2">>, <<"Pk">>]
	, {<<"TAGS/Branches/Almaty_220_AT-2_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"902_903_2">>, <<"Qk">>]
    , {<<"TAGS/Branches/Almaty_220_AT-2_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"902_903_2">>, <<"mPn">>]
	, {<<"TAGS/Branches/Almaty_220_AT-2_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"902_903_2">>, <<"mQn">>]
	, {<<"TAGS/Branches/Almaty_220_AT-2_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"902_903_2">>, <<"mPk">>]
	, {<<"TAGS/Branches/Almaty_220_AT-2_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"902_903_2">>, <<"mQk">>]


	%% 938_939_1
	, {<<"TAGS/Branches/Alma_220_AT-1_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"938_939_1">>, <<"state">>]	
	, {<<"TAGS/Branches/Alma_220_AT-1_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"938_939_1">>, <<"Pn">>]
	, {<<"TAGS/Branches/Alma_220_AT-1_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"938_939_1">>, <<"Qn">>]
	, {<<"TAGS/Branches/Alma_220_AT-1_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"938_939_1">>, <<"Pk">>]
	, {<<"TAGS/Branches/Alma_220_AT-1_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"938_939_1">>, <<"Qk">>]
    , {<<"TAGS/Branches/Alma_220_AT-1_F/vtag">>, <<"p_start_m">>} =>  		[<<"links">>, <<"938_939_1">>, <<"mPn">>]
	, {<<"TAGS/Branches/Alma_220_AT-1_F/vtag">>, <<"q_start_m">>} => 	    [<<"links">>, <<"938_939_1">>, <<"mQn">>]
	, {<<"TAGS/Branches/Alma_220_AT-1_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"938_939_1">>, <<"mPk">>]
	, {<<"TAGS/Branches/Alma_220_AT-1_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"938_939_1">>, <<"mQk">>]

	%% 938_939_2
	, {<<"TAGS/Branches/Alma_220_AT-2_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"938_939_2">>, <<"state">>]	
	, {<<"TAGS/Branches/Alma_220_AT-2_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"938_939_2">>, <<"Pn">>]
	, {<<"TAGS/Branches/Alma_220_AT-2_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"938_939_2">>, <<"Qn">>]
	, {<<"TAGS/Branches/Alma_220_AT-2_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"938_939_2">>, <<"Pk">>]
	, {<<"TAGS/Branches/Alma_220_AT-2_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"938_939_2">>, <<"Qk">>]
    , {<<"TAGS/Branches/Alma_220_AT-2_F/vtag">>, <<"p_start_m">>} =>   		[<<"links">>, <<"938_939_2">>, <<"mPn">>]
	, {<<"TAGS/Branches/Alma_220_AT-2_F/vtag">>, <<"q_start_m">>} =>   		[<<"links">>, <<"938_939_2">>, <<"mQn">>]
	, {<<"TAGS/Branches/Alma_220_AT-2_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"938_939_2">>, <<"mPk">>]
	, {<<"TAGS/Branches/Alma_220_AT-2_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"938_939_2">>, <<"mQk">>]


	%% 987_988_3
	, {<<"TAGS/Branches/TaldK500_220_AT-1_F/vtag">>, <<"state_b_ras">>} => 	    	[<<"links">>, <<"987_988_3">>, <<"state">>]	
	, {<<"TAGS/Branches/TaldK500_220_AT-1_F/vtag">>, <<"p_start_ras">>} => 	    	[<<"links">>, <<"987_988_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/TaldK500_220_AT-1_F/vtag">>, <<"q_start_ras">>} => 	    	[<<"links">>, <<"987_988_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/TaldK500_220_AT-1_F/vtag">>, <<"p_end_ras">>} => 		    [<<"links">>, <<"987_988_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/TaldK500_220_AT-1_F/vtag">>, <<"q_end_ras">>} => 	    	[<<"links">>, <<"987_988_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/TaldK500_220_AT-1_F/vtag">>, <<"p_start_m">>} =>       		[<<"links">>, <<"987_988_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/TaldK500_220_AT-1_F/vtag">>, <<"q_start_m">>} =>      		[<<"links">>, <<"987_988_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/TaldK500_220_AT-1_F/vtag">>, <<"p_end_m">>} => 				[<<"links">>, <<"987_988_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/TaldK500_220_AT-1_F/vtag">>, <<"q_end_m">>} => 				[<<"links">>, <<"987_988_3">>, <<"mQk">>]


	%% 918_2952_3
	, {<<"TAGS/Branches/VL2183_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"918_2952_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2183_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"918_2952_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2183_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"918_2952_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2183_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"918_2952_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2183_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"918_2952_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2183_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"918_2952_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2183_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"918_2952_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2183_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"918_2952_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2183_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"918_2952_3">>, <<"mQk">>]


	%% 800_801_3
	, {<<"TAGS/Branches/Zambul_220_AT-1_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"800_801_3">>, <<"state">>]	
	, {<<"TAGS/Branches/Zambul_220_AT-1_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"800_801_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/Zambul_220_AT-1_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"800_801_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/Zambul_220_AT-1_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"800_801_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/Zambul_220_AT-1_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"800_801_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/Zambul_220_AT-1_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"800_801_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/Zambul_220_AT-1_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"800_801_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/Zambul_220_AT-1_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"800_801_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/Zambul_220_AT-1_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"800_801_3">>, <<"mQk">>]
	

	%% 2919_2918_1
	, {<<"TAGS/Branches/Frunze500_200_AT1_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"2919_2918_1">>, <<"state">>]	
	, {<<"TAGS/Branches/Frunze500_200_AT1_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"2919_2918_1">>, <<"Pn">>]
	, {<<"TAGS/Branches/Frunze500_200_AT1_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"2919_2918_1">>, <<"Qn">>]
	, {<<"TAGS/Branches/Frunze500_200_AT1_F/vtag">>, <<"p_end_ras">>} => 	[<<"links">>, <<"2919_2918_1">>, <<"Pk">>]
	, {<<"TAGS/Branches/Frunze500_200_AT1_F/vtag">>, <<"q_end_ras">>} => 	[<<"links">>, <<"2919_2918_1">>, <<"Qk">>]
    , {<<"TAGS/Branches/Frunze500_200_AT1_F/vtag">>, <<"p_start_m">>} => 	[<<"links">>, <<"2919_2918_1">>, <<"mPn">>]
	, {<<"TAGS/Branches/Frunze500_200_AT1_F/vtag">>, <<"q_start_m">>} => 	[<<"links">>, <<"2919_2918_1">>, <<"mQn">>]
	, {<<"TAGS/Branches/Frunze500_200_AT1_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"2919_2918_1">>, <<"mPk">>]
	, {<<"TAGS/Branches/Frunze500_200_AT1_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"2919_2918_1">>, <<"mQk">>]


	%% 2919_2918_2 
	, {<<"TAGS/Branches/Frunze500_200_AT2_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"2919_2918_2">>, <<"state">>]	
	, {<<"TAGS/Branches/Frunze500_200_AT2_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"2919_2918_2">>, <<"Pn">>]
	, {<<"TAGS/Branches/Frunze500_200_AT2_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"2919_2918_2">>, <<"Qn">>]
	, {<<"TAGS/Branches/Frunze500_200_AT2_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"2919_2918_2">>, <<"Pk">>]
	, {<<"TAGS/Branches/Frunze500_200_AT2_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"2919_2918_2">>, <<"Qk">>]
    , {<<"TAGS/Branches/Frunze500_200_AT2_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"2919_2918_2">>, <<"mPn">>]
	, {<<"TAGS/Branches/Frunze500_200_AT2_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"2919_2918_2">>, <<"mQn">>]
	, {<<"TAGS/Branches/Frunze500_200_AT2_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"2919_2918_2">>, <<"mPk">>]
	, {<<"TAGS/Branches/Frunze500_200_AT2_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"2919_2918_2">>, <<"mQk">>]


	%% 224_2925_3 
	, {<<"TAGS/Branches/TashTES_AT_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"224_2925_3">>, <<"state">>]	
	, {<<"TAGS/Branches/TashTES_AT_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"224_2925_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/TashTES_AT_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"224_2925_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/TashTES_AT_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"224_2925_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/TashTES_AT_F/vtag">>, <<"q_end_ras">>} => 	 	[<<"links">>, <<"224_2925_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/TashTES_AT_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"224_2925_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/TashTES_AT_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"224_2925_3">>, <<"mQn">>] 
	, {<<"TAGS/Branches/TashTES_AT_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"224_2925_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/TashTES_AT_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"224_2925_3">>, <<"mQk">>]


	%% 830_831_1
	, {<<"TAGS/Branches/Shym500_220_AT1_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"830_831_1">>, <<"state">>]	
	, {<<"TAGS/Branches/Shym500_220_AT1_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"830_831_1">>, <<"Pn">>]
	, {<<"TAGS/Branches/Shym500_220_AT1_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"830_831_1">>, <<"Qn">>]
	, {<<"TAGS/Branches/Shym500_220_AT1_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"830_831_1">>, <<"Pk">>]
	, {<<"TAGS/Branches/Shym500_220_AT1_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"830_831_1">>, <<"Qk">>]
    , {<<"TAGS/Branches/Shym500_220_AT1_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"830_831_1">>, <<"mPn">>]
	, {<<"TAGS/Branches/Shym500_220_AT1_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"830_831_1">>, <<"mQn">>]
	, {<<"TAGS/Branches/Shym500_220_AT1_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"830_831_1">>, <<"mPk">>]
	, {<<"TAGS/Branches/Shym500_220_AT1_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"830_831_1">>, <<"mQk">>]


	%% 830_831_2
	, {<<"TAGS/Branches/Shym500_220_AT3_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"830_831_2">>, <<"state">>]	
	, {<<"TAGS/Branches/Shym500_220_AT3_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"830_831_2">>, <<"Pn">>]
	, {<<"TAGS/Branches/Shym500_220_AT3_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"830_831_2">>, <<"Qn">>]
	, {<<"TAGS/Branches/Shym500_220_AT3_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"830_831_2">>, <<"Pk">>]
	, {<<"TAGS/Branches/Shym500_220_AT3_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"830_831_2">>, <<"Qk">>]
    , {<<"TAGS/Branches/Shym500_220_AT3_F/vtag">>, <<"p_start_m">>} =>	 	[<<"links">>, <<"830_831_2">>, <<"mPn">>]
	, {<<"TAGS/Branches/Shym500_220_AT3_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"830_831_2">>, <<"mQn">>]
	, {<<"TAGS/Branches/Shym500_220_AT3_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"830_831_2">>, <<"mPk">>]
	, {<<"TAGS/Branches/Shym500_220_AT3_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"830_831_2">>, <<"mQk">>]


	%% 1660_1630_1
	, {<<"TAGS/Branches/VL595_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"1660_1630_1">>, <<"state">>]	
	, {<<"TAGS/Branches/VL595_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"1660_1630_1">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL595_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"1660_1630_1">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL595_F/vtag">>, <<"p_end_ras">>} =>  		[<<"links">>, <<"1660_1630_1">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL595_F/vtag">>, <<"q_end_ras">>} => 	    [<<"links">>, <<"1660_1630_1">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL595_F/vtag">>, <<"p_start_m">>} =>  		[<<"links">>, <<"1660_1630_1">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL595_F/vtag">>, <<"q_start_m">>} =>  		[<<"links">>, <<"1660_1630_1">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL595_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"1660_1630_1">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL595_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"1660_1630_1">>, <<"mQk">>]


	%% 1630_1621_3
	, {<<"TAGS/Branches/VL551_F/vtag">>, <<"state_b_ras">>} =>  	[<<"links">>, <<"1630_1621_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL551_F/vtag">>, <<"p_start_ras">>} => 	    [<<"links">>, <<"1630_1621_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL551_F/vtag">>, <<"q_start_ras">>} =>  	[<<"links">>, <<"1630_1621_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL551_F/vtag">>, <<"p_end_ras">>} =>	 	[<<"links">>, <<"1630_1621_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL551_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"1630_1621_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL551_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"1630_1621_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL551_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"1630_1621_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL551_F/vtag">>, <<"p_end_m">>} => 		    [<<"links">>, <<"1630_1621_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL551_F/vtag">>, <<"q_end_m">>} => 		    [<<"links">>, <<"1630_1621_3">>, <<"mQk">>]


	%% 26_175_3
	, {<<"TAGS/Branches/VL1101_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"26_175_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL1101_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"26_175_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL1101_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"26_175_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL1101_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"26_175_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL1101_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"26_175_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL1101_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"26_175_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL1101_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"26_175_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL1101_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"26_175_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL1101_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"26_175_3">>, <<"mQk">>]


	%% 175_590_3
	, {<<"TAGS/Branches/VL1102_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"175_590_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL1102_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"175_590_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL1102_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"175_590_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL1102_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"175_590_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL1102_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"175_590_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL1102_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"175_590_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL1102_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"175_590_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL1102_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"175_590_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL1102_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"175_590_3">>, <<"mQk">>]


	%% 590_576_3
	, {<<"TAGS/Branches/VL5096_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"590_576_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5096_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"590_576_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5096_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"590_576_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5096_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"590_576_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5096_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"590_576_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5096_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"590_576_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5096_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"590_576_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5096_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"590_576_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5096_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"590_576_3">>, <<"mQk">>]


	%% 129_180_3
	, {<<"TAGS/Branches/VL5071_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"129_180_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5071_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"129_180_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5071_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"129_180_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5071_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"129_180_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5071_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"129_180_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5071_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"129_180_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5071_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"129_180_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5071_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"129_180_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5071_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"129_180_3">>, <<"mQk">>]


	%% 180_576_3
	, {<<"TAGS/Branches/VL5086_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"180_576_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5086_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"180_576_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5086_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"180_576_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5086_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"180_576_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5086_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"180_576_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5086_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"180_576_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5086_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"180_576_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5086_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"180_576_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5086_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"180_576_3">>, <<"mQk">>]


	%% 576_577_3
	, {<<"TAGS/Branches/VL5726_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"576_577_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5726_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"576_577_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5726_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"576_577_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5726_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"576_577_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5726_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"576_577_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5726_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"576_577_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5726_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"576_577_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5726_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"576_577_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5726_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"576_577_3">>, <<"mQk">>]


	%% 1660_1631_3 
	, {<<"TAGS/Branches/VL_ekv_altai_sibir_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"1660_1631_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_altai_sibir_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"1660_1631_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_altai_sibir_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"1660_1631_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_altai_sibir_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"1660_1631_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_altai_sibir_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"1660_1631_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_altai_sibir_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"1660_1631_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_altai_sibir_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"1660_1631_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_altai_sibir_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"1660_1631_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_altai_sibir_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"1660_1631_3">>, <<"mQk">>]

	%% 469_480_3
	, {<<"TAGS/Branches/VL5148_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"469_480_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5148_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"469_480_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5148_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"469_480_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5148_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"469_480_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5148_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"469_480_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5148_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"469_480_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5148_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"469_480_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5148_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"469_480_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5148_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"469_480_3">>, <<"mQk">>]


	% %% 1660_1630_2 
	, {<<"TAGS/Branches/VL596_F/vtag">>, <<"state_b_ras">>} =>	 	[<<"links">>, <<"1660_1630_2">>, <<"state">>]	
	, {<<"TAGS/Branches/VL596_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"1660_1630_2">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL596_F/vtag">>, <<"q_start_ras">>} => 	    [<<"links">>, <<"1660_1630_2">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL596_F/vtag">>, <<"p_end_ras">>} =>    	[<<"links">>, <<"1660_1630_2">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL596_F/vtag">>, <<"q_end_ras">>} =>  		[<<"links">>, <<"1660_1630_2">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL596_F/vtag">>, <<"p_start_m">>} => 	    [<<"links">>, <<"1660_1630_2">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL596_F/vtag">>, <<"q_start_m">>} =>  	 	[<<"links">>, <<"1660_1630_2">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL596_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"1660_1630_2">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL596_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"1660_1630_2">>, <<"mQk">>]


	%% 28_60_3   
	, {<<"TAGS/Branches/EGRES-2_500_20_T_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"28_60_3">>, <<"state">>]	
	, {<<"TAGS/Branches/EGRES-2_500_20_T_F/vtag">>, <<"p_start_ras">>} =>	 	[<<"links">>, <<"28_60_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/EGRES-2_500_20_T_F/vtag">>, <<"q_start_ras">>} =>	 	[<<"links">>, <<"28_60_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/EGRES-2_500_20_T_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"28_60_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/EGRES-2_500_20_T_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"28_60_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/EGRES-2_500_20_T_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"28_60_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/EGRES-2_500_20_T_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"28_60_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/EGRES-2_500_20_T_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"28_60_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/EGRES-2_500_20_T_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"28_60_3">>, <<"mQk">>]


	%% 25_51_3 
	, {<<"TAGS/Branches/EGRES-1_500_T3-8_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"25_51_3">>, <<"state">>]	
	, {<<"TAGS/Branches/EGRES-1_500_T3-8_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"25_51_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/EGRES-1_500_T3-8_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"25_51_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/EGRES-1_500_T3-8_F/vtag">>, <<"p_end_ras">>} =>   	 	[<<"links">>, <<"25_51_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/EGRES-1_500_T3-8/vtag">>, <<"q_end_ras">>} => 	  		[<<"links">>, <<"25_51_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/EGRES-1_500_T3-8_F/vtag">>, <<"p_start_m">>} => 	    [<<"links">>, <<"25_51_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/EGRES-1_500_T3-8_F/vtag">>, <<"q_start_m">>} => 	    [<<"links">>, <<"25_51_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/EGRES-1_500_T3-8_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"25_51_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/EGRES-1_500_T3-8_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"25_51_3">>, <<"mQk">>]


	%% 28_26_1 
	, {<<"TAGS/Branches/VL5817_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"28_26_1">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5817_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"28_26_1">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5817_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"28_26_1">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5817_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"28_26_1">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5817_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"28_26_1">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5817_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"28_26_1">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5817_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"28_26_1">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5817_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"28_26_1">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5817_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"28_26_1">>, <<"mQk">>]


	%% 28_26_2 			
	, {<<"TAGS/Branches/VL5827_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"28_26_2">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5827_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"28_26_2">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5827_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"28_26_2">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5827_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"28_26_2">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5827_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"28_26_2">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5827_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"28_26_2">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5827_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"28_26_2">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5827_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"28_26_2">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5827_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"28_26_2">>, <<"mQk">>]


	%% 480_466_3
	, {<<"TAGS/Branches/Zhezkaz_500_AT1H_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"480_466_3">>, <<"state">>]	
	, {<<"TAGS/Branches/Zhezkaz_500_AT1H_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"480_466_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/Zhezkaz_500_AT1H_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"480_466_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/Zhezkaz_500_AT1H_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"480_466_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/Zhezkaz_500_AT1H_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"480_466_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/Zhezkaz_500_AT1H_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"480_466_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/Zhezkaz_500_AT1H_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"480_466_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/Zhezkaz_500_AT1H_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"480_466_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/Zhezkaz_500_AT1H_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"480_466_3">>, <<"mQk">>]


	%% 904_939_3
	, {<<"TAGS/Branches/VL_ekv_ATETS-3-Alma220_F/vtag">>, <<"state_b_ras">>} => 			[<<"links">>, <<"904_939_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_ATETS-3-Alma220_F/vtag">>, <<"p_start_ras">>} => 			[<<"links">>, <<"904_939_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_ATETS-3-Alma220_F/vtag">>, <<"q_start_ras">>} =>         	[<<"links">>, <<"904_939_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_ATETS-3-Alma220_F/vtag">>, <<"p_end_ras">>} => 		        [<<"links">>, <<"904_939_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_ATETS-3-Alma220_F/vtag">>, <<"q_end_ras">>} => 	        	[<<"links">>, <<"904_939_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_ATETS-3-Alma220_F/vtag">>, <<"p_start_m">>} => 			    [<<"links">>, <<"904_939_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_ATETS-3-Alma220_F/vtag">>, <<"q_start_m">>} => 		     	[<<"links">>, <<"904_939_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_ATETS-3-Alma220_F/vtag">>, <<"p_end_m">>} => 				[<<"links">>, <<"904_939_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_ATETS-3-Alma220_F/vtag">>, <<"q_end_m">>} => 				[<<"links">>, <<"904_939_3">>, <<"mQk">>]


	%% 912_953_3
	, {<<"TAGS/Branches/VL_ekv_ps7_ahbk-koyan_kos_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"912_953_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_ps7_ahbk-koyan_kos_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"912_953_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_ps7_ahbk-koyan_kos_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"912_953_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_ps7_ahbk-koyan_kos_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"912_953_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_ps7_ahbk-koyan_kos_F/vtag">>, <<"q_end_ras">>} =>   	[<<"links">>, <<"912_953_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_ps7_ahbk-koyan_kos_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"912_953_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_ps7_ahbk-koyan_kos_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"912_953_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_ps7_ahbk-koyan_kos_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"912_953_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_ps7_ahbk-koyan_kos_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"912_953_3">>, <<"mQk">>]


	%% 918_912_3
	, {<<"TAGS/Branches/VL2173_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"918_912_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2173_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"918_912_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2173_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"918_912_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2173_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"918_912_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2173_F/vtag">>, <<"q_end_ras">>} =>   	[<<"links">>, <<"918_912_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2173_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"918_912_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2173_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"918_912_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2173_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"918_912_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2173_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"918_912_3">>, <<"mQk">>]


	%% 939_907_3
	, {<<"TAGS/Branches/VL2463_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"939_907_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2463_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"939_907_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2463_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"939_907_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2463_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"939_907_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2463_F/vtag">>, <<"q_end_ras">>} =>   	[<<"links">>, <<"939_907_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2463_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"939_907_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2463_F/vtag">>, <<"q_start_m">>} => 	    [<<"links">>, <<"939_907_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2463_F/vtag">>, <<"p_end_m">>} => 	   	[<<"links">>, <<"939_907_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2463_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"939_907_3">>, <<"mQk">>]


	%% 903_904_3
	, {<<"TAGS/Branches/VL2013_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"903_904_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2013_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"903_904_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2013_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"903_904_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2013_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"903_904_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2013_F/vtag">>, <<"q_end_ras">>} =>    	[<<"links">>, <<"903_904_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2013_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"903_904_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2013_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"903_904_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2013_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"903_904_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2013_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"903_904_3">>, <<"mQk">>]


	%% 468_473_3
	, {<<"TAGS/Branches/VL2208_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"468_473_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2208_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"468_473_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2208_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"468_473_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2208_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"468_473_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2208_F/vtag">>, <<"q_end_ras">>} =>   	[<<"links">>, <<"468_473_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2208_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"468_473_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2208_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"468_473_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2208_F/vtag">>, <<"p_end_m">>} => 	   	[<<"links">>, <<"468_473_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2208_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"468_473_3">>, <<"mQk">>]


	%% 473_475_3
	, {<<"TAGS/Branches/VL2448_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"473_475_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2448_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"473_475_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2448_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"473_475_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2448_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"473_475_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2448_F/vtag">>, <<"q_end_ras">>} => 	    [<<"links">>, <<"473_475_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2448_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"473_475_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2448_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"473_475_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2448_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"473_475_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2448_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"473_475_3">>, <<"mQk">>]


	%% 475_468_3
	, {<<"TAGS/Branches/VL2438_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"475_468_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2438_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"475_468_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2438_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"475_468_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2438_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"475_468_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2438_F/vtag">>, <<"q_end_ras">>} =>   	[<<"links">>, <<"475_468_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2438_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"475_468_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2438_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"475_468_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2438_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"475_468_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2438_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"475_468_3">>, <<"mQk">>]
	

	%% 39_50_3 EGRES-1_T1-2_F   
	, {<<"TAGS/Branches/EGRES-1_T1-2_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"39_50_3">>, <<"state">>]	
	, {<<"TAGS/Branches/EGRES-1_T1-2_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"39_50_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/EGRES-1_T1-2_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"39_50_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/EGRES-1_T1-2_F/vtag">>, <<"p_end_ras">>} => 	[<<"links">>, <<"39_50_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/EGRES-1_T1-2_F/vtag">>, <<"q_end_ras">>} => 	[<<"links">>, <<"39_50_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/EGRES-1_T1-2_F/vtag">>, <<"p_start_m">>} => 	[<<"links">>, <<"39_50_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/EGRES-1_T1-2_F/vtag">>, <<"q_start_m">>} => 	[<<"links">>, <<"39_50_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/EGRES-1_T1-2_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"39_50_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/EGRES-1_T1-2_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"39_50_3">>, <<"mQk">>]


	%% 903_907_3%% udalen
	, {<<"TAGS/Branches/VL2053_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"903_907_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2053_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"903_907_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2053_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"903_907_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2053_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"903_907_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2053_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"903_907_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2053_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"903_907_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2053_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"903_907_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2053_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"903_907_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2053_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"903_907_3">>, <<"mQk">>]
	

	%% 224_63_3
	, {<<"TAGS/Branches/VL522_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"224_63_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL522_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"224_63_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL522_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"224_63_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL522_F/vtag">>, <<"p_end_ras">>} => 	[<<"links">>, <<"224_63_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL522_F/vtag">>, <<"q_end_ras">>} => 	[<<"links">>, <<"224_63_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL522_F/vtag">>, <<"p_start_m">>} => 	[<<"links">>, <<"224_63_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL522_F/vtag">>, <<"q_start_m">>} => 	[<<"links">>, <<"224_63_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL522_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"224_63_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL522_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"224_63_3">>, <<"mQk">>]
	

	%% 175_147_3
	, {<<"TAGS/Branches/VL5191_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"175_147_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5191_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"175_147_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5191_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"175_147_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5191_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"175_147_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5191_F/vtag">>, <<"q_end_ras">>} =>   	[<<"links">>, <<"175_147_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5191_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"175_147_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5191_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"175_147_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5191_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"175_147_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5191_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"175_147_3">>, <<"mQk">>]
	

	%% 147_1817_3
	, {<<"TAGS/Branches/VL5561_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"147_1817_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5561_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"147_1817_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5561_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"147_1817_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5561_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"147_1817_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5561_F/vtag">>, <<"q_end_ras">>} =>   	[<<"links">>, <<"147_1817_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5561_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"147_1817_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5561_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"147_1817_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5561_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"147_1817_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5561_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"147_1817_3">>, <<"mQk">>]
	

	%% 25_1817_3
	, {<<"TAGS/Branches/VL5577_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"25_1817_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5577_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"25_1817_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5577_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"25_1817_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5577_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"25_1817_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5577_F/vtag">>, <<"q_end_ras">>} =>   	[<<"links">>, <<"25_1817_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5577_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"25_1817_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5577_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"25_1817_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5577_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"25_1817_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5577_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"25_1817_3">>, <<"mQk">>]
	

	%% 31_1850_3
	, {<<"TAGS/Branches/VL5537_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"31_1850_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL5537_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"31_1850_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL5537_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"31_1850_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL5537_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"31_1850_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL5537_F/vtag">>, <<"q_end_ras">>} =>   	[<<"links">>, <<"31_1850_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL5537_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"31_1850_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL5537_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"31_1850_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL5537_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"31_1850_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL5537_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"31_1850_3">>, <<"mQk">>]
	

	%% 1817_1850_3
	, {<<"TAGS/Branches/VL555_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"1817_1850_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL555_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"1817_1850_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL555_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"1817_1850_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL555_F/vtag">>, <<"p_end_ras">>} => 	[<<"links">>, <<"1817_1850_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL555_F/vtag">>, <<"q_end_ras">>} => 	[<<"links">>, <<"1817_1850_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL555_F/vtag">>, <<"p_start_m">>} => 	[<<"links">>, <<"1817_1850_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL555_F/vtag">>, <<"q_start_m">>} => 	[<<"links">>, <<"1817_1850_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL555_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"1817_1850_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL555_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"1817_1850_3">>, <<"mQk">>]
	

	%% 31_30_3  
	, {<<"TAGS/Branches/EEC_500_blok_5-8_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"31_30_3">>, <<"state">>]	
	, {<<"TAGS/Branches/EEC_500_blok_5-8_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"31_30_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/EEC_500_blok_5-8_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"31_30_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/EEC_500_blok_5-8_F/vtag">>, <<"p_end_ras">>} =>    	[<<"links">>, <<"31_30_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/EEC_500_blok_5-8_F/vtag">>, <<"q_end_ras">>} =>   	[<<"links">>, <<"31_30_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/EEC_500_blok_5-8_F/vtag">>, <<"p_start_m">>} =>    	[<<"links">>, <<"31_30_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/EEC_500_blok_5-8_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"31_30_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/EEC_500_blok_5-8_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"31_30_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/EEC_500_blok_5-8_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"31_30_3">>, <<"mQk">>]
	

	%% 241_7201_3 
	, {<<"TAGS/Branches/VL_ekv_uk-pu_semey_uk_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"241_7201_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_uk-pu_semey_uk_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"241_7201_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_uk-pu_semey_uk_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"241_7201_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_uk-pu_semey_uk_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"241_7201_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_uk-pu_semey_uk_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"241_7201_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_uk-pu_semey_uk_F/vtag">>, <<"p_start_m">>} => 	  	[<<"links">>, <<"241_7201_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_uk-pu_semey_uk_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"241_7201_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_uk-pu_semey_uk_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"241_7201_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_uk-pu_semey_uk_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"241_7201_3">>, <<"mQk">>]


	%% 981_7201_3
	, {<<"TAGS/Branches/VL_ekv_semey-pu_semey_uk_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"981_7201_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_semey-pu_semey_uk_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"981_7201_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_semey-pu_semey_uk_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"981_7201_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_semey-pu_semey_uk_F/vtag">>, <<"p_end_ras">>} =>   	 	[<<"links">>, <<"981_7201_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_semey-pu_semey_uk_F/vtag">>, <<"q_end_ras">>} =>   	 	[<<"links">>, <<"981_7201_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_semey-pu_semey_uk_F/vtag">>, <<"p_start_m">>} => 	  	[<<"links">>, <<"981_7201_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_semey-pu_semey_uk_F/vtag">>, <<"q_start_m">>} =>    	    [<<"links">>, <<"981_7201_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_semey-pu_semey_uk_F/vtag">>, <<"p_end_m">>} => 	        [<<"links">>, <<"981_7201_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_semey-pu_semey_uk_F/vtag">>, <<"q_end_m">>} => 	        [<<"links">>, <<"981_7201_3">>, <<"mQk">>]
	

	%% 981_7202_3 
	, {<<"TAGS/Branches/VL_ekv_semey-pu_eec_semey_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"981_7202_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_semey-pu_eec_semey_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"981_7202_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_semey-pu_eec_semey_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"981_7202_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_semey-pu_eec_semey_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"981_7202_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_semey-pu_eec_semey_F/vtag">>, <<"q_end_ras">>} =>   	[<<"links">>, <<"981_7202_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_semey-pu_eec_semey_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"981_7202_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_semey-pu_eec_semey_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"981_7202_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_semey-pu_eec_semey_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"981_7202_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_semey-pu_eec_semey_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"981_7202_3">>, <<"mQk">>]
	

	%% 33_7202_3
	, {<<"TAGS/Branches/VL_ekv_eec-pu_eec_semey_F/vtag">>,	<<"state_b_ras">>} => 	[<<"links">>, <<"33_7202_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_eec-pu_eec_semey_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"33_7202_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_eec-pu_eec_semey_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"33_7202_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_eec-pu_eec_semey_F/vtag">>, <<"p_end_ras">>} => 	[<<"links">>, <<"33_7202_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_eec-pu_eec_semey_F/vtag">>, <<"q_end_ras">>} => 	[<<"links">>, <<"33_7202_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_eec-pu_eec_semey_F/vtag_">>, <<"p_start_m">>} => 	[<<"links">>, <<"33_7202_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_eec-pu_eec_semey_F/vtag_">>, <<"q_start_m">>} => 	[<<"links">>, <<"33_7202_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_eec-pu_eec_semey_F/vtag_">>, <<"p_end_m">>} => 		[<<"links">>, <<"33_7202_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_eec-pu_eec_semey_F/vtag_">>, <<"q_end_m">>} => 		[<<"links">>, <<"33_7202_3">>, <<"mQk">>]
	

	%% 33_7203_3 
	, {<<"TAGS/Branches/VL_ekv_eec-pu_egres_eec_F/vtag">>, <<"state_b_ras">>} => 			[<<"links">>, <<"33_7203_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_eec-pu_egres_eec_F/vtag">>, <<"p_start_ras">>} => 			[<<"links">>, <<"33_7203_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_eec-pu_egres_eec_F/vtag">>, <<"q_start_ras">>} => 			[<<"links">>, <<"33_7203_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_eec-pu_egres_eec_F/vtag">>, <<"p_end_ras">>} =>   			[<<"links">>, <<"33_7203_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_eec-pu_egres_eec_F/vtag">>, <<"q_end_ras">>} =>   			[<<"links">>, <<"33_7203_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_eec-pu_egres_eec_F/vtag">>, <<"p_start_m">>} =>          	[<<"links">>, <<"33_7203_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_eec-pu_egres_eec_F/vtag">>, <<"q_start_m">>} =>              [<<"links">>, <<"33_7203_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_eec-pu_egres_eec_F/vtag">>, <<"p_end_m">>} => 	         	[<<"links">>, <<"33_7203_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_eec-pu_egres_eec_F/vtag">>, <<"q_end_m">>} => 	        	[<<"links">>, <<"33_7203_3">>, <<"mQk">>]
	

	%% 39_7203_3
	, {<<"TAGS/Branches/VL_ekv_egres-pu_egres_eec_F/vtag">>,<<"state_b_ras">>} => 			[<<"links">>, <<"39_7203_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_egres-pu_egres_eec_F/vtag">>, <<"p_start_ras">>} => 			[<<"links">>, <<"39_7203_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_egres-pu_egres_eec_F/vtag">>, <<"q_start_ras">>} => 			[<<"links">>, <<"39_7203_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_egres-pu_egres_eec_F/vtag">>, <<"p_end_ras">>} =>   			[<<"links">>, <<"39_7203_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_egres-pu_egres_eec_F/vtag">>, <<"q_end_ras">>} => 		  	[<<"links">>, <<"39_7203_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_egres-pu_egres_eec_F/vtag">>, <<"p_start_m">>} =>          	[<<"links">>, <<"39_7203_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_egres-pu_egres_eec_F/vtag">>, <<"q_start_m">>} => 	        [<<"links">>, <<"39_7203_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_egres-pu_egres_eec_F/vtag">>, <<"p_end_m">>} => 	         	[<<"links">>, <<"39_7203_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_egres-pu_egres_eec_F/vtag">>, <<"q_end_m">>} => 		        [<<"links">>, <<"39_7203_3">>, <<"mQk">>]
	

	%% 39_7204_3
	, {<<"TAGS/Branches/Vl_ekv_egres_1-pu_cgpp_egres_1_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"39_7204_3">>, <<"state">>]	
	, {<<"TAGS/Branches/Vl_ekv_egres_1-pu_cgpp_egres_1_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"39_7204_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/Vl_ekv_egres_1-pu_cgpp_egres_1_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"39_7204_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/Vl_ekv_egres_1-pu_cgpp_egres_1_F/vtag">>, <<"p_end_ras">>} =>   		[<<"links">>, <<"39_7204_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/Vl_ekv_egres_1-pu_cgpp_egres_1_F/vtag">>, <<"q_end_ras">>} => 	      	[<<"links">>, <<"39_7204_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/Vl_ekv_egres_1-pu_cgpp_egres_1_F/vtag">>, <<"p_start_m">>} => 	        [<<"links">>, <<"39_7204_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/Vl_ekv_egres_1-pu_cgpp_egres_1_F/vtag">>, <<"q_start_m">>} => 	    	[<<"links">>, <<"39_7204_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/Vl_ekv_egres_1-pu_cgpp_egres_1_F/vtag">>, <<"p_end_m">>} => 		    [<<"links">>, <<"39_7204_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/Vl_ekv_egres_1-pu_cgpp_egres_1_F/vtag">>, <<"q_end_m">>} => 		    [<<"links">>, <<"39_7204_3">>, <<"mQk">>]
	

	%% 130_7204_3
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_egres_1_F/vtag">>,	<<"state_b_ras">>} => 		    [<<"links">>, <<"130_7204_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_egres_1_F/vtag">>, <<"p_start_ras">>} => 			[<<"links">>, <<"130_7204_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_egres_1_F/vtag">>, <<"q_start_ras">>} => 			[<<"links">>, <<"130_7204_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_egres_1_F/vtag">>, <<"p_end_ras">>} => 	   	        [<<"links">>, <<"130_7204_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_egres_1_F/vtag">>, <<"q_end_ras">>} => 	   	        [<<"links">>, <<"130_7204_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_egres_1_F/vtag">>, <<"p_start_m">>} => 	            [<<"links">>, <<"130_7204_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_egres_1_F/vtag">>, <<"q_start_m">>} => 	            [<<"links">>, <<"130_7204_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_egres_1_F/vtag">>, <<"p_end_m">>} => 	      	    [<<"links">>, <<"130_7204_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_egres_1_F/vtag">>, <<"q_end_m">>} => 	       	 	[<<"links">>, <<"130_7204_3">>, <<"mQk">>]
	

	%% 130_7205_3 
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_oskarovka_F/vtag">>, <<"state_b_ras">>} => 			    [<<"links">>, <<"130_7205_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_oskarovka_F/vtag">>, <<"p_start_ras">>} => 			    [<<"links">>, <<"130_7205_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_oskarovka_F/vtag">>, <<"q_start_ras">>} => 		    	[<<"links">>, <<"130_7205_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_oskarovka_F/vtag">>, <<"p_end_ras">>} =>    			[<<"links">>, <<"130_7205_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_oskarovka_F/vtag">>, <<"q_end_ras">>} =>    			[<<"links">>, <<"130_7205_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_oskarovka_F/vtag">>, <<"p_start_m">>} => 	        	[<<"links">>, <<"130_7205_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_oskarovka_F/vtag">>, <<"q_start_m">>} => 				[<<"links">>, <<"130_7205_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_oskarovka_F/vtag">>, <<"p_end_m">>} => 			    	[<<"links">>, <<"130_7205_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_cgpp-pu_cgpp_oskarovka_F/vtag">>, <<"q_end_m">>} => 			    	[<<"links">>, <<"130_7205_3">>, <<"mQk">>]
	

	%% 306_7205_3 
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"306_7205_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"306_7205_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"306_7205_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/vtag">>, <<"p_end_ras">>} => 	    	[<<"links">>, <<"306_7205_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/vtag">>, <<"q_end_ras">>} => 	    	[<<"links">>, <<"306_7205_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/vtag">>, <<"p_start_m">>} =>   		[<<"links">>, <<"306_7205_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/vtag">>, <<"q_start_m">>} =>   		[<<"links">>, <<"306_7205_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"306_7205_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"306_7205_3">>, <<"mQk">>]
	

	%% 39_7206_3 
	, {<<"TAGS/Branches/VL_ekv_egres_1-pu_egres_1_oskarovka_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"39_7206_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_egres_1-pu_egres_1_oskarovka_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"39_7206_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_egres_1-pu_egres_1_oskarovka_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"39_7206_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_egres_1-pu_egres_1_oskarovka_F/vtag">>, <<"p_end_ras">>} => 	    	[<<"links">>, <<"39_7206_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_egres_1-pu_egres_1_oskarovka_F/vtag">>, <<"q_end_ras">>} => 	    	[<<"links">>, <<"39_7206_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_egres_1-pu_egres_1_oskarovka_F/vtag">>, <<"p_start_m">>} => 	    	[<<"links">>, <<"39_7206_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_egres_1-pu_egres_1_oskarovka_F/vtag">>, <<"q_start_m">>} => 	    	[<<"links">>, <<"39_7206_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_egres_1-pu_egres_1_oskarovka_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"39_7206_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_egres_1-pu_egres_1_oskarovka_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"39_7206_3">>, <<"mQk">>]


	%% 306_7206_3
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/vtag">>, <<"state_b_ras">>} => 		    [<<"links">>, <<"306_7206_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/vtag">>, <<"p_start_ras">>} => 	    	[<<"links">>, <<"306_7206_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/vtag">>, <<"q_start_ras">>} => 	    	[<<"links">>, <<"306_7206_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/vtag">>, <<"p_end_ras">>} => 			[<<"links">>, <<"306_7206_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/vtag">>, <<"q_end_ras">>} => 			[<<"links">>, <<"306_7206_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/vtag">>, <<"p_start_m">>} => 			[<<"links">>, <<"306_7206_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/vtag">>, <<"q_start_m">>} => 			[<<"links">>, <<"306_7206_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/vtag">>, <<"p_end_m">>} => 		    	[<<"links">>, <<"306_7206_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/vtag">>, <<"q_end_m">>} => 		    	[<<"links">>, <<"306_7206_3">>, <<"mQk">>]
	

	%% 306_7229_3
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"306_7229_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"306_7229_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"306_7229_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/vtag">>, <<"p_end_ras">>} => 			[<<"links">>, <<"306_7229_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/vtag">>, <<"q_end_ras">>} => 			[<<"links">>, <<"306_7229_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/vtag">>, <<"p_start_m">>} => 			[<<"links">>, <<"306_7229_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/vtag">>, <<"q_start_m">>} => 			[<<"links">>, <<"306_7229_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"306_7229_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"306_7229_3">>, <<"mQk">>]
	
	

	%% 310_7208_3
	, {<<"TAGS/Branches/VL_ekv_nura-pu_kargres_nura_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"310_7208_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_nura-pu_kargres_nura_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"310_7208_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_nura-pu_kargres_nura_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"310_7208_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_nura-pu_kargres_nura_F/vtag">>, <<"p_end_ras">>} => 	    	[<<"links">>, <<"310_7208_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_nura-pu_kargres_nura_F/vtag">>, <<"q_end_ras">>} => 		    [<<"links">>, <<"310_7208_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_nura-pu_kargres_nura_F/vtag">>, <<"p_start_m">>} => 		    [<<"links">>, <<"310_7208_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_nura-pu_kargres_nura_F/vtag">>, <<"q_start_m">>} => 	    	[<<"links">>, <<"310_7208_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_nura-pu_kargres_nura_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"310_7208_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_nura-pu_kargres_nura_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"310_7208_3">>, <<"mQk">>]
	
	%% 355_7208_3 
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_nura_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"355_7208_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_nura_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"355_7208_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_nura_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"355_7208_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_nura_F/vtag">>, <<"p_end_ras">>} =>   	[<<"links">>, <<"355_7208_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_nura_F/vtag">>, <<"q_end_ras">>} =>   	[<<"links">>, <<"355_7208_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_nura_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"355_7208_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_nura_F/vtag">>, <<"q_start_m">>} =>   	[<<"links">>, <<"355_7208_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_nura_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"355_7208_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_nura_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"355_7208_3">>, <<"mQk">>]
	

	%% 355_7209_3
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_karazhal_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"355_7209_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_karazhal_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"355_7209_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_karazhal_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"355_7209_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_karazhal_F/vtag">>, <<"p_end_ras">>} => 	    	[<<"links">>, <<"355_7209_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_karazhal_F/vtag">>, <<"q_end_ras">>} => 	    	[<<"links">>, <<"355_7209_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_karazhal_F/vtag">>, <<"p_start_m">>} => 	    	[<<"links">>, <<"355_7209_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_karazhal_F/vtag">>, <<"q_start_m">>} => 	    	[<<"links">>, <<"355_7209_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_karazhal_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"355_7209_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_karazhal_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"355_7209_3">>, <<"mQk">>]
	

	%% 465_7209_3
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_kargres_karazhal_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"465_7209_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_kargres_karazhal_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"465_7209_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_kargres_karazhal_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"465_7209_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_kargres_karazhal_F/vtag">>, <<"p_end_ras">>} => 		    [<<"links">>, <<"465_7209_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_kargres_karazhal_F/vtag">>, <<"q_end_ras">>} => 		    [<<"links">>, <<"465_7209_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_karazhal-pu_kargres_karazhal_F/vtag">>, <<"p_start_m">>} => 	    	[<<"links">>, <<"465_7209_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_kargres_karazhal_F/vtag">>, <<"q_start_m">>} => 	    	[<<"links">>, <<"465_7209_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_kargres_karazhal_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"465_7209_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_kargres_karazhal_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"465_7209_3">>, <<"mQk">>]
	

	%% 355_7210_3
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_agadyr_F/vtag">>,	<<"state_b_ras">>} => 			[<<"links">>, <<"355_7210_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_agadyr_F/vtag">>, <<"p_start_ras">>} => 			[<<"links">>, <<"355_7210_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_agadyr_F/vtag">>, <<"q_start_ras">>} => 			[<<"links">>, <<"355_7210_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_agadyr_F/vtag">>, <<"p_end_ras">>} => 			[<<"links">>, <<"355_7210_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_agadyr_F/vtag">>, <<"q_end_ras">>} => 			[<<"links">>, <<"355_7210_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_agadyr_F/vtag">>, <<"p_start_m">>} => 			[<<"links">>, <<"355_7210_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_agadyr_F/vtag">>, <<"q_start_m">>} => 			[<<"links">>, <<"355_7210_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_agadyr_F/vtag">>, <<"p_end_m">>} => 				[<<"links">>, <<"355_7210_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_agadyr_F/vtag">>, <<"q_end_m">>} => 				[<<"links">>, <<"355_7210_3">>, <<"mQk">>]
	

	%% 468_7210_3
	, {<<"TAGS/Branches/VL_ekv_agadyr-pu_kargres_agadyr _F/vtag">>, <<"state_b_ras">>} => 			[<<"links">>, <<"468_7210_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_agadyr-pu_kargres_agadyr _F/vtag">>, <<"p_start_ras">>} => 			[<<"links">>, <<"468_7210_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_agadyr-pu_kargres_agadyr _F/vtag">>, <<"q_start_ras">>} => 			[<<"links">>, <<"468_7210_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_agadyr-pu_kargres_agadyr _F/vtag">>, <<"p_end_ras">>} => 	    	[<<"links">>, <<"468_7210_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_agadyr-pu_kargres_agadyr _F/vtag">>, <<"q_end_ras">>} => 	    	[<<"links">>, <<"468_7210_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_agadyr-pu_kargres_agadyr _F/vtag">>, <<"p_start_m">>} => 	    	[<<"links">>, <<"468_7210_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_agadyr-pu_kargres_agadyr _F/vtag">>, <<"q_start_m">>} => 		    [<<"links">>, <<"468_7210_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_agadyr-pu_kargres_agadyr _F/vtag">>, <<"p_end_m">>} => 				[<<"links">>, <<"468_7210_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_agadyr-pu_kargres_agadyr _F/vtag">>, <<"q_end_m">>} => 				[<<"links">>, <<"468_7210_3">>, <<"mQk">>]
	

	%% 355_7211_3 
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_balkhashskaya_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"355_7211_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_balkhashskaya_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"355_7211_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_balkhashskaya_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"355_7211_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_balkhashskaya_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"355_7211_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_balkhashskaya_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"355_7211_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_balkhashskaya_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"355_7211_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_balkhashskaya_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"355_7211_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_balkhashskaya_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"355_7211_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_kargres-pu_kargres_balkhashskaya_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"355_7211_3">>, <<"mQk">>]
	

	%% 473_7211_3  
	, {<<"TAGS/Branches/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"473_7211_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"473_7211_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"473_7211_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"473_7211_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"473_7211_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"473_7211_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"473_7211_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"473_7211_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"473_7211_3">>, <<"mQk">>]
	

	%% 901_7212_3 
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_ukgres_shu_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"901_7212_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_ukgres_shu_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"901_7212_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_ukgres_shu_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"901_7212_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_ukgres_shu_F/vtag">>, <<"p_end_ras">>} => 			[<<"links">>, <<"901_7212_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_ukgres_shu_F/vtag">>, <<"q_end_ras">>} => 			[<<"links">>, <<"901_7212_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_ukgres-pu_ukgres_shu_F/vtag">>,   <<"p_start_m">>} => 		[<<"links">>, <<"901_7212_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_ukgres_shu_F/vtag">>,   <<"q_start_m">>} => 		[<<"links">>, <<"901_7212_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_ukgres_shu_F/vtag">>,   <<"p_end_m">>} => 			[<<"links">>, <<"901_7212_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_ukgres_shu_F/vtag">>,   <<"q_end_m">>} => 			[<<"links">>, <<"901_7212_3">>, <<"mQk">>]
	

	%% 932_7212_3  
	, {<<"TAGS/Branches/VL_ekv_shu-pu_ukgres_shu_F/vtag">>,	<<"state_b_ras">>} => 		[<<"links">>, <<"932_7212_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_shu-pu_ukgres_shu_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"932_7212_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_shu-pu_ukgres_shu_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"932_7212_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_shu-pu_ukgres_shu_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"932_7212_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_shu-pu_ukgres_shu_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"932_7212_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_shu-pu_ukgres_shu_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"932_7212_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_shu-pu_ukgres_shu_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"932_7212_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_shu-pu_ukgres_shu_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"932_7212_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_shu-pu_ukgres_shu_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"932_7212_3">>, <<"mQk">>]


	%% 903_7213_3 
	, {<<"TAGS/Branches/VL_ekv_almaty-pu_almaty_alma_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"903_7213_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_almaty-pu_almaty_alma_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"903_7213_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_almaty-pu_almaty_alma_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"903_7213_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_almaty-pu_almaty_alma_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"903_7213_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_almaty-pu_almaty_alma_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"903_7213_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_almaty-pu_almaty_alma_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"903_7213_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_almaty-pu_almaty_alma_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"903_7213_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_almaty-pu_almaty_alma_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"903_7213_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_almaty-pu_almaty_alma_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"903_7213_3">>, <<"mQk">>]


	%% 939_7213_3  
	, {<<"TAGS/Branches/VL_ekv_alma-pu_almaty_alma_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"939_7213_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_alma-pu_almaty_alma_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"939_7213_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_alma-pu_almaty_alma_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"939_7213_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_alma-pu_almaty_alma_F/vtag">>, <<"p_end_ras">>} => 			[<<"links">>, <<"939_7213_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_alma-pu_almaty_alma_F/vtag">>, <<"q_end_ras">>} => 			[<<"links">>, <<"939_7213_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_alma-pu_almaty_alma_F/vtag">>, <<"p_start_m">>} => 			[<<"links">>, <<"939_7213_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_alma-pu_almaty_alma_F/vtag">>, <<"q_start_m">>} => 			[<<"links">>, <<"939_7213_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_alma-pu_almaty_alma_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"939_7213_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_alma-pu_almaty_alma_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"939_7213_3">>, <<"mQk">>]


	%% 1916_7214_3 
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_kemin_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"1916_7214_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_kemin_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"1916_7214_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_kemin_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"1916_7214_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_kemin_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"1916_7214_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_kemin_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"1916_7214_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_kemin_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"1916_7214_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_kemin_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"1916_7214_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_kemin_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"1916_7214_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_kemin_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"1916_7214_3">>, <<"mQk">>]


	%% 2952_7214_3 
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_glavnaya_kemin _F/vtag">>,	<<"state_b_ras">>} => 			[<<"links">>, <<"2952_7214_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_glavnaya_kemin _F/vtag">>, <<"p_start_ras">>} => 			[<<"links">>, <<"2952_7214_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_glavnaya_kemin _F/vtag">>, <<"q_start_ras">>} => 			[<<"links">>, <<"2952_7214_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_glavnaya_kemin _F/vtag">>, <<"p_end_ras">>} => 				[<<"links">>, <<"2952_7214_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_glavnaya_kemin _F/vtag">>, <<"q_end_ras">>} => 				[<<"links">>, <<"2952_7214_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_kemin-pu_glavnaya_kemin _F/vtag">>, <<"p_start_m">>} => 				[<<"links">>, <<"2952_7214_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_glavnaya_kemin _F/vtag">>, <<"q_start_m">>} => 				[<<"links">>, <<"2952_7214_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_glavnaya_kemin _F/vtag">>, <<"p_end_m">>} => 				[<<"links">>, <<"2952_7214_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_glavnaya_kemin _F/vtag">>, <<"q_end_m">>} => 				[<<"links">>, <<"2952_7214_3">>, <<"mQk">>]


	%% 2952_7215_3 
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_kemin_frunze_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"2952_7215_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_kemin_frunze_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"2952_7215_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_kemin_frunze_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"2952_7215_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_kemin_frunze_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"2952_7215_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_kemin_frunze_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"2952_7215_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_kemin-pu_kemin_frunze_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"2952_7215_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_kemin_frunze_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"2952_7215_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_kemin_frunze_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"2952_7215_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_kemin-pu_kemin_frunze_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"2952_7215_3">>, <<"mQk">>]


	%% 2918_7215_3 
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_kemin_frunze_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"2918_7215_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_kemin_frunze_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"2918_7215_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_kemin_frunze_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"2918_7215_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_kemin_frunze_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"2918_7215_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_kemin_frunze_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"2918_7215_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_frunze-pu_kemin_frunze_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"2918_7215_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_kemin_frunze_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"2918_7215_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_kemin_frunze_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"2918_7215_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_kemin_frunze_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"2918_7215_3">>, <<"mQk">>]


	%% 801_7216_3 
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"801_7216_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"801_7216_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"801_7216_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"801_7216_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"801_7216_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"801_7216_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"801_7216_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"801_7216_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"801_7216_3">>, <<"mQk">>]


	%% 831_7216_3 
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhambyl_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"831_7216_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhambyl_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"831_7216_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhambyl_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"831_7216_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhambyl_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"831_7216_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhambyl_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"831_7216_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhambyl_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"831_7216_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhambyl_F/vtag">>, <<"q_start_m">>} =>		[<<"links">>, <<"831_7216_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhambyl_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"831_7216_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhambyl_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"831_7216_3">>, <<"mQk">>]


	%% 2918_7217_3 
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_glavnaya_frunze_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"2918_7217_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_glavnaya_frunze_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"2918_7217_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_glavnaya_frunze_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"2918_7217_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_glavnaya_frunze_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"2918_7217_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_glavnaya_frunze_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"2918_7217_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_frunze-pu_glavnaya_frunze_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"2918_7217_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_glavnaya_frunze_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"2918_7217_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_glavnaya_frunze_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"2918_7217_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-pu_glavnaya_frunze_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"2918_7217_3">>, <<"mQk">>]


	%% 1916_7217_3 
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_frunze_F/vtag">>,	<<"state_b_ras">>} => 		[<<"links">>, <<"1916_7217_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_frunze_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"1916_7217_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_frunze_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"1916_7217_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_frunze_F/vtag">>, <<"p_end_ras">>} => 			[<<"links">>, <<"1916_7217_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_frunze_F/vtag">>, <<"q_end_ras">>} => 			[<<"links">>, <<"1916_7217_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_frunze_F/vtag">>, <<"p_start_m">>} => 			[<<"links">>, <<"1916_7217_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_frunze_F/vtag">>, <<"q_start_m">>} => 			[<<"links">>, <<"1916_7217_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_frunze_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"1916_7217_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_glavnaya-pu_glavnaya_frunze_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"1916_7217_3">>, <<"mQk">>]


	%% 907_7218_3 
	, {<<"TAGS/Branches/VL_ekv_robot-pu_taldykorgan_robot_F/vtag">>,	<<"state_b_ras">>} => 			[<<"links">>, <<"907_7218_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_robot-pu_taldykorgan_robot_F/vtag">>, <<"p_start_ras">>} => 				[<<"links">>, <<"907_7218_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_robot-pu_taldykorgan_robot_F/vtag">>, <<"q_start_ras">>} => 				[<<"links">>, <<"907_7218_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_robot-pu_taldykorgan_robot_F/vtag">>, <<"p_end_ras">>} => 				[<<"links">>, <<"907_7218_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_robot-pu_taldykorgan_robot_F/vtag">>, <<"q_end_ras">>} => 				[<<"links">>, <<"907_7218_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_robot-pu_taldykorgan_robot_F/vtag">>, <<"p_start_m">>} => 				[<<"links">>, <<"907_7218_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_robot-pu_taldykorgan_robot_F/vtag">>, <<"q_start_m">>} => 				[<<"links">>, <<"907_7218_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_robot-pu_taldykorgan_robot_F/vtag">>, <<"p_end_m">>} => 					[<<"links">>, <<"907_7218_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_robot-pu_taldykorgan_robot_F/vtag">>, <<"q_end_m">>} => 					[<<"links">>, <<"907_7218_3">>, <<"mQk">>]


	%% 988_7218_3 
	, {<<"TAGS/Branches/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/vtag">>,	<<"state_b_ras">>} => 		[<<"links">>, <<"988_7218_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"988_7218_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"988_7218_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/vtag">>, <<"p_end_ras">>} => 			[<<"links">>, <<"988_7218_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/vtag">>, <<"q_end_ras">>} => 			[<<"links">>, <<"988_7218_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/vtag">>, <<"p_start_m">>} => 			[<<"links">>, <<"988_7218_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/vtag">>, <<"q_start_m">>} => 			[<<"links">>, <<"988_7218_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"988_7218_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"988_7218_3">>, <<"mQk">>]


	%% 475_7219_3 
	, {<<"TAGS/Branches/VL_ekv_moiynty-pu_moiynty_ukgres_F/vtag">>,	<<"state_b_ras">>} => 		[<<"links">>, <<"475_7219_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_moiynty-pu_moiynty_ukgres_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"475_7219_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_moiynty-pu_moiynty_ukgres_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"475_7219_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_moiynty-pu_moiynty_ukgres_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"475_7219_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_moiynty-pu_moiynty_ukgres_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"475_7219_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_moiynty-pu_moiynty_ukgres_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"475_7219_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_moiynty-pu_moiynty_ukgres_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"475_7219_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_moiynty-pu_moiynty_ukgres_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"475_7219_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_moiynty-pu_moiynty_ukgres_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"475_7219_3">>, <<"mQk">>]


	%% 901_7219_3 
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_moiynty_ukgres_F/vtag">>,	<<"state_b_ras">>} => 		[<<"links">>, <<"901_7219_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_moiynty_ukgres_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"901_7219_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_moiynty_ukgres_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"901_7219_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_moiynty_ukgres_F/vtag">>, <<"p_end_ras">>} => 			[<<"links">>, <<"901_7219_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_moiynty_ukgres_F/vtag">>, <<"q_end_ras">>} => 			[<<"links">>, <<"901_7219_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_ukgres-pu_moiynty_ukgres_F/vtag">>, <<"p_start_m">>} => 			[<<"links">>, <<"901_7219_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_moiynty_ukgres_F/vtag">>, <<"q_start_m">>} => 			[<<"links">>, <<"901_7219_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_moiynty_ukgres_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"901_7219_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_ukgres-pu_moiynty_ukgres_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"901_7219_3">>, <<"mQk">>]


	%% 465_468_3 
	, {<<"TAGS/Branches/VL2428_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"465_468_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2428_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"465_468_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2428_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"465_468_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2428_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"465_468_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2428_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"465_468_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2428_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"465_468_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2428_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"465_468_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2428_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"465_468_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2428_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"465_468_3">>, <<"mQk">>]



	%% 465_7221_3
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/vtag">>,	<<"state_b_ras">>} => 	[<<"links">>, <<"465_7221_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"465_7221_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"465_7221_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"465_7221_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"465_7221_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"465_7221_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"465_7221_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"465_7221_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"465_7221_3">>, <<"mQk">>]


	%% 466_7221_3 
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"466_7221_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"466_7221_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"466_7221_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"466_7221_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"466_7221_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"466_7221_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"466_7221_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"466_7221_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"466_7221_3">>, <<"mQk">>]


	%% 831_7228_3 VL2309_F 
	, {<<"TAGS/Branches/VL2309_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"831_7228_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2309_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"831_7228_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2309_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"831_7228_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2309_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"831_7228_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2309_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"831_7228_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2309_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"831_7228_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2309_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"831_7228_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2309_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"831_7228_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2309_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"831_7228_3">>, <<"mQk">>]


	%% 7228_840_3 VL2439_F 
	, {<<"TAGS/Branches/VL2439_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"7228_840_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2439_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"7228_840_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2439_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"7228_840_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2439_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"7228_840_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2439_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"7228_840_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2439_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"7228_840_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2439_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"7228_840_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2439_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"7228_840_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2439_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"7228_840_3">>, <<"mQk">>]



	%% 840_869_3
	, {<<"TAGS/Branches/VL2519_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"840_869_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2519_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"840_869_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2519_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"840_869_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2519_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"840_869_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2519_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"840_869_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2519_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"840_869_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2519_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"840_869_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2519_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"840_869_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2519_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"840_869_3">>, <<"mQk">>]


	%% 869_7226_3 
	, {<<"TAGS/Branches/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"869_7226_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"869_7226_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"869_7226_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/vtag">>, <<"p_end_ras">>} => 	[<<"links">>, <<"869_7226_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/vtag">>, <<"q_end_ras">>} => 	[<<"links">>, <<"869_7226_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/vtag">>, <<"p_start_m">>} => 	[<<"links">>, <<"869_7226_3">>, <<"mPn">>]
    , {<<"TAGS/Branches/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/vtag">>, <<"q_start_m">>} => 	[<<"links">>, <<"869_7226_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"869_7226_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"869_7226_3">>, <<"mQk">>]


	%% 862_7227_3 
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"862_7227_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"862_7227_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"862_7227_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/vtag">>, <<"p_end_ras">>} => 	[<<"links">>, <<"862_7227_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/vtag">>, <<"q_end_ras">>} => 	[<<"links">>, <<"862_7227_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/vtag">>, <<"p_start_m">>} => 	[<<"links">>, <<"862_7227_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/vtag">>, <<"q_start_m">>} => 	[<<"links">>, <<"862_7227_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"862_7227_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"862_7227_3">>, <<"mQk">>]


	%% 466_467_3 
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-kumkol_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"466_467_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-kumkol_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"466_467_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-kumkol_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"466_467_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-kumkol_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"466_467_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-kumkol_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"466_467_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_zhezkazgan-kumkol_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"466_467_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-kumkol_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"466_467_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-kumkol_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"466_467_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_zhezkazgan-kumkol_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"466_467_3">>, <<"mQk">>]


	%% 928_904_3
	, {<<"TAGS/Branches/VL2023_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"928_904_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2023_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"928_904_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2023_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"928_904_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2023_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"928_904_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2023_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"928_904_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2023_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"928_904_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2023_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"928_904_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2023_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"928_904_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2023_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"928_904_3">>, <<"mQk">>]


	%% 903_928_3
	, {<<"TAGS/Branches/VL2773_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"903_928_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2773_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"903_928_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2773_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"903_928_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2773_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"903_928_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2773_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"903_928_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2773_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"903_928_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2773_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"903_928_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2773_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"903_928_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2773_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"903_928_3">>, <<"mQk">>]


	%% 953_904_3  
	, {<<"TAGS/Branches/VL_ekv_koyan_kos-atets_3_F/vtag">>, <<"state_b_ras">>} => 			[<<"links">>, <<"953_904_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_koyan_kos-atets_3_F/vtag">>, <<"p_start_ras">>} => 			[<<"links">>, <<"953_904_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_koyan_kos-atets_3_F/vtag">>, <<"q_start_ras">>} => 			[<<"links">>, <<"953_904_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_koyan_kos-atets_3_F/vtag">>, <<"p_end_ras">>} => 			[<<"links">>, <<"953_904_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_koyan_kos-atets_3_F/vtag">>, <<"q_end_ras">>} => 			[<<"links">>, <<"953_904_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_koyan_kos-atets_3_F/vtag">>, <<"p_start_m">>} => 			[<<"links">>, <<"953_904_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_koyan_kos-atets_3_F/vtag">>, <<"q_start_m">>} => 			[<<"links">>, <<"953_904_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_koyan_kos-atets_3_F/vtag">>, <<"p_end_m">>} => 				[<<"links">>, <<"953_904_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_koyan_kos-atets_3_F/vtag">>, <<"q_end_m">>} => 				[<<"links">>, <<"953_904_3">>, <<"mQk">>]


	%% 467_7227_3 
	, {<<"TAGS/Branches/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"467_7227_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"467_7227_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"467_7227_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/vtag">>, <<"p_end_ras">>} => 	[<<"links">>, <<"467_7227_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/vtag">>, <<"q_end_ras">>} => 	[<<"links">>, <<"467_7227_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/vtag">>, <<"p_start_m">>} => 	[<<"links">>, <<"467_7227_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/vtag">>, <<"q_start_m">>} => 	[<<"links">>, <<"467_7227_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"467_7227_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"467_7227_3">>, <<"mQk">>]


	%% 801_805_3 
	, {<<"TAGS/Branches/VL_ekv_zhambyl_zhgres/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"801_805_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_zhambyl_zhgres/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"801_805_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl_zhgres/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"801_805_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl_zhgres/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"801_805_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl_zhgres/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"801_805_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_zhambyl_zhgres/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"801_805_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl_zhgres/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"801_805_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl_zhgres/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"801_805_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl_zhgres/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"801_805_3">>, <<"mQk">>]


	%% 932_814_3
	, {<<"TAGS/Branches/VL2233_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"932_814_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2233_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"932_814_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2233_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"932_814_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2233_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"932_814_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2233_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"932_814_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2233_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"932_814_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2233_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"932_814_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2233_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"932_814_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2233_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"932_814_3">>, <<"mQk">>]
    

	%% 814_805_3
	, {<<"TAGS/Branches/VL2249_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"814_805_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2249_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"814_805_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2249_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"814_805_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2249_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"814_805_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2249_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"814_805_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2249_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"814_805_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2249_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"814_805_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2249_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"814_805_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2249_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"814_805_3">>, <<"mQk">>]
    

	%% 7223_805_3 
	, {<<"TAGS/Branches/VL_ekv_zhgres-pu_frunze_zhgres_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"7223_805_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_zhgres-pu_frunze_zhgres_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"7223_805_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_zhgres-pu_frunze_zhgres_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"7223_805_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_zhgres-pu_frunze_zhgres_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"7223_805_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_zhgres-pu_frunze_zhgres_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"7223_805_3">>, <<"Qk">>]
  	, {<<"TAGS/Branches/VL_ekv_zhgres-pu_frunze_zhgres_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"7223_805_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_zhgres-pu_frunze_zhgres_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"7223_805_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_zhgres-pu_frunze_zhgres_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"7223_805_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_zhgres-pu_frunze_zhgres_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"7223_805_3">>, <<"mQk">>]
    

	%% 7223_2918_3 
	, {<<"TAGS/Branches/VL_ekv_pu_zhgres_frunze-frunze220_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"7223_2918_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_pu_zhgres_frunze-frunze220_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"7223_2918_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_pu_zhgres_frunze-frunze220_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"7223_2918_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_pu_zhgres_frunze-frunze220_F/vtag">>, <<"p_end_ras">>} => 	[<<"links">>, <<"7223_2918_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_pu_zhgres_frunze-frunze220_F/vtag">>, <<"q_end_ras">>} => 	[<<"links">>, <<"7223_2918_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_pu_zhgres_frunze-frunze220_F/vtag">>, <<"p_start_m">>} => 	[<<"links">>, <<"7223_2918_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_pu_zhgres_frunze-frunze220_F/vtag">>, <<"q_start_m">>} => 	[<<"links">>, <<"7223_2918_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_pu_zhgres_frunze-frunze220_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"7223_2918_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_pu_zhgres_frunze-frunze220_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"7223_2918_3">>, <<"mQk">>]
    

	%% 839_843_3
	, {<<"TAGS/Branches/VL2449_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"839_843_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2449_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"839_843_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2449_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"839_843_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2449_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"839_843_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2449_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"839_843_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2449_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"839_843_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2449_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"839_843_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2449_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"839_843_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2449_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"839_843_3">>, <<"mQk">>]
    

	%% 839_7225_3 
	, {<<"TAGS/Branches/VL_ekv_zhylga-pu_shymkent_zhylga_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"839_7225_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_zhylga-pu_shymkent_zhylga_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"839_7225_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_zhylga-pu_shymkent_zhylga_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"839_7225_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_zhylga-pu_shymkent_zhylga_F/vtag">>, <<"p_end_ras">>} => 	[<<"links">>, <<"839_7225_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_zhylga-pu_shymkent_zhylga_F/vtag">>, <<"q_end_ras">>} => 	[<<"links">>, <<"839_7225_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_zhylga-pu_shymkent_zhylga_F/vtag">>, <<"p_start_m">>} => 	[<<"links">>, <<"839_7225_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_zhylga-pu_shymkent_zhylga_F/vtag">>, <<"q_start_m">>} => 	[<<"links">>, <<"839_7225_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_zhylga-pu_shymkent_zhylga_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"839_7225_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_zhylga-pu_shymkent_zhylga_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"839_7225_3">>, <<"mQk">>]
    

	%% 839_2925_3 
	, {<<"TAGS/Branches/VL2429_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"839_2925_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2429_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"839_2925_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2429_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"839_2925_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2429_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"839_2925_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2429_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"839_2925_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2429_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"839_2925_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2429_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"839_2925_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2429_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"839_2925_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2429_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"839_2925_3">>, <<"mQk">>]
    

	%% 2925_843_3
	, {<<"TAGS/Branches/VL2419_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"2925_843_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2419_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"2925_843_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2419_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"2925_843_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2419_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"2925_843_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2419_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"2925_843_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2419_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"2925_843_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2419_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"2925_843_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2419_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"2925_843_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2419_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"2925_843_3">>, <<"mQk">>]
    
    
	%% 831_843_3
	, {<<"TAGS/Branches/VL2349_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"831_843_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2349_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"831_843_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2349_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"831_843_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2349_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"831_843_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2349_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"831_843_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2349_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"831_843_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2349_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"831_843_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2349_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"831_843_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2349_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"831_843_3">>, <<"mQk">>]
    

	%% 842_843_3
	, {<<"TAGS/Branches/VL2319_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"842_843_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2319_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"842_843_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2319_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"842_843_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2319_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"842_843_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2319_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"842_843_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2319_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"842_843_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2319_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"842_843_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2319_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"842_843_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2319_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"842_843_3">>, <<"mQk">>]
    

	%% 842_840_3
	, {<<"TAGS/Branches/VL2549_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"842_840_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2549_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"842_840_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2549_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"842_840_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2549_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"842_840_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2549_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"842_840_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2549_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"842_840_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2549_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"842_840_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2549_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"842_840_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2549_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"842_840_3">>, <<"mQk">>]
    

	%% 840_7224_3 
	, {<<"TAGS/Branches/VL_ekv_kentau-pu_kentau_zhambyl_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"840_7224_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_kentau-pu_kentau_zhambyl_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"840_7224_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_kentau-pu_kentau_zhambyl_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"840_7224_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_kentau-pu_kentau_zhambyl_F/vtag">>, <<"p_end_ras">>} => 			[<<"links">>, <<"840_7224_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_kentau-pu_kentau_zhambyl_F/vtag">>, <<"q_end_ras">>} => 			[<<"links">>, <<"840_7224_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_kentau-pu_kentau_zhambyl_F/vtag">>, <<"p_start_m">>} => 			[<<"links">>, <<"840_7224_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_kentau-pu_kentau_zhambyl_F/vtag">>, <<"q_start_m">>} => 			[<<"links">>, <<"840_7224_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_kentau-pu_kentau_zhambyl_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"840_7224_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_kentau-pu_kentau_zhambyl_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"840_7224_3">>, <<"mQk">>]
    

	%% 7224_801_3 
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_kentau_zhambyl_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"7224_801_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_kentau_zhambyl_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"7224_801_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_kentau_zhambyl_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"7224_801_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_kentau_zhambyl_F/vtag">>, <<"p_end_ras">>} => 	[<<"links">>, <<"7224_801_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_kentau_zhambyl_F/vtag">>, <<"q_end_ras">>} => 	[<<"links">>, <<"7224_801_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_zhambyl-pu_kentau_zhambyl_F/vtag">>, <<"p_start_m">>} => 	[<<"links">>, <<"7224_801_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_kentau_zhambyl_F/vtag">>, <<"q_start_m">>} => 	[<<"links">>, <<"7224_801_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_kentau_zhambyl_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"7224_801_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_zhambyl-pu_kentau_zhambyl_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"7224_801_3">>, <<"mQk">>]
    

	%% 2919_2921_3  
	, {<<"TAGS/Branches/VL_ekv_frunze-ca_1_F/vtag">>, <<"state_b_ras">>} => 		[<<"links">>, <<"2919_2921_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL_ekv_frunze-ca_1_F/vtag">>, <<"p_start_ras">>} => 		[<<"links">>, <<"2919_2921_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-ca_1_F/vtag">>, <<"q_start_ras">>} => 		[<<"links">>, <<"2919_2921_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-ca_1_F/vtag">>, <<"p_end_ras">>} => 			[<<"links">>, <<"2919_2921_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-ca_1_F/vtag">>, <<"q_end_ras">>} =>			[<<"links">>, <<"2919_2921_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL_ekv_frunze-ca_1_F/vtag">>, <<"p_start_m">>} => 			[<<"links">>, <<"2919_2921_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-ca_1_F/vtag">>, <<"q_start_m">>} => 			[<<"links">>, <<"2919_2921_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-ca_1_F/vtag">>, <<"p_end_m">>} => 			[<<"links">>, <<"2919_2921_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_frunze-ca_1_F/vtag">>, <<"q_end_m">>} => 			[<<"links">>, <<"2919_2921_3">>, <<"mQk">>]
    

	%% 865_842_3
	, {<<"TAGS/Branches/VL2539_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"865_842_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2539_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"865_842_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2539_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"865_842_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2539_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"865_842_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2539_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"865_842_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2539_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"865_842_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2539_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"865_842_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2539_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"865_842_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2539_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"865_842_3">>, <<"mQk">>]
    

	%% 865_869_3
	, {<<"TAGS/Branches/VL2529_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"865_869_3">>, <<"state">>]	
	, {<<"TAGS/Branches/VL2529_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"865_869_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL2529_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"865_869_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL2529_F/vtag">>, <<"p_end_ras">>} => 		[<<"links">>, <<"865_869_3">>, <<"Pk">>]
	, {<<"TAGS/Branches/VL2529_F/vtag">>, <<"q_end_ras">>} => 		[<<"links">>, <<"865_869_3">>, <<"Qk">>]
    , {<<"TAGS/Branches/VL2529_F/vtag">>, <<"p_start_m">>} => 		[<<"links">>, <<"865_869_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL2529_F/vtag">>, <<"q_start_m">>} => 		[<<"links">>, <<"865_869_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL2529_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"865_869_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL2529_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"865_869_3">>, <<"mQk">>]


		%% 831_7225_3
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhylga_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"831_7225_3">>, <<"state">>]
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhylga_F/vtag">>, <<"p_start_ras">>} => 	[<<"links">>, <<"831_7225_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhylga_F/vtag">>, <<"q_start_ras">>} => 	[<<"links">>, <<"831_7225_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhylga_F/vtag">>, <<"p_end_ras">>} => 	[<<"links">>, <<"831_7225_3">>, <<"Pk">>]  
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhylga_F/vtag">>, <<"q_end_ras">>} =>		[<<"links">>, <<"831_7225_3">>, <<"Qk">>]   
    , {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhylga_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"831_7225_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhylga_F/vtag">>, <<"q_start_m">>} =>  	[<<"links">>, <<"831_7225_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhylga_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"831_7225_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_shymkent-pu_shymkent_zhylga_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"831_7225_3">>, <<"mQk">>]



		% 862_7226_3
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"862_7226_3">>, <<"state">>]
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/vtag">>, <<"p_start_ras">>} =>	[<<"links">>, <<"862_7226_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/vtag">>, <<"q_start_ras">>} =>	[<<"links">>, <<"862_7226_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/vtag">>, <<"p_end_ras">>} =>		[<<"links">>, <<"862_7226_3">>, <<"Pk">>]    
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/vtag">>, <<"q_end_ras">>} =>		[<<"links">>, <<"862_7226_3">>, <<"Qk">>] 
    , {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"862_7226_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/vtag">>, <<"q_start_m">>} =>  	[<<"links">>, <<"862_7226_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"862_7226_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"862_7226_3">>, <<"mQk">>]


		% 355_7229_3
	, {<<"TAGS/Branches/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/vtag">>, <<"state_b_ras">>} => 	[<<"links">>, <<"355_7229_3">>, <<"state">>]
	, {<<"TAGS/Branches/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/vtag">>, <<"p_start_ras">>} =>	[<<"links">>, <<"355_7229_3">>, <<"Pn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/vtag">>, <<"q_start_ras">>} =>	[<<"links">>, <<"355_7229_3">>, <<"Qn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/vtag">>, <<"p_end_ras">>} =>	[<<"links">>, <<"355_7229_3">>, <<"Pk">>] 
	, {<<"TAGS/Branches/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/vtag">>, <<"q_end_ras">>} => 	[<<"links">>, <<"355_7229_3">>, <<"Qk">>]    
    , {<<"TAGS/Branches/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/vtag">>, <<"p_start_m">>} =>   	[<<"links">>, <<"355_7229_3">>, <<"mPn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/vtag">>, <<"q_start_m">>} =>  	[<<"links">>, <<"355_7229_3">>, <<"mQn">>]
	, {<<"TAGS/Branches/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/vtag">>, <<"p_end_m">>} => 		[<<"links">>, <<"355_7229_3">>, <<"mPk">>]
	, {<<"TAGS/Branches/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/vtag">>, <<"q_end_m">>} => 		[<<"links">>, <<"355_7229_3">>, <<"mQk">>]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% NODES begin -------------------------------------------------------------------------------------------------------------------------------------------------

	%% 26
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"26">>, <<"state">>]	
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"26">>, <<"U">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"26">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"26">>, <<"Pg">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"26">>, <<"Qg">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"26">>, <<"Pn">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"26">>, <<"Qn">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"26">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"26">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"26">>, <<"mU">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"26">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"26">>, <<"mPg">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"26">>, <<"mQg">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"26">>, <<"mPn">>]
	, {<<"TAGS/Nodes/EK-1150_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"26">>, <<"mQn">>]


	%% 25
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"25">>, <<"state">>]	
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"25">>, <<"U">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"25">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"25">>, <<"Pg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"25">>, <<"Qg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"25">>, <<"Pn">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"25">>, <<"Qn">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"25">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"25">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"25">>, <<"mU">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"25">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"25">>, <<"mPg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"25">>, <<"mQg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"25">>, <<"mPn">>]
	, {<<"TAGS/Nodes/EGRES-1_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"25">>, <<"mQn">>]

	%% 325
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"325">>, <<"state">>]	
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"325">>, <<"U">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"325">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"325">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"325">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"325">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"325">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"325">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"325">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"325">>, <<"mU">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"325">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"325">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"325">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"325">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Nura_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"325">>, <<"mQn">>]

	%% 469
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"469">>, <<"state">>]	
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"469">>, <<"U">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"469">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"469">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"469">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"469">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"469">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"469">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"469">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"469">>, <<"mU">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"469">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"469">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"469">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"469">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Agadyr_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"469">>, <<"mQn">>]
	
	%% 900
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"900">>, <<"state">>]	
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"900">>, <<"U">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"900">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"900">>, <<"Pg">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"900">>, <<"Qg">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"900">>, <<"Pn">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"900">>, <<"Qn">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"900">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"900">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"900">>, <<"mU">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"900">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"900">>, <<"mPg">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"900">>, <<"mQg">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"900">>, <<"mPn">>]
	, {<<"TAGS/Nodes/UKGRES_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"900">>, <<"mQn">>]

	%% 902
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"902">>, <<"state">>]	
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"902">>, <<"U">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"902">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"902">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"902">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"902">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"902">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"902">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"902">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"902">>, <<"mU">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"902">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"902">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"902">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"902">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Almaty_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"902">>, <<"mQn">>]

	%% 938
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"938">>, <<"state">>]	
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"938">>, <<"U">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"938">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"938">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"938">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"938">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"938">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"938">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"938">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"938">>, <<"mU">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"938">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"938">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"938">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"938">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Alma_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"938">>, <<"mQn">>]
	
	%% 9932
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"9932">>, <<"state">>]	
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"9932">>, <<"U">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"u_fi_n_ras">>} => 			[<<"nodes">>, <<"9932">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"9932">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"9932">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"p_load_ras">>} => 			[<<"nodes">>, <<"9932">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"q_load_ras">>} => 			[<<"nodes">>, <<"9932">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"9932">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"9932">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"9932">>, <<"mU">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"9932">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"9932">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"9932">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"9932">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Shu_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"9932">>, <<"mQn">>]

	%% 2919
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"2919">>, <<"state">>]	
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"2919">>, <<"U">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"2919">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"2919">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"2919">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"2919">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"2919">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"2919">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"2919">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"2919">>, <<"mU">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"2919">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"2919">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"2919">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"2919">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Frunze_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"2919">>, <<"mQn">>]

	%% 800
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"800">>, <<"state">>]	
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"800">>, <<"U">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"800">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"800">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"800">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"800">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"800">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"800">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"800">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"800">>, <<"mU">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"800">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"800">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"800">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"800">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Zhambyl_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"800">>, <<"mQn">>]
	
	%% 830
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"830">>, <<"state">>]	
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"830">>, <<"U">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"u_fi_n_ras">>} => 	[<<"nodes">>, <<"830">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"830">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"830">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"p_load_ras">>} => 	[<<"nodes">>, <<"830">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"q_load_ras">>} => 	[<<"nodes">>, <<"830">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"830">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"830">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"830">>, <<"mU">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"830">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"830">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"830">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"830">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Shymkent_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"830">>, <<"mQn">>]

	%% 980
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"980">>, <<"state">>]	
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"980">>, <<"U">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"980">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"980">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"980">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"980">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"980">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"980">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"980">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"980">>, <<"mU">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"980">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"980">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"980">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"980">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Semey_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"980">>, <<"mQn">>]

	%% 986
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"986">>, <<"state">>]	
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"986">>, <<"U">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"986">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"986">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"986">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"986">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"986">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"986">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"986">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"986">>, <<"mU">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"986">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"986">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"986">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"986">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Aktogay_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"986">>, <<"mQn">>]
	
	%% 987
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"987">>, <<"state">>]	
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"987">>, <<"U">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"987">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"987">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"987">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"987">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"987">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"987">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"987">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"987">>, <<"mU">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"987">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"987">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"987">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"987">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"987">>, <<"mQn">>]

	%% 240
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"240">>, <<"state">>]	
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"240">>, <<"U">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"u_fi_n_ras">>} => 	[<<"nodes">>, <<"240">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"240">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"240">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"p_load_ras">>} => 	[<<"nodes">>, <<"240">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"q_load_ras">>} => 	[<<"nodes">>, <<"240">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"240">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"240">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"240">>, <<"mU">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"240">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"240">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"240">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"240">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"240">>, <<"mQn">>]

	%% 224
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"state_n_ras">>} =>	 	[<<"nodes">>, <<"224">>, <<"state">>]	
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"224">>, <<"U">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"224">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"224">>, <<"Pg">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"224">>, <<"Qg">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"224">>, <<"Pn">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"224">>, <<"Qn">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"224">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"224">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"224">>, <<"mU">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"224">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"224">>, <<"mPg">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"224">>, <<"mQg">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"224">>, <<"mPn">>]
	, {<<"TAGS/Nodes/TashGRES_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"224">>, <<"mQn">>]


	%% 129
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"129">>, <<"state">>]	
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"129">>, <<"U">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"129">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"129">>, <<"Pg">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"129">>, <<"Qg">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"129">>, <<"Pn">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"129">>, <<"Qn">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"129">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"129">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"129">>, <<"mU">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"129">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"129">>, <<"mPg">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"129">>, <<"mQg">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"129">>, <<"mPn">>]
	, {<<"TAGS/Nodes/CGPP_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"129">>, <<"mQn">>]
	
	%% 31
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"31">>, <<"state">>]	
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"31">>, <<"U">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"u_fi_n_ras">>} => 			[<<"nodes">>, <<"31">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"31">>, <<"Pg">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"31">>, <<"Qg">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"p_load_ras">>} => 			[<<"nodes">>, <<"31">>, <<"Pn">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"q_load_ras">>} => 			[<<"nodes">>, <<"31">>, <<"Qn">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"31">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"31">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"31">>, <<"mU">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"31">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"31">>, <<"mPg">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"31">>, <<"mQg">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"31">>, <<"mPn">>]
	, {<<"TAGS/Nodes/EEC_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"31">>, <<"mQn">>]

	%% 1621
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"1621">>, <<"state">>]	
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"1621">>, <<"U">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"u_fi_n_ras">>} => 	[<<"nodes">>, <<"1621">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"p_gen_ras">>} => 	[<<"nodes">>, <<"1621">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"q_gen_ras">>} => 	[<<"nodes">>, <<"1621">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"p_load_ras">>} => 	[<<"nodes">>, <<"1621">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"q_load_ras">>} => 	[<<"nodes">>, <<"1621">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"1621">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"1621">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"u_n_m">>} => 		[<<"nodes">>, <<"1621">>, <<"mU">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"1621">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"1621">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"1621">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"1621">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Rubtsovsk_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"1621">>, <<"mQn">>]

	%% 1660
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"1660">>, <<"state">>]	
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"1660">>, <<"U">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"1660">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"1660">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"1660">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"1660">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"1660">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"1660">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"1660">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"1660">>, <<"mU">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"1660">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"1660">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"1660">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"1660">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Altai_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"1660">>, <<"mQn">>]
	
	%% 241
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"241">>, <<"state">>]	
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"241">>, <<"U">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"241">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"241">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"241">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"241">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"241">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"241">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"241">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"241">>, <<"mU">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"241">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"241">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"241">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"241">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Usti-Kamenogorsk_F/u220/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"241">>, <<"mQn">>]

	%% 981
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"981">>, <<"state">>]	
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"981">>, <<"U">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"981">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"981">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"981">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"981">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"981">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"981">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"981">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"981">>, <<"mU">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"981">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"981">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"981">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"981">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Semey_F/u220/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"981">>, <<"mQn">>]

	%% 33
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"33">>, <<"state">>]	
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"33">>, <<"U">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"33">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"33">>, <<"Pg">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"33">>, <<"Qg">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"33">>, <<"Pn">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"33">>, <<"Qn">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"33">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"33">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"33">>, <<"mU">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"33">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"33">>, <<"mPg">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"33">>, <<"m">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"33">>, <<"mPn">>]
	, {<<"TAGS/Nodes/EEC_F/u220/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"33">>, <<"mQn">>]
	
	%% 39
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"39">>, <<"state">>]	
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"39">>, <<"U">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"39">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"39">>, <<"Pg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"39">>, <<"Qg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"39">>, <<"Pn">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"39">>, <<"Qn">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"39">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"39">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"39">>, <<"mU">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"39">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"39">>, <<"mPg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"39">>, <<"mQg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"39">>, <<"mPn">>]
	, {<<"TAGS/Nodes/EGRES-1_F/u220/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"39">>, <<"mQn">>]

	%% 130
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"130">>, <<"state">>]	
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"130">>, <<"U">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"130">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"130">>, <<"Pg">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"130">>, <<"Qg">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"130">>, <<"Pn">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"130">>, <<"Qn">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"130">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"130">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"130">>, <<"mU">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"130">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"130">>, <<"mPg">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"130">>, <<"mQg">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"130">>, <<"mPn">>]
	, {<<"TAGS/Nodes/CGPP_F/u220/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"130">>, <<"mQn">>]

	%% 306
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"306">>, <<"state">>]	
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"306">>, <<"U">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"306">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"306">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"306">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"306">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"306">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"306">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"306">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"306">>, <<"mU">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"306">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"306">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"306">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"306">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Oskarovka-220_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"306">>, <<"mQn">>]
	
	%% 310
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"310">>, <<"state">>]	
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"310">>, <<"U">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"310">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"310">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"310">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"310">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"310">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"310">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"310">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"310">>, <<"mU">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"310">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"310">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"310">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"310">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Nura_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"310">>, <<"mQn">>]

	%% 468
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"468">>, <<"state">>]	
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"468">>, <<"U">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"468">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"468">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"468">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"468">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"468">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"468">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"468">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"468">>, <<"mU">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"468">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"468">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"468">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"468">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Agadyr_F/u220/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"468">>, <<"mQn">>]

	%% 901
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"901">>, <<"state">>]	
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"901">>, <<"U">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"901">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"901">>, <<"Pg">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"901">>, <<"Qg">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"901">>, <<"Pn">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"901">>, <<"Qn">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"901">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"901">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"901">>, <<"mU">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"901">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"901">>, <<"mPg">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"901">>, <<"mQg">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"901">>, <<"mPn">>]
	, {<<"TAGS/Nodes/UKGRES_F/u220/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"901">>, <<"mQn">>]

	%% 932
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"932">>, <<"state">>]	
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"932">>, <<"U">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"932">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"932">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"932">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"932">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"932">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"932">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"932">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"932">>, <<"mU">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"932">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"932">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"932">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"932">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Shu_F/u220/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"932">>, <<"mQn">>]
	
	%% 1916
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"1916">>, <<"state">>]	
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"1916">>, <<"U">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"u_fi_n_ras">>} => 	[<<"nodes">>, <<"1916">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"1916">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"1916">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"p_load_ras">>} => 	[<<"nodes">>, <<"1916">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"q_load_ras">>} => 	[<<"nodes">>, <<"1916">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"1916">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"1916">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"1916">>, <<"mU">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"1916">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"1916">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"1916">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"1916">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Glavnaya_220_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"1916">>, <<"mQn">>]


	%% 903
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"903">>, <<"state">>]	
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"903">>, <<"U">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"903">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"903">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"903">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"903">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"903">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"903">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"903">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"903">>, <<"mU">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"903">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"903">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"903">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"903">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Almaty_F/u220/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"903">>, <<"mQn">>]

	%% 939
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"939">>, <<"state">>]	
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"939">>, <<"U">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"939">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"939">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"939">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"939">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"939">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"939">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"939">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"939">>, <<"mU">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"939">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"939">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"939">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"939">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Alma_F/u220/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"939">>, <<"mQn">>]
	
	%% 988
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"988">>, <<"state">>]	
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"988">>, <<"U">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"988">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"988">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"988">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"988">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"988">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"988">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"988">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"988">>, <<"mU">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"988">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"988">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"988">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"988">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Taldykorgan_F/u220/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"988">>, <<"mQn">>]

	%% 918
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"918">>, <<"state">>]	
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"918">>, <<"U">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"918">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"918">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"918">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"918">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"918">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"918">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"918">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"918">>, <<"mU">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"918">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"918">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"918">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"918">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Zapadnaya_220_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"918">>, <<"mQn">>]

	%% 2952
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"2952">>, <<"state">>]	
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"2952">>, <<"U">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"2952">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"2952">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"2952">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"2952">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"2952">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"2952">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"2952">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"2952">>, <<"mU">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"2952">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"2952">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"2952">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"2952">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Kemin_220_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"2952">>, <<"mQn">>]

	
	%% 2918 
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"2918">>, <<"state">>]	
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"2918">>, <<"U">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"u_fi_n_ras">>} => 			[<<"nodes">>, <<"2918">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"2918">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"2918">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"p_load_ras">>} => 			[<<"nodes">>, <<"2918">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"q_load_ras">>} => 			[<<"nodes">>, <<"2918">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"2918">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"2918">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"2918">>, <<"mU">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"2918">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"2918">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"2918">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"2918">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Frunze_F/u220/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"2918">>, <<"mQn">>]

	%% 801
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"801">>, <<"state">>]	
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"801">>, <<"U">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"801">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"801">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"801">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"801">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"801">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"801">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"801">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"801">>, <<"mU">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"801">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"801">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"801">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"801">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Zhambyl_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"801">>, <<"mQn">>]

	%% 831
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"831">>, <<"state">>]	
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"831">>, <<"U">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"831">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"831">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"831">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"831">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"831">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"831">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"831">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"831">>, <<"mU">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"831">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"831">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"831">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"831">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Shymkent_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"831">>, <<"mQn">>]
	
	%% 2925
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"2925">>, <<"state">>]	
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"2925">>, <<"U">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"u_fi_n_ras">>} => 			[<<"nodes">>, <<"2925">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"2925">>, <<"Pg">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"2925">>, <<"Qg">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"p_load_ras">>} => 			[<<"nodes">>, <<"2925">>, <<"Pn">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"q_load_ras">>} => 			[<<"nodes">>, <<"2925">>, <<"Qn">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"2925">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"2925">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"2925">>, <<"mU">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"2925">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"2925">>, <<"mPg">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"2925">>, <<"mQg">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"2925">>, <<"mPn">>]
	, {<<"TAGS/Nodes/TashTES-220_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"2925">>, <<"mQn">>]

	%% 1630
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"1630">>, <<"state">>]	
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"1630">>, <<"U">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"1630">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"1630">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"1630">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"1630">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"1630">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"1630">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"1630">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"1630">>, <<"mU">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"1630">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"1630">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"1630">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"1630">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Barnaul_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"1630">>, <<"mQn">>]

	%% 175
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"175">>, <<"state">>]	
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"175">>, <<"U">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"175">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"175">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"175">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"175">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"175">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"175">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"175">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"175">>, <<"mU">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"175">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"175">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"175">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"175">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Kokshetau_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"175">>, <<"mQn">>]

	%% 590
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"590">>, <<"state">>]	
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"590">>, <<"U">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"590">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"590">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"590">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"590">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"590">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"590">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"590">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"590">>, <<"mU">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"590">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"590">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"590">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"590">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Kostanai_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"590">>, <<"mQn">>]
	
	%% 576
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"576">>, <<"state">>]	
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"576">>, <<"U">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"576">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"576">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"576">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"576">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"576">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"576">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"576">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"576">>, <<"mU">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"576">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"576">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"576">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"576">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Sokol_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"576">>, <<"mQn">>]

	%% 180
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"180">>, <<"state">>]	
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"180">>, <<"U">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"180">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"180">>, <<"Pg">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"180">>, <<"Qg">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"180">>, <<"Pn">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"180">>, <<"Qn">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"180">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"180">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"180">>, <<"mU">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"180">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"180">>, <<"mPg">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"180">>, <<"mQg">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"180">>, <<"mPn">>]
	, {<<"TAGS/Nodes/PS-EGPP_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"180">>, <<"mQn">>]

	%% 1631
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"1631">>, <<"state">>]	
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"1631">>, <<"U">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"1631">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"1631">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"1631">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"1631">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"1631">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"1631">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"1631">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"1631">>, <<"mU">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"1631">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"1631">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"1631">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"1631">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Sibir_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"1631">>, <<"mQn">>]
	
	%% 577
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"577">>, <<"state">>]	
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"577">>, <<"U">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"577">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"577">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"577">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"577">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"577">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"577">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"577">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"577">>, <<"mU">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"577">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"577">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"577">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"577">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Sistema-Ural_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"577">>, <<"mQn">>]

	%% 480
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"state_n_ras">>} => 			[<<"nodes">>, <<"480">>, <<"state">>]	
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"u_n_ras">>} => 				[<<"nodes">>, <<"480">>, <<"U">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"u_fi_n_ras">>} => 			[<<"nodes">>, <<"480">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"480">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"480">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"p_load_ras">>} => 			[<<"nodes">>, <<"480">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"q_load_ras">>} => 			[<<"nodes">>, <<"480">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"480">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"480">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"480">>, <<"mU">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"480">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"p_gen_m">>} => 				[<<"nodes">>, <<"480">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"q_gen_m">>} => 				[<<"nodes">>, <<"480">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"480">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"480">>, <<"mQn">>]

	%% 2921
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"2921">>, <<"state">>]	
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"2921">>, <<"U">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"2921">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"2921">>, <<"Pg">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"2921">>, <<"Qg">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"2921">>, <<"Pn">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"2921">>, <<"Qn">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"2921">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"2921">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"2921">>, <<"mU">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"2921">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"2921">>, <<"mPg">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"2921">>, <<"mQg">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"2921">>, <<"mPn">>]
	, {<<"TAGS/Nodes/CA-1_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"2921">>, <<"mQn">>]
	
	%% 51
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"51">>, <<"state">>]	
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"51">>, <<"U">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"51">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"51">>, <<"Pg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"51">>, <<"Qg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"51">>, <<"Pn">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"51">>, <<"Qn">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"51">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"51">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"51">>, <<"mU">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"51">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"51">>, <<"mPg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"51">>, <<"mQg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"51">>, <<"mPn">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl3-8/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"51">>, <<"mQn">>]

	%% 28
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"28">>, <<"state">>]	
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"28">>, <<"U">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"u_fi_n_ras">>} => 			[<<"nodes">>, <<"28">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"28">>, <<"Pg">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"28">>, <<"Qg">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"p_load_ras">>} => 			[<<"nodes">>, <<"28">>, <<"Pn">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"q_load_ras">>} => 			[<<"nodes">>, <<"28">>, <<"Qn">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"28">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"28">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"28">>, <<"mU">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"28">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"28">>, <<"mPg">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"28">>, <<"mQg">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"28">>, <<"mPn">>]
	, {<<"TAGS/Nodes/EGRES-2_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"28">>, <<"mQn">>]

	%% 60
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"60">>, <<"state">>]	
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"60">>, <<"U">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"60">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"60">>, <<"Pg">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"60">>, <<"Qg">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"60">>, <<"Pn">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"60">>, <<"Qn">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"60">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"60">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"60">>, <<"mU">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"60">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"60">>, <<"mPg">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"60">>, <<"mQg">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"60">>, <<"mPn">>]
	, {<<"TAGS/Nodes/EGRES-2_F/u20/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"60">>, <<"mQn">>]
	
	%% 466
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"466">>, <<"state">>]	
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"466">>, <<"U">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"466">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"466">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"466">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"466">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"466">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"466">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"466">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"466">>, <<"mU">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"466">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"466">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"466">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"466">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Zhezkazgan_F/u220/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"466">>, <<"mQn">>]

	%% 355
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"355">>, <<"state">>]	
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"355">>, <<"U">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"355">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"355">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"355">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"355">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"355">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"355">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"355">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"355">>, <<"mU">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"355">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"355">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"355">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"355">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Kar.GRES-2_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"355">>, <<"mQn">>]

	%% 912
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"912">>, <<"state">>]	
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"912">>, <<"U">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"912">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"912">>, <<"Pg">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"912">>, <<"Qg">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"912">>, <<"Pn">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"912">>, <<"Qn">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"912">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"912">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"912">>, <<"mU">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"912">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"912">>, <<"mPg">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"912">>, <<"mQg">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"912">>, <<"mPn">>]
	, {<<"TAGS/Nodes/PS7-AHBK_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"912">>, <<"mQn">>]

	%% 904
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"904">>, <<"state">>]	
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"904">>, <<"U">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"u_fi_n_ras">>} => 			[<<"nodes">>, <<"904">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"904">>, <<"Pg">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"904">>, <<"Qg">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"p_load_ras">>} => 			[<<"nodes">>, <<"904">>, <<"Pn">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"q_load_ras">>} => 			[<<"nodes">>, <<"904">>, <<"Qn">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"904">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"904">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"904">>, <<"mU">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"904">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"904">>, <<"mPg">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"904">>, <<"mQg">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"904">>, <<"mPn">>]
	, {<<"TAGS/Nodes/ATETS-3_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"904">>, <<"mQn">>]
	
	%% 907
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"907">>, <<"state">>]	
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"907">>, <<"U">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"907">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"907">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"907">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"907">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"907">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"907">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"907">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"907">>, <<"mU">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"907">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"907">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"907">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"907">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Robot_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"907">>, <<"mQn">>]

	%% 475
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"475">>, <<"state">>]	
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"475">>, <<"U">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"475">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"475">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"475">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"475">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"475">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"475">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"475">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"475">>, <<"mU">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"475">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"475">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"475">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"475">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Mointy_220_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"475">>, <<"mQn">>]


	%% 473
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"473">>, <<"state">>]	
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"473">>, <<"U">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"473">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"473">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"473">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"473">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"473">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"473">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"473">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"473">>, <<"mU">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"473">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"473">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"473">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"473">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Balkhashskaya_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"473">>, <<"mQn">>]
	
	%% 50
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"50">>, <<"state">>]	
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"50">>, <<"U">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"u_fi_n_ras">>} => 			[<<"nodes">>, <<"50">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"50">>, <<"Pg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"50">>, <<"Qg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"p_load_ras">>} => 			[<<"nodes">>, <<"50">>, <<"Pn">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"q_load_ras">>} => 			[<<"nodes">>, <<"50">>, <<"Qn">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"50">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"50">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"50">>, <<"mU">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"50">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"50">>, <<"mPg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"50">>, <<"mQg">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"50">>, <<"mPn">>]
	, {<<"TAGS/Nodes/EGRES-1_F/bl2/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"50">>, <<"mQn">>]

	%% 465
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"465">>, <<"state">>]	
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"465">>, <<"U">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"465">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"465">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"465">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"465">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"465">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"465">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"465">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"465">>, <<"mU">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"465">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"465">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"465">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"465">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Karazhal_220_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"465">>, <<"mQn">>]

	%% 63
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"63">>, <<"state">>]	
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"63">>, <<"U">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"63">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"63">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"63">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"63">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"63">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"63">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"63">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"63">>, <<"mU">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"63">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"63">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"63">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"63">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Tashkent_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"63">>, <<"mQn">>]

	
	%% 147
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"147">>, <<"state">>]	
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"147">>, <<"U">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"147">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"147">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"147">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"147">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"147">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"147">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"147">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"147">>, <<"mU">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"147">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"147">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"147">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"147">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Avrora_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"147">>, <<"mQn">>]

	%% 1817
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"1817">>, <<"state">>]	
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"1817">>, <<"U">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"1817">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"1817">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"1817">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"1817">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"1817">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"1817">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"1817">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"1817">>, <<"mU">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"1817">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"1817">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"1817">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"1817">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Tavricheskaya_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"1817">>, <<"mQn">>]

	%% 1850
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"1850">>, <<"state">>]	
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"1850">>, <<"U">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"u_fi_n_ras">>} => 			[<<"nodes">>, <<"1850">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"1850">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"1850">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"p_load_ras">>} => 			[<<"nodes">>, <<"1850">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"q_load_ras">>} => 			[<<"nodes">>, <<"1850">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"1850">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"1850">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"1850">>, <<"mU">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"1850">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"1850">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"1850">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"1850">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Irtyshskaya_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"1850">>, <<"mQn">>]
	
	%% 30
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"30">>, <<"state">>]	
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"30">>, <<"U">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"30">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"30">>, <<"Pg">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"30">>, <<"Qg">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"30">>, <<"Pn">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"30">>, <<"Qn">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"30">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"30">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"30">>, <<"mU">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"30">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"30">>, <<"mPg">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"30">>, <<"mQg">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"30">>, <<"mPn">>]
	, {<<"TAGS/Nodes/EEC_F/bl5-8/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"30">>, <<"mQn">>]

	%% 7201
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7201">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7201">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7201">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"7201">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"7201">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7201">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7201">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7201">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7201">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"7201">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7201">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7201">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7201">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7201">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Semey_U-K_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7201">>, <<"mQn">>]


	%% 7202
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7202">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7202">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7202">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"7202">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"7202">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7202">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7202">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7202">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7202">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"7202">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7202">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7202">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7202">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7202">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_EEC_Semey_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7202">>, <<"mQn">>]


	%% 7203
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7203">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7203">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7203">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7203">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7203">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7203">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7203">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"7203">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"7203">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7203">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"7203">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7203">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7203">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"7203">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_EEC_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"7203">>, <<"mQn">>]

	
	%% 7204
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7204">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7204">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"u_fi_n_ras">>} => 			[<<"nodes">>, <<"7204">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"7204">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"7204">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"p_load_ras">>} => 			[<<"nodes">>, <<"7204">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"q_load_ras">>} => 			[<<"nodes">>, <<"7204">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7204">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7204">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"7204">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7204">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7204">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7204">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7204">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_CGPP_EGRES-1_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7204">>, <<"mQn">>]


	%% 7205
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7205">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7205">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7205">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"p_gen_ras">>} =>	 		[<<"nodes">>, <<"7205">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7205">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7205">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7205">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7205">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7205">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7205">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7205">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"p_gen_m">>} =>	 		[<<"nodes">>, <<"7205">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7205">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7205">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_CGPP_Oskarovka_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7205">>, <<"mQn">>]


	%% 7206
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7206">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7206">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7206">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"p_gen_ras">>} =>	 		[<<"nodes">>, <<"7206">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"7206">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7206">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7206">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7206">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7206">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"7206">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7206">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"p_gen_m">>} =>	 		[<<"nodes">>, <<"7206">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7206">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7206">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_EGRES-1_Oskarovka_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7206">>, <<"mQn">>]
	

	%% 7208
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"7208">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"7208">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7208">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7208">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7208">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7208">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7208">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"7208">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"7208">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7208">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"7208">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"7208">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"7208">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"7208">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Nura_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"7208">>, <<"mQn">>]
	

	%% 7209
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"7209">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"7209">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7209">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7209">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7209">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7209">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7209">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"7209">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"7209">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7209">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"7209">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"7209">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"7209">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"7209">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Karazhal_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"7209">>, <<"mQn">>]
	

	%% 7210
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7210">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7210">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7210">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"p_gen_ras">>} => 	    [<<"nodes">>, <<"7210">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"q_gen_ras">>} => 	    [<<"nodes">>, <<"7210">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7210">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7210">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7210">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7210">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7210">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7210">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"p_gen_m">>} => 	   		[<<"nodes">>, <<"7210">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"q_gen_m">>} => 	   	 	[<<"nodes">>, <<"7210">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7210">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Agadyr_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7210">>, <<"mQn">>]
	

	%% 7211
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7211">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7211">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7211">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"7211">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"7211">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7211">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7211">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7211">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7211">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"7211">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7211">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7211">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7211">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7211">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Balkhashskaya_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7211">>, <<"mQn">>]
	

	%% 7212
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7212">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7212">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7212">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7212">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7212">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7212">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7212">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7212">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7212">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7212">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7212">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7212">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7212">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7212">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_UkGRES_Shu_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7212">>, <<"mQn">>]

	
	%% 7213
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7213">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7213">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7213">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7213">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7213">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7213">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7213">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"7213">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"7213">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7213">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"7213">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7213">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7213">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"7213">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Almaty_Alma_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"7213">>, <<"mQn">>]
	


	%% 7214
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7214">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7214">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7214">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7214">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7214">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7214">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7214">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7214">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7214">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7214">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7214">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7214">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7214">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7214">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Kemin_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7214">>, <<"mQn">>]
	

	%% 7215
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"7215">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"7215">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7215">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7215">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7215">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7215">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7215">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"7215">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"7215">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7215">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"u_fi_n_m">>} =>		[<<"nodes">>, <<"7215">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"7215">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"7215">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"p_load_m">>} =>	 	[<<"nodes">>, <<"7215">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Frunze_Kemin_F/ntag">>, <<"q_load_m">>} =>	 	[<<"nodes">>, <<"7215">>, <<"mQn">>]
	

	%% 7216
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"7216">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"7216">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7216">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7216">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7216">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7216">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7216">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"7216">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"7216">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7216">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"7216">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"7216">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"7216">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"7216">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Zhambyl_Shymkent_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"7216">>, <<"mQn">>]
	
	
	%% 7217
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7217">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7217">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7217">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7217">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7217">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7217">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7217">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"7217">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"7217">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7217">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"7217">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7217">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7217">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"7217">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Glavnaya_Frunze_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"7217">>, <<"mQn">>]


	%% 7218
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7218">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7218">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7218">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"7218">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"7218">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7218">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7218">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7218">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7218">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"7218">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7218">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7218">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7218">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7218">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Taldykorgan_Robot_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7218">>, <<"mQn">>]


	%% 7219
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7219">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7219">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7219">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"7219">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"7219">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7219">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7219">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7219">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7219">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"7219">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7219">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7219">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7219">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7219">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Mointy_UkGRES_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7219">>, <<"mQn">>]
	

	%% 7221
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7221">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7221">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7221">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7221">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7221">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7221">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7221">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"7221">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"7221">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7221">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"7221">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7221">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7221">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"7221">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Zhezkazgan_Karazhal_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"7221">>, <<"mQn">>]


	%% 842
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"842">>, <<"state">>]	
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"842">>, <<"U">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"u_fi_n_ras">>} => 			[<<"nodes">>, <<"842">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"842">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"842">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"p_load_ras">>} => 			[<<"nodes">>, <<"842">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"q_load_ras">>} => 			[<<"nodes">>, <<"842">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"842">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"842">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"842">>, <<"mU">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"842">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"842">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"842">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"842">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Mirgalimsai_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"842">>, <<"mQn">>]
	
	%% 840
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"840">>, <<"state">>]	
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"840">>, <<"U">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"840">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"840">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"840">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"840">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"840">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"840">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"840">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"840">>, <<"mU">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"840">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"840">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"840">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"840">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Kentau_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"840">>, <<"mQn">>]

	%% 869
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"869">>, <<"state">>]	
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"869">>, <<"U">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"869">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"869">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"869">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"869">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"869">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"869">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"869">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"869">>, <<"mU">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"869">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"869">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"869">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"869">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Ru-6_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"869">>, <<"mQn">>]

	%% 862
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"862">>, <<"state">>]	
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"862">>, <<"U">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"862">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"862">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"862">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"862">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"862">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"q_genmax_ras">>} =>	 	[<<"nodes">>, <<"862">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"862">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"862">>, <<"mU">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"862">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"862">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"862">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"862">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Kyzylorda_220_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"862">>, <<"mQn">>]
	
	%% 467
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"467">>, <<"state">>]	
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"467">>, <<"U">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"467">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"467">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"467">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"467">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"467">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"467">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"467">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"467">>, <<"mU">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"467">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"467">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"467">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"467">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Kumkol_220_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"467">>, <<"mQn">>]

	%% 928
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"928">>, <<"state">>]	
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"928">>, <<"U">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"928">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"928">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"928">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"928">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"928">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"928">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"928">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"928">>, <<"mU">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"928">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"928">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"928">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"928">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Medeo_220_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"928">>, <<"mQn">>]

	
	%% 953
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"953">>, <<"state">>]	
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"953">>, <<"U">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"u_fi_n_ras">>} => 	[<<"nodes">>, <<"953">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"p_gen_ras">>} => 	[<<"nodes">>, <<"953">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"q_gen_ras">>} => 	[<<"nodes">>, <<"953">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"p_load_ras">>} => 	[<<"nodes">>, <<"953">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"q_load_ras">>} => 	[<<"nodes">>, <<"953">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"953">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"953">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"u_n_m">>} => 		[<<"nodes">>, <<"953">>, <<"mU">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"953">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"953">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"953">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"953">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Koyan-Kos_220_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"953">>, <<"mQn">>]


	%% 7227
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"7227">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"7227">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7227">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7227">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7227">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7227">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7227">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"7227">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"7227">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7227">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"7227">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"7227">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"7227">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"7227">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Kyzylorda_Kumkol_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"7227">>, <<"mQn">>]

	
	%% 805
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"805">>, <<"state">>]	
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"805">>, <<"U">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"805">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"805">>, <<"Pg">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"805">>, <<"Qg">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"805">>, <<"Pn">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"805">>, <<"Qn">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"805">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"805">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"805">>, <<"mU">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"805">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"805">>, <<"mPg">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"805">>, <<"mQg">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"805">>, <<"mPn">>]
	, {<<"TAGS/Nodes/ZhGRES_220_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"805">>, <<"mQn">>]

	%% 814
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"814">>, <<"state">>]	
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"814">>, <<"U">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"814">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"814">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"814">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"814">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"814">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"814">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"814">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"814">>, <<"mU">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"814">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"814">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"814">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"814">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Aspara_220_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"814">>, <<"mQn">>]


	%% 7223
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7223">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7223">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7223">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"7223">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"q_gen_ras">>} => 			[<<"nodes">>, <<"7223">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7223">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7223">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7223">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7223">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"7223">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7223">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7223">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7223">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7223">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_ZhGRES_Frunze_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7223">>, <<"mQn">>]

	
	%% 839
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"839">>, <<"state">>]	
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"839">>, <<"U">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"839">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"839">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"839">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"839">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"839">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"839">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"839">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"839">>, <<"mU">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"839">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"839">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"839">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"839">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Zhylga_220_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"839">>, <<"mQn">>]


	%% 843
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"843">>, <<"state">>]	
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"843">>, <<"U">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"843">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"843">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"843">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"843">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"843">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"843">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"843">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"843">>, <<"mU">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"843">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"843">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"843">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"843">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Shymkentskaya_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"843">>, <<"mQn">>]

	%% 7224
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7224">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7224">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7224">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7224">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7224">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7224">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7224">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7224">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7224">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7224">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7224">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7224">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7224">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7224">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Kentau_Zhambyl_220_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7224">>, <<"mQn">>]

	
	%% 865
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"865">>, <<"state">>]	
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"865">>, <<"U">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"u_fi_n_ras">>} =>			[<<"nodes">>, <<"865">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"p_gen_ras">>} => 			[<<"nodes">>, <<"865">>, <<"Pg">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"q_gen_ras">>} =>			[<<"nodes">>, <<"865">>, <<"Qg">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"p_load_ras">>} => 			[<<"nodes">>, <<"865">>, <<"Pn">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"q_load_ras">>} => 			[<<"nodes">>, <<"865">>, <<"Qn">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"865">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"865">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"u_n_m">>} => 				[<<"nodes">>, <<"865">>, <<"mU">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"u_fi_n_m">>} =>			[<<"nodes">>, <<"865">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"865">>, <<"mPg">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"q_gen_m">>} =>				[<<"nodes">>, <<"865">>, <<"mQg">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"865">>, <<"mPn">>]
	, {<<"TAGS/Nodes/Zhanakorgan_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"865">>, <<"mQn">>]

	%% 7225
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7225">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7225">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7225">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7225">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7225">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7225">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7225">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"7225">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"7225">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7225">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"7225">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7225">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7225">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"7225">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Shymkent_220_Zhylga_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"7225">>, <<"mQn">>]

	
	%% 7226
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7226">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7226">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7226">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7226">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7226">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7226">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7226">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7226">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7226">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7226">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7226">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7226">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7226">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7226">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_Ru-6_Kyzylorda_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7226">>, <<"mQn">>]

	
	%% 7228
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"state_n_ras">>} => 		[<<"nodes">>, <<"7228">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"u_n_ras">>} => 			[<<"nodes">>, <<"7228">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"u_fi_n_ras">>} => 		[<<"nodes">>, <<"7228">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7228">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7228">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"p_load_ras">>} => 		[<<"nodes">>, <<"7228">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"q_load_ras">>} => 		[<<"nodes">>, <<"7228">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"q_genmax_ras">>} => 		[<<"nodes">>, <<"7228">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"q_genmin_ras">>} => 		[<<"nodes">>, <<"7228">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7228">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"u_fi_n_m">>} => 			[<<"nodes">>, <<"7228">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"p_gen_m">>} => 			[<<"nodes">>, <<"7228">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"q_gen_m">>} => 			[<<"nodes">>, <<"7228">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"p_load_m">>} => 			[<<"nodes">>, <<"7228">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_GNPS_F/ntag">>, <<"q_load_m">>} => 			[<<"nodes">>, <<"7228">>, <<"mQn">>]
	
	
	
	
	% 7229
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"state_n_ras">>} => 	[<<"nodes">>, <<"7229">>, <<"state">>]	
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"u_n_ras">>} => 		[<<"nodes">>, <<"7229">>, <<"U">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"u_fi_n_ras">>} => 	[<<"nodes">>, <<"7229">>, <<"Ufi">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"p_gen_ras">>} => 		[<<"nodes">>, <<"7229">>, <<"Pg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"q_gen_ras">>} => 		[<<"nodes">>, <<"7229">>, <<"Qg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"p_load_ras">>} => 	[<<"nodes">>, <<"7229">>, <<"Pn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"q_load_ras">>} => 	[<<"nodes">>, <<"7229">>, <<"Qn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"q_genmax_ras">>} => 	[<<"nodes">>, <<"7229">>, <<"Qmax">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"q_genmin_ras">>} => 	[<<"nodes">>, <<"7229">>, <<"Qmin">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"u_n_m">>} => 			[<<"nodes">>, <<"7229">>, <<"mU">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"u_fi_n_m">>} => 		[<<"nodes">>, <<"7229">>, <<"mUfi">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"p_gen_m">>} => 		[<<"nodes">>, <<"7229">>, <<"mPg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"q_gen_m">>} => 		[<<"nodes">>, <<"7229">>, <<"mQg">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"p_load_m">>} => 		[<<"nodes">>, <<"7229">>, <<"mPn">>]
	, {<<"TAGS/Nodes/pu_KarGRES_Oskarovka_F/ntag">>, <<"q_load_m">>} => 		[<<"nodes">>, <<"7229">>, <<"mQn">>]
	
	
%% NODES end ---------------------------------------------------------------------------------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
%% LINKSREACT begin  ---------------------------------------------------------------------------------------------------------------------------------------------------

% 	%% _______
% 	, {<<"TAGS/Reactors/_______/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"_______">>, <<"state">>]
% 	, {<<"TAGS/Reactors/_______/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"_______">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/_______/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"_______">>, <<"weight">>]


%	%% 26_1660_3_4	%% Izmenit' v FP! 26_1660_3_3
%	, {<<"TAGS/Reactors/26_1660_3_4/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"26_1660_3_4">>, <<"state">>]
%	, {<<"TAGS/Reactors/26_1660_3_4/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"26_1660_3_4">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/26_1660_3_4/rtag">>, <<"wt_reactor_ras">>} => 	[<<"lr">>, <<"26_1660_3_4">>, <<"weight">>]


% 	%% 26_1660_3_5	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/26_1660_3_5/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"26_1660_3_5">>, <<"state">>]
% 	, {<<"TAGS/Reactors/26_1660_3_5/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"26_1660_3_5">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/26_1660_3_5/rtag">>, <<"wt_reactor_ras">>} => 	[<<"lr">>, <<"26_1660_3_5">>, <<"weight">>]


% 	%% 26_469_3_3	%% Izmenit' v FP! 26_469_3_5170
% 	, {<<"TAGS/Reactors/26_469_3_3/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"26_469_3_3">>, <<"state">>]
% 	, {<<"TAGS/Reactors/26_469_3_3/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"26_469_3_3">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/26_469_3_3/rtag">>, <<"wt_reactor_ras">>} => 		[<<"lr">>, <<"26_469_3_3">>, <<"weight">>]


% 	%% 26_980_3_2	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/26_980_3_2/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"26_980_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/26_980_3_2/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"26_980_3_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/26_980_3_2/rtag">>, <<"wt_reactor_ras">>} =>	 	[<<"lr">>, <<"26_980_3_2">>, <<"weight">>]


% 	%% 26_175_3_6	%% Izmenit' v FP! 26_175_3_1
% 	, {<<"TAGS/Reactors/26_175_3_6/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"26_175_3_6">>, <<"state">>]
% 	, {<<"TAGS/Reactors/26_175_3_6/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"26_175_3_6">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/26_175_3_6/rtag">>, <<"wt_reactor_ras">>} => 		[<<"lr">>, <<"26_175_3_6">>, <<"weight">>]


% 	%% 25_129_3_1	%% Izmenit' v FP! 25_129_3_2
% 	, {<<"TAGS/Reactors/25_129_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"25_129_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/25_129_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"25_129_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/25_129_3_1/rtag">>, <<"wt_reactor_ras">>} => 		[<<"lr">>, <<"25_129_3_1">>, <<"weight">>]

% 	%% 25_325_3_2	%% Izmenit' v FP! 25_325_3_1
% 	, {<<"TAGS/Reactors/25_325_3_2/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"25_325_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/25_325_3_2/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"25_325_3_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/25_325_3_2/rtag">>, <<"wt_reactor_ras">>} => 		[<<"lr">>, <<"25_325_3_2">>, <<"weight">>]


% 	%% 25_1817_3_3	%% Izmenit' v FP! 25_1817_3_5577
% 	, {<<"TAGS/Reactors/25_1817_3_3/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"25_1817_3_3">>, <<"state">>]
% 	, {<<"TAGS/Reactors/25_1817_3_3/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"25_1817_3_3">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/25_1817_3_3/rtag">>, <<"wt_reactor_ras">>} => 	[<<"lr">>, <<"25_1817_3_3">>, <<"weight">>]


% 	%% 325_25_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/325_25_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"325_25_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/325_25_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"325_25_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/325_25_3_1/rtag">>, <<"wt_reactor_ras">>} => 	    [<<"lr">>, <<"325_25_3_1">>, <<"weight">>]


% 	%% 469_325_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/469_325_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"469_325_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/469_325_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"469_325_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/469_325_3_1/rtag">>, <<"wt_reactor_ras">>} =>     [<<"lr">>, <<"469_325_3_1">>, <<"weight">>]


% 	%% 469_900_1_5	%% Izmenit' v FP! 469_900_1_3
% 	, {<<"TAGS/Reactors/469_900_1_5/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"469_900_1_5">>, <<"state">>]
% 	, {<<"TAGS/Reactors/469_900_1_5/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"469_900_1_5">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/469_900_1_5/rtag">>, <<"wt_reactor_ras">>} =>     [<<"lr">>, <<"469_900_1_5">>, <<"weight">>]


% 	%% 469_26_3_2	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/469_26_3_2/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"469_26_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/469_26_3_2/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"469_26_3_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/469_26_3_2/rtag">>, <<"wt_reactor_ras">>} =>      [<<"lr">>, <<"469_26_3_2">>, <<"weight">>]


% 	%% 469_26_3_4	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/469_26_3_4/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"469_26_3_4">>, <<"state">>]
% 	, {<<"TAGS/Reactors/469_26_3_4/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"469_26_3_4">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/469_26_3_4/rtag">>, <<"wt_reactor_ras">>} =>      [<<"lr">>, <<"469_26_3_4">>, <<"weight">>]

% 	%% 469_900_2_5	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/469_900_2_5/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"469_900_2_5">>, <<"state">>]
% 	, {<<"TAGS/Reactors/469_900_2_5/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"469_900_2_5">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/469_900_2_5/rtag">>, <<"wt_reactor_ras">>} =>     [<<"lr">>, <<"469_900_2_5">>, <<"weight">>]

% 	%% 900_469_1_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/900_469_1_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"900_469_1_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/900_469_1_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"900_469_1_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/900_469_1_1/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"900_469_1_1 ">>, <<"weight">>]


% 	%% 900_469_2_2	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/900_469_2_2/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"900_469_2_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/900_469_2_2/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"900_469_2_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/900_469_2_2/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"900_469_2_2 ">>, <<"weight">>]


% 	%% 900_902_3_3	%% Izmenit' v FP! 900_902_3_1
% 	, {<<"TAGS/Reactors/900_902_3_3/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"900_902_3_3">>, <<"state">>]
% 	, {<<"TAGS/Reactors/900_902_3_3/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"900_902_3_3">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/900_902_3_3/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"900_902_3_3">>, <<"weight">>]


% 	%% 900_338_3_5	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/900_338_3_5/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"900_338_3_5">>, <<"state">>]
% 	, {<<"TAGS/Reactors/900_338_3_5/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"900_338_3_5">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/900_338_3_5/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"900_338_3_5">>, <<"weight">>]


% 	%% 902_900_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/902_900_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"902_900_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/902_900_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"902_900_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/902_900_3_1/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"902_900_3_1">>, <<"weight">>]


% 	%% 902_9932_3_2
% 	, {<<"TAGS/Reactors/V_902-9932-3-2_F/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"902_9932_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/V_902-9932-3-2_F/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"902_9932_3_2">>, <<"conduct">>]	
% 	, {<<"TAGS/Reactors/V_902-9932-3-2_F/rtag">>, <<"wt_reactor_ras">>} =>	[<<"lr">>, <<"902_9932_3_2">>, <<"weight">>]
	

% 	%% 938_900_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/938_900_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"938_900_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/938_900_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"938_900_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/938_900_3_1/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"938_900_3_1">>, <<"weight">>]


% 	%% 9932_900_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/9932_900_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"9932_900_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/9932_900_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"9932_900_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/9932_900_3_1/rtag">>, <<"wt_reactor_ras">>} =>	[<<"lr">>, <<"9932_900_3_1">>, <<"weight">>]


% 	%% 2919_9932_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/2919_9932_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"2919_9932_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/2919_9932_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"2919_9932_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/2919_9932_3_1/rtag">>, <<"wt_reactor_ras">>} =>	[<<"lr">>, <<"2919_9932_3_1">>, <<"weight">>]


% 	%% 800_2919_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/800_2919_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"800_2919_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/800_2919_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"800_2919_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/800_2919_3_1/rtag">>, <<"wt_reactor_ras">>} =>	[<<"lr">>, <<"800_2919_3_1">>, <<"weight">>]


% 	%% 800_830_3_2
% 	, {<<"TAGS/Reactors/V_800-830-3-2_F/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"800_830_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/V_800-830-3-2_F/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"800_830_3_2">>, <<"conduct">>]	
% 	, {<<"TAGS/Reactors/V_800-830-3-2_F/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"800_830_3_2">>, <<"weight">>]


% 	%% 980_26_3_1
% 	, {<<"TAGS/Reactors/980_26_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"980_26_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/980_26_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"980_26_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/980_26_3_1/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"980_26_3_1">>, <<"weight">>]


% 	%% 980_240_3_2	%% Izmenit' v FP! 980_240_3_5384
% 	, {<<"TAGS/Reactors/980_240_3_2/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"980_240_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/980_240_3_2/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"980_240_3_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/980_240_3_2/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"980_240_3_2">>, <<"weight">>]


% 	%% 986_980_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/986_980_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"986_980_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/986_980_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"986_980_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/986_980_3_1/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"986_980_3_1">>, <<"weight">>]


% 	%% 987_986_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/987_986_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"987_986_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/987_986_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"987_986_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/987_986_3_1/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"987_986_3_1">>, <<"weight">>]


% 	%% 129_180_3_1
% 	, {<<"TAGS/Reactors/V_129-180-3-1_F/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"129_180_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/V_129-180-3-1_F/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"129_180_3_1">>, <<"conduct">>]	
% 	, {<<"TAGS/Reactors/V_129-180-3-1_F/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"129_180_3_1">>, <<"weight">>]


% 	%% 129_25_3_2	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/129_25_3_2/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"129_25_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/129_25_3_2/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"129_25_3_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/129_25_3_2/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"129_25_3_2">>, <<"weight">>]


% 	%% 31_1850_3_2	%% Izmenit' v FP! 31_1850_3_553
% 	, {<<"TAGS/Reactors/31_1850_3_2/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"31_1850_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/31_1850_3_2/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"31_1850_3_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/31_1850_3_2/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"31_1850_3_2">>, <<"weight">>]


% 	%% 1621_31_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/1621_31_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"1621_31_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/1621_31_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"1621_31_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/1621_31_3_1/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"1621_31_3_1">>, <<"weight">>]


% 	%% 1660_26_3_3	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/1660_26_3_3/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"1660_26_3_3">>, <<"state">>]
% 	, {<<"TAGS/Reactors/1660_26_3_3/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"1660_26_3_3">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/1660_26_3_3/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"1660_26_3_3">>, <<"weight">>]


% 	%% 1660_26_3_4	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/1660_26_3_4/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"1660_26_3_4">>, <<"state">>]
% 	, {<<"TAGS/Reactors/1660_26_3_4/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"1660_26_3_4">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/1660_26_3_4/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"1660_26_3_4">>, <<"weight">>]


% 	%% 1630_1660_1_2	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/1630_1660_1_2/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"1630_1660_1_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/1630_1660_1_2/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"1630_1660_1_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/1630_1660_1_2/rtag">>, <<"wt_reactor_ras">>} =>	[<<"lr">>, <<"1630_1660_1_2">>, <<"weight">>]


% 	%% 1630_1660_1_3	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/1630_1660_1_3/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"1630_1660_1_3">>, <<"state">>]
% 	, {<<"TAGS/Reactors/1630_1660_1_3/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"1630_1660_1_3">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/1630_1660_1_3/rtag">>, <<"wt_reactor_ras">>} =>	[<<"lr">>, <<"1630_1660_1_3">>, <<"weight">>]


% 	%% 175_26_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/175_26_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"175_26_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/175_26_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"175_26_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/175_26_3_1/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"175_26_3_1">>, <<"weight">>]


% 	%% 175_26_3_2	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/175_26_3_2/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"175_26_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/175_26_3_2/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"175_26_3_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/175_26_3_2/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"175_26_3_2">>, <<"weight">>]


% 	%% 175_590_3_3
% 	, {<<"TAGS/Reactors/V_175-590-3-3_F/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"175_590_3_3">>, <<"state">>]
% 	, {<<"TAGS/Reactors/V_175-590-3-3_F/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"175_590_3_3">>, <<"conduct">>]	
% 	, {<<"TAGS/Reactors/V_175-590-3-3_F/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"175_590_3_3">>, <<"weight">>]
	

% 	%% 175_590_3_4	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/175_590_3_4/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"175_590_3_4">>, <<"state">>]
% 	, {<<"TAGS/Reactors/175_590_3_4/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"175_590_3_4">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/175_590_3_4/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"175_590_3_4">>, <<"weight">>]


% 	%% 175_590_3_5	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/175_590_3_5/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"175_590_3_5">>, <<"state">>]
% 	, {<<"TAGS/Reactors/175_590_3_5/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"175_590_3_5">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/175_590_3_5/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"175_590_3_5">>, <<"weight">>]


% 	%% 175_26_3_6	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/175_26_3_6/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"175_26_3_6">>, <<"state">>]
% 	, {<<"TAGS/Reactors/175_26_3_6/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"175_26_3_6">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/175_26_3_6/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"175_26_3_6">>, <<"weight">>]


 	%% 590_175_3_2	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/590_175_3_2/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"590_175_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/590_175_3_2/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"590_175_3_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/590_175_3_2/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"590_175_3_2">>, <<"weight">>]


 	%% 576_577_3_1
% 	, {<<"TAGS/Reactors/V_576-577-3-1_F/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"576_577_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/V_576-577-3-1_F/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"576_577_3_1">>, <<"conduct">>]	
% 	, {<<"TAGS/Reactors/V_576-577-3-1_F/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"576_577_3_1">>, <<"weight">>]

	
 	%% 576_180_3_2	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/576_180_3_2/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"576_180_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/576_180_3_2/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"576_180_3_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/576_180_3_2/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"576_180_3_2">>, <<"weight">>]


 	%% 180_129_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/180_129_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"180_129_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/180_129_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"180_129_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/180_129_3_1/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"180_129_3_1">>, <<"weight">>]


 	%% 180_576_3_2
% 	, {<<"TAGS/Reactors/V_180-576-3-2_F/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"180_576_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/V_180-576-3-2_F/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"180_576_3_2">>, <<"conduct">>]	
% 	, {<<"TAGS/Reactors/V_180-576-3-2_F/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"180_576_3_2">>, <<"weight">>]


 	%% 480_469_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/480_469_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"480_469_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/480_469_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"480_469_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/480_469_3_1/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"480_469_3_1">>, <<"weight">>]


 	%% 2921_2919_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/2921_2919_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"2921_2919_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/2921_2919_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"2921_2919_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/2921_2919_3_1/rtag">>, <<"wt_reactor_ras">>} =>	[<<"lr">>, <<"2921_2919_3_1">>, <<"weight">>]


 	%% 147_175_3_2	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/147_175_3_2/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"147_175_3_2">>, <<"state">>]
% 	, {<<"TAGS/Reactors/147_175_3_2/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"147_175_3_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/147_175_3_2/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"147_175_3_2">>, <<"weight">>]


	%% 147_1817_3_1
% 	, {<<"TAGS/Reactors/V_147-1817-3-1_F/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"147_1817_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/V_147-1817-3-1_F/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"147_1817_3_1">>, <<"conduct">>]	
% 	, {<<"TAGS/Reactors/V_147-1817-3-1_F/rtag">>, <<"wt_reactor_ras">>} =>	[<<"lr">>, <<"147_1817_3_1">>, <<"weight">>]


	%% 31_25_3_1	%% Dobavit' v FP!
% 	, {<<"TAGS/Reactors/31_25_3_1/rtag">>, <<"state_r_ras">>} => 		[<<"lr">>, <<"31_25_3_1">>, <<"state">>]
% 	, {<<"TAGS/Reactors/31_25_3_1/rtag">>, <<"b_r_ras">>} => 			[<<"lr">>, <<"31_25_3_1">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/31_25_3_1/rtag">>, <<"wt_reactor_ras">>} =>		[<<"lr">>, <<"31_25_3_1">>, <<"weight">>]



%% LINKSREACT end  ---------------------------------------------------------------------------------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% NODESREACT begin  ---------------------------------------------------------------------------------------------------------------------------------------------------

%% 26_1
%	, {<<"TAGS/Reactors/N_26-1_F/rtag">>, <<"state_r_ras">>} => 		[<<"nr">>, <<"26_1">>, <<"state">>]	
%	, {<<"TAGS/Reactors/N_26-1_F/rtag">>, <<"b_r_ras">>} => 			[<<"nr">>, <<"26_1">>, <<"conduct">>]	
% 	, {<<"TAGS/Reactors/N_26-1_F/rtag">>, <<"wt_reactor_ras">>} => 	[<<"nr">>, <<"26_1">>, <<"weight">>]



%% 9932_2
% 	, {<<"TAGS/Reactors/N_9932-2_F/rtag">>, <<"state_r_ras">>} => 	[<<"nr">>, <<"9932_2">>, <<"state">>]	
%	, {<<"TAGS/Reactors/N_9932-2_F/rtag">>, <<"b_r_ras">>} => 		[<<"nr">>, <<"9932_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/N_9932-2_F/rtag">>, <<"wt_reactor_ras">>} => 	[<<"nr">>, <<"9932_2">>, <<"weight">>]


%% 2919_2
%	, {<<"TAGS/Reactors/N_2919-2_F/rtag">>, <<"state_r_ras">>} => 	[<<"nr">>, <<"2919_2">>, <<"state">>]	
%	, {<<"TAGS/Reactors/N_2919-2_F/rtag">>, <<"b_r_ras">>} => 		[<<"nr">>, <<"2919_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/N_2919-2_F/rtag">>, <<"wt_reactor_ras">>} => 	[<<"nr">>, <<"2919_2">>, <<"weight">>]



%% 1621_2
%	, {<<"TAGS/Reactors/N_1621-2_F/rtag">>, <<"state_r_ras">>} => 		[<<"nr">>, <<"1621_2">>, <<"state">>]	
%	, {<<"TAGS/Reactors/N_1621-2_F/rtag">>, <<"b_r_ras">>} => 			[<<"nr">>, <<"1621_2">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/N_1621-2_F/rtag">>, <<"wt_reactor_ras">>} => 		[<<"nr">>, <<"1621_2">>, <<"weight">>]



%% 180_3
%	, {<<"TAGS/Reactors/N_180-3_F/rtag">>, <<"state_r_ras">>} => 		[<<"nr">>, <<"180_3">>, <<"state">>]	
%	, {<<"TAGS/Reactors/N_180-3_F/rtag">>, <<"b_r_ras">>} => 			[<<"nr">>, <<"180_3">>, <<"conduct">>]	
%	, {<<"TAGS/Reactors/N_180-3_F/rtag">>, <<"wt_reactor_ras">>} => 	[<<"nr">>, <<"180_3">>, <<"weight">>]



%% 1631_1 %% Izmenit' v FP! 1631_532
% 	, {<<"TAGS/Reactors/N_1631-532_F/rtag">>, <<"state_r_ras">>} => 		[<<"nr">>, <<"1631_1">>, <<"state">>]	
% 	, {<<"TAGS/Reactors/N_1631-532_F/rtag">>, <<"b_r_ras">>} => 			[<<"nr">>, <<"1631_1">>, <<"conduct">>]	
% 	, {<<"TAGS/Reactors/N_1631-532_F/rtag">>, <<"wt_reactor_ras">>} => 	[<<"nr">>, <<"1631_1">>, <<"weight">>]



%% 1817_1
% 	, {<<"TAGS/Reactors/N_1817-1_F/rtag">>, <<"state_r_ras">>} => 		[<<"nr">>, <<"1817_1">>, <<"state">>]	
% 	, {<<"TAGS/Reactors/N_1817-1_F/rtag">>, <<"b_r_ras">>} => 			[<<"nr">>, <<"1817_1">>, <<"conduct">>]	
% 	, {<<"TAGS/Reactors/N_1817-1_F/rtag">>, <<"wt_reactor_ras">>} => 		[<<"nr">>, <<"1817_1">>, <<"weight">>]



%% 1850_1
% 	, {<<"TAGS/Reactors/N_1850-1_F/rtag">>, <<"state_r_ras">>} => 		[<<"nr">>, <<"1850_1">>, <<"state">>]	
% 	, {<<"TAGS/Reactors/N_1850-1_F/rtag">>, <<"b_r_ras">>} => 			[<<"nr">>, <<"1850_1">>, <<"conduct">>]	
% 	, {<<"TAGS/Reactors/N_1850-1_F/rtag">>, <<"wt_reactor_ras">>} => 		[<<"nr">>, <<"1850_1">>, <<"weight">>]
		


%% 1817_2 %% Izmenit' v FP!  1817_1
% 	, {<<"TAGS/Reactors/N_1817-1_F/rtag">>, <<"state_r_ras">>} => 		[<<"nr">>, <<"1817_2">>, <<"state">>]	
% 	, {<<"TAGS/Reactors/N_1817-1_F/rtag">>, <<"b_r_ras">>} => 			[<<"nr">>, <<"1817_2">>, <<"conduct">>]	
% 	, {<<"TAGS/Reactors/N_1817-1_F/rtag">>, <<"wt_reactor_ras">>} => 		[<<"nr">>, <<"1817_2">>, <<"weight">>]	

%% NODESREACT end  --------------------------------------------------------------------------------------------------------------------------------------------

      
  }.

%d260921_t2200_aza 
