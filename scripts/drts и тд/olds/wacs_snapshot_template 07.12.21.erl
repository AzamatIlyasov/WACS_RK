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

-module(wacs_snapshot_template).

-include("fp_struct.hrl").

-export([on_event/2]).

-export([
    template/0, 
    csv_template/1
    ]).

on_event(_, State)->
  % here should be your code that is executed each Cycle milliseconds
  State.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  CONFIGURATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
template()->
  #{
    %----------------------------------------------------------------------------------
    %     NODES (УЗЛЫ)
    %----------------------------------------------------------------------------------
    "nodes" => #{

      %	"__" => #{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %		"num" => fun(_TS)-> 	__ 		end,
      %		"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/_________/ntag">>, 	<<"state_n">> ) 			end,
      %		"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/_________/ntag">>, 	<<"u_nom_n">> ) 				end,
      %		"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/_________/ntag">>, 	<<"u_fi_n">> ) 	end,
      %		"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/_________/futureP/predict_p_load">>, TS )		end,
      %		"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/_________/futureP/predict_q_load">>, TS )		end,
      %		"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/_________/futureP/predict_p_gen">>, TS )		end,
      %		"Qg" => fun(_TS)-> 		wacs_snapshot:read_archive( <<"/_________/futureP/predict_q_gen">>, TS )		end,
      %		"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/_________/ntag">>, 	<<"q_genmin">> ) 		end,
      %		"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/_________/ntag">>, 	<<"q_genmax">> ) 		end
      %	},

 
		"26" => #{ 
		"num" => fun(_TS)-> 	26 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/EK-1150_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/EK-1150_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EK-1150_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EK-1150_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EK-1150_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EK-1150_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EK-1150_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EK-1150_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EK-1150_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"25" => #{ 
		"num" => fun(_TS)-> 	25 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/EGRES-1_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/EGRES-1_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-1_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-1_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-1_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"325" => #{
		"num" => fun(_TS)-> 	325 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Nura_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Nura_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Nura_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Nura_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Nura_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Nura_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Nura_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Nura_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Nura_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"469" => #{
		"num" => fun(_TS)-> 	469 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Agadyr_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Agadyr_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Agadyr_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Agadyr_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Agadyr_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Agadyr_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Agadyr_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Agadyr_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Agadyr_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "900" =>  #{ 
		"num" => fun(_TS)-> 	900 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/UKGRES_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/UKGRES_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/UKGRES_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/UKGRES_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/UKGRES_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/UKGRES_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/UKGRES_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/UKGRES_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/UKGRES_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"902" => #{
		"num" => fun(_TS)-> 	902 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Almaty_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Almaty_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Almaty_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Almaty_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Almaty_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Almaty_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Almaty_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Almaty_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Almaty_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"938" => #{
		"num" => fun(_TS)-> 	938		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Alma_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Alma_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Alma_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Alma_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Alma_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Alma_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Alma_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Alma_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Alma_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"9932" =>  #{ 
		"num" => fun(_TS)-> 	9932 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Shu_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Shu_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shu_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shu_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shu_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shu_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shu_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shu_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shu_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},



		"2919" =>  #{ 
		"num" => fun(_TS)-> 	2919 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Frunze_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Frunze_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Frunze_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Frunze_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Frunze_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Frunze_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Frunze_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Frunze_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Frunze_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "800" =>  #{ 
		"num" => fun(_TS)-> 	    800 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Zhambyl_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Zhambyl_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhambyl_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhambyl_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhambyl_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhambyl_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhambyl_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhambyl_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhambyl_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"830" =>  #{ 
		"num" => fun(_TS)-> 	830 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Shymkent_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Shymkent_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shymkent_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shymkent_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shymkent_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shymkent_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shymkent_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shymkent_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shymkent_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


	    "980" =>  #{ 
		"num" => fun(_TS)->   	980 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Semey_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Semey_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Semey_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Semey_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Semey_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Semey_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Semey_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Semey_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Semey_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},



		"986" =>  #{ 
		"num" => fun(_TS)->  	986 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Aktogay_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Aktogay_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Aktogay_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Aktogay_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Aktogay_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Aktogay_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Aktogay_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Aktogay_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Aktogay_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"987" =>  #{ 
		"num" => fun(_TS)->  	987 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Taldykorgan_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Taldykorgan_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Taldykorgan_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Taldykorgan_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Taldykorgan_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Taldykorgan_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Taldykorgan_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Taldykorgan_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Taldykorgan_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"240" => #{ 
		"num" => fun(_TS)->  	240 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Usti-Kamenogorsk_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Usti-Kamenogorsk_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Usti-Kamenogorsk_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Usti-Kamenogorsk_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Usti-Kamenogorsk_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Usti-Kamenogorsk_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Usti-Kamenogorsk_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Usti-Kamenogorsk_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Usti-Kamenogorsk_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"224" => #{ 
		"num" => fun(_TS)->  	224 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/TashGRES_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/TashGRES_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/TashGRES_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/TashGRES_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/TashGRES_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/TashGRES_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/TashGRES_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/TashGRES_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/TashGRES_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "129" => #{ 
		"num" => fun(_TS)->   	129 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/CGPP_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/CGPP_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/CGPP_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/CGPP_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/CGPP_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/CGPP_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/CGPP_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/CGPP_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/CGPP_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "31" =>  #{ 
		"num" => fun(_TS)->  	31 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/EEC_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/EEC_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EEC_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EEC_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EEC_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EEC_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EEC_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EEC_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EEC_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"1621" => #{ 
		"num" => fun(_TS)->  	1621		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Rubtsovsk_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Rubtsovsk_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Rubtsovsk_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Rubtsovsk_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Rubtsovsk_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Rubtsovsk_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Rubtsovsk_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Rubtsovsk_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Rubtsovsk_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"1660" =>  #{ 
		"num" => fun(_TS)->  	1660 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Altai_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Altai_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Altai_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Altai_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Altai_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Altai_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Altai_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Altai_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Altai_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"241" =>  #{ 
		"num" => fun(_TS)->  	241 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Usti-Kamenogorsk_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Usti-Kamenogorsk_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Usti-Kamenogorsk_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Usti-Kamenogorsk_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Usti-Kamenogorsk_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Usti-Kamenogorsk_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Usti-Kamenogorsk_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Usti-Kamenogorsk_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Usti-Kamenogorsk_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "981" =>  #{ 
		"num" => fun(_TS)->   	981 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Semey_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Semey_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Semey_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Semey_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Semey_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Semey_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Semey_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Semey_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Semey_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},



		"33" => #{ 
		"num" => fun(_TS)->  	33 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/EEC_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/EEC_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EEC_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EEC_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EEC_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EEC_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EEC_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EEC_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EEC_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"39" =>  #{ 
		"num" => fun(_TS)->  	39 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/EGRES-1_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/EGRES-1_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-1_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-1_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-1_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"130" =>  #{ 
		"num" => fun(_TS)->  	130 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/CGPP_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/CGPP_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/CGPP_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/CGPP_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/CGPP_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/CGPP_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/CGPP_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/CGPP_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/CGPP_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"306" => #{ 
		"num" => fun(_TS)->  	306 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Oskarovka-220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Oskarovka-220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Oskarovka-220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Oskarovka-220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Oskarovka-220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Oskarovka-220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Oskarovka-220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Oskarovka-220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Oskarovka-220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "310" =>  #{ 
		"num" => fun(_TS)->   	310 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Nura_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Nura_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Nura_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Nura_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Nura_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Nura_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Nura_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Nura_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Nura_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "468" =>  #{ 
		"num" => fun(_TS)->  	468 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Agadyr_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Agadyr_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Agadyr_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Agadyr_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Agadyr_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Agadyr_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Agadyr_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Agadyr_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Agadyr_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"901" => #{ 
		"num" => fun(_TS)->  	901		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/UKGRES_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/UKGRES_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/UKGRES_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/UKGRES_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/UKGRES_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/UKGRES_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/UKGRES_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/UKGRES_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/UKGRES_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"932" => #{ 
		"num" => fun(_TS)->  	932 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Shu_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Shu_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shu_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shu_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shu_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shu_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shu_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shu_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shu_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"1916" =>  #{ 
		"num" => fun(_TS)->  	1916		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Glavnaya_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Glavnaya_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Glavnaya_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Glavnaya_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Glavnaya_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Glavnaya_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Glavnaya_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Glavnaya_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Glavnaya_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "903" =>  #{ 
		"num" => fun(_TS)->   	903 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Almaty_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Almaty_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Almaty_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Almaty_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Almaty_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Almaty_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Almaty_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Almaty_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Almaty_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},



		"939" => #{ 
		"num" => fun(_TS)->  	939 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Alma_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Alma_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Alma_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Alma_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Alma_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Alma_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Alma_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Alma_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Alma_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"988" =>  #{ 
		"num" => fun(_TS)->  	988		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Taldykorgan_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Taldykorgan_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Taldykorgan_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Taldykorgan_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Taldykorgan_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Taldykorgan_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Taldykorgan_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Taldykorgan_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Taldykorgan_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"918" =>  #{ 
		"num" => fun(_TS)->  	918 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Zapadnaya_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Zapadnaya_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zapadnaya_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zapadnaya_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zapadnaya_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zapadnaya_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zapadnaya_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zapadnaya_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zapadnaya_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"2952" =>  #{ 
		"num" => fun(_TS)->  	2952		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Kemin_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Kemin_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kemin_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kemin_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kemin_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kemin_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kemin_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kemin_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kemin_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "2918" =>  #{ 
		"num" => fun(_TS)->   	2918 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Frunze_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Frunze_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Frunze_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Frunze_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Frunze_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Frunze_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Frunze_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Frunze_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Frunze_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "801" =>  #{ 
		"num" => fun(_TS)->  	801 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Zhambyl_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Zhambyl_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhambyl_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhambyl_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhambyl_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhambyl_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhambyl_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhambyl_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhambyl_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"831" =>  #{ 
		"num" => fun(_TS)->  	831 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Shymkent_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Shymkent_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shymkent_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shymkent_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shymkent_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shymkent_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shymkent_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shymkent_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shymkent_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"2925" =>  #{ 
		"num" => fun(_TS)->  	2925 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/TashGRES_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/TashGRES_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/TashGRES_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/TashGRES_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/TashGRES_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/TashGRES_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/TashGRES_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/TashGRES_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/TashGRES_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"1630" =>  #{ 
		"num" => fun(_TS)->  	1630 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Barnaul_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Barnaul_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Barnaul_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Barnaul_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Barnaul_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Barnaul_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Barnaul_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Barnaul_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Barnaul_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "175" => #{ 
		"num" => fun(_TS)->   	175 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Kokshetau_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Kokshetau_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kokshetau_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kokshetau_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kokshetau_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kokshetau_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kokshetau_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kokshetau_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kokshetau_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},



		"590" => #{ 
		"num" => fun(_TS)->  	590 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Kostanai_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Kostanai_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kostanai_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kostanai_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kostanai_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kostanai_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kostanai_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kostanai_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kostanai_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"576" => #{ 
		"num" => fun(_TS)->  	576 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Sokol_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Sokol_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Sokol_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Sokol_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Sokol_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Sokol_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Sokol_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Sokol_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Sokol_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"180" => #{ 
		"num" => fun(_TS)->  	180 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/PS-EGPP_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/PS-EGPP_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/PS-EGPP_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/PS-EGPP_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/PS-EGPP_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/PS-EGPP_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/PS-EGPP_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/PS-EGPP_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/PS-EGPP_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"1636" => #{ 
		"num" => fun(_TS)->  	1636		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Sibir_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Sibir_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Sibir_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Sibir_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Sibir_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Sibir_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Sibir_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Sibir_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Sibir_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "577" =>  #{ 
		"num" => fun(_TS)->   	577 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Sistema-Ural_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Sistema-Ural_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Sistema-Ural_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Sistema-Ural_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Sistema-Ural_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Sistema-Ural_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Sistema-Ural_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Sistema-Ural_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Sistema-Ural_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "480" => #{ 
		"num" => fun(_TS)->  	480 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Zhezkazgan_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Zhezkazgan_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhezkazgan_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhezkazgan_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhezkazgan_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhezkazgan_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhezkazgan_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhezkazgan_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhezkazgan_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"2921" =>  #{ 
		"num" => fun(_TS)->  	2921 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/CA-1_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/CA-1_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/CA-1_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/CA-1_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/CA-1_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/CA-1_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/CA-1_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/CA-1_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/CA-1_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"51" => #{ 
		"num" => fun(_TS)->  	51 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/EGRES-1_F/u20/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/EGRES-1_F/u20/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-1_F/u20/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/u20/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/u20/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/u20/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/u20/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-1_F/u20/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-1_F/u20/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"28" =>  #{ 
		"num" => fun(_TS)->  	28 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/EGRES-2_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/EGRES-2_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-2_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-2_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-2_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-2_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-2_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-2_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-2_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "60" => #{ 
		"num" => fun(_TS)->   	60 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/EGRES-2_F/u20/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/EGRES-2_F/u20/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-2_F/u20/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-2_F/u20/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-2_F/u20/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-2_F/u20/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-2_F/u20/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-2_F/u20/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-2_F/u20/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"466" =>  #{ 
		"num" => fun(_TS)->  	466		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Zhezkazgan_F/u220/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Zhezkazgan_F/u220/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhezkazgan_F/u220/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhezkazgan_F/u220/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhezkazgan_F/u220/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhezkazgan_F/u220/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhezkazgan_F/u220/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhezkazgan_F/u220/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhezkazgan_F/u220/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"355" =>  #{ 
		"num" => fun(_TS)->  	355		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Kar.GRES-2_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Kar.GRES-2_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kar.GRES-2_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kar.GRES-2_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kar.GRES-2_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kar.GRES-2_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kar.GRES-2_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kar.GRES-2_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kar.GRES-2_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"912" => #{ 
		"num" => fun(_TS)->  	912 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/PS7-AHBK_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/PS7-AHBK_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/PS7-AHBK_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/PS7-AHBK_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/PS7-AHBK_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/PS7-AHBK_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/PS7-AHBK_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/PS7-AHBK_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/PS7-AHBK_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"904" =>  #{ 
		"num" => fun(_TS)->  	904 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/ATETS-3_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/ATETS-3_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/ATETS-3_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/ATETS-3_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/ATETS-3_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/ATETS-3_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/ATETS-3_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/ATETS-3_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/ATETS-3_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "907" =>  #{ 
		"num" => fun(_TS)->   	907 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Robot_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Robot_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Robot_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Robot_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Robot_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Robot_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Robot_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Robot_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Robot_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "475" =>  #{ 
		"num" => fun(_TS)->  	475 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Mointy_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Mointy_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Mointy_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Mointy_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Mointy_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Mointy_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Mointy_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Mointy_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Mointy_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"473" => #{ 
		"num" => fun(_TS)->  	473 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Balkhashskaya_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Balkhashskaya_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Balkhashskaya_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Balkhashskaya_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Balkhashskaya_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Balkhashskaya_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Balkhashskaya_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Balkhashskaya_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Balkhashskaya_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"50" =>  #{ 
		"num" => fun(_TS)->  	50 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/EGRES-1_F/bl2/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/EGRES-1_F/bl2/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-1_F/bl2/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/bl2/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/bl2/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/bl2/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EGRES-1_F/bl2/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-1_F/bl2/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EGRES-1_F/bl2/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		

        "465" =>  #{ 
		"num" => fun(_TS)->   	465 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Karazhal_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Karazhal_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Karazhal_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Karazhal_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Karazhal_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Karazhal_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Karazhal_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Karazhal_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Karazhal_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},



		"63" =>  #{ 
		"num" => fun(_TS)->  	63 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Tashkent_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Tashkent_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Tashkent_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Tashkent_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Tashkent_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Tashkent_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Tashkent_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Tashkent_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Tashkent_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"147" =>  #{ 
		"num" => fun(_TS)->  	147		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Avrora_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Avrora_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Avrora_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Avrora_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Avrora_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Avrora_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Avrora_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Avrora_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Avrora_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"1817" =>  #{ 
		"num" => fun(_TS)->  	1817 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Tavricheskaya_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Tavricheskaya_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Tavricheskaya_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Tavricheskaya_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Tavricheskaya_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Tavricheskaya_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Tavricheskaya_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Tavricheskaya_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Tavricheskaya_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"1850" =>  #{ 
		"num" => fun(_TS)->  	1850 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Irtyshskaya_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Irtyshskaya_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Irtyshskaya_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Irtyshskaya_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Irtyshskaya_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Irtyshskaya_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Irtyshskaya_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Irtyshskaya_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Irtyshskaya_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "30" =>  #{ 
		"num" => fun(_TS)->   	30 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/EEC_F/bl5-8/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/EEC_F/bl5-8/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EEC_F/bl5-8/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EEC_F/bl5-8/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EEC_F/bl5-8/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EEC_F/bl5-8/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/EEC_F/bl5-8/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EEC_F/bl5-8/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/EEC_F/bl5-8/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "7201" => #{ 
		"num" => fun(_TS)->  	7201 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Semey_U-K_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Semey_U-K_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Semey_U-K_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Semey_U-K_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Semey_U-K_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Semey_U-K_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Semey_U-K_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Semey_U-K_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Semey_U-K_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"7202" =>  #{ 
		"num" => fun(_TS)->  	7202 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_EEC_Semey_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_EEC_Semey_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_EEC_Semey_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_EEC_Semey_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_EEC_Semey_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_EEC_Semey_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_EEC_Semey_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_EEC_Semey_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_EEC_Semey_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"7203" =>  #{ 
		"num" => fun(_TS)->  	7203 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_EGRES-1_EEC_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_EGRES-1_EEC_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_EGRES-1_EEC_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_EGRES-1_EEC_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_EGRES-1_EEC_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_EGRES-1_EEC_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_EGRES-1_EEC_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_EGRES-1_EEC_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_EGRES-1_EEC_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"7204" =>  #{ 
		"num" => fun(_TS)->  	7204 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_CGPP_EGRES-1_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_CGPP_EGRES-1_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_CGPP_EGRES-1_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_CGPP_EGRES-1_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_CGPP_EGRES-1_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_CGPP_EGRES-1_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_CGPP_EGRES-1_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_CGPP_EGRES-1_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_CGPP_EGRES-1_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "7205" =>  #{ 
		"num" => fun(_TS)->   	7205 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_CGPP_Oskarovka_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_CGPP_Oskarovka_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_CGPP_Oskarovka_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_CGPP_Oskarovka_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_CGPP_Oskarovka_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_CGPP_Oskarovka_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_CGPP_Oskarovka_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_CGPP_Oskarovka_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_CGPP_Oskarovka_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"7206" =>  #{ 
		"num" => fun(_TS)->  	7206 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_EGRES-1_Oskarovka_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_EGRES-1_Oskarovka_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_EGRES-1_Oskarovka_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_EGRES-1_Oskarovka_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_EGRES-1_Oskarovka_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_EGRES-1_Oskarovka_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_EGRES-1_Oskarovka_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_EGRES-1_Oskarovka_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_EGRES-1_Oskarovka_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"7208" =>  #{ 
		"num" => fun(_TS)->  	7208		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_KarGRES_Nura_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Nura_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Nura_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Nura_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Nura_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Nura_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Nura_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Nura_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Nura_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"7209" => #{ 
		"num" => fun(_TS)->  	7209 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_KarGRES_Karazhal_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Karazhal_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Karazhal_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Karazhal_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Karazhal_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Karazhal_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Karazhal_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Karazhal_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Karazhal_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "7210" => #{ 
		"num" => fun(_TS)->   	7210 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_KarGRES_Agadyr_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Agadyr_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Agadyr_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Agadyr_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Agadyr_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Agadyr_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Agadyr_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Agadyr_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Agadyr_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "7211" =>  #{ 
		"num" => fun(_TS)->  	7211 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_KarGRES_Balkhashskaya_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Balkhashskaya_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Balkhashskaya_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Balkhashskaya_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Balkhashskaya_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Balkhashskaya_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Balkhashskaya_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Balkhashskaya_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Balkhashskaya_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"7212" => #{ 
		"num" => fun(_TS)->  	7212 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_UkGRES_Shu_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_UkGRES_Shu_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_UkGRES_Shu_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_UkGRES_Shu_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_UkGRES_Shu_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_UkGRES_Shu_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_UkGRES_Shu_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_UkGRES_Shu_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_UkGRES_Shu_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		
		"7213" =>  #{ 
		"num" => fun(_TS)->  	7213 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Almaty_Alma_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Almaty_Alma_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Almaty_Alma_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Almaty_Alma_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Almaty_Alma_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Almaty_Alma_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Almaty_Alma_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Almaty_Alma_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Almaty_Alma_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "7214" => #{ 
		"num" => fun(_TS)->   	7214 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Glavnaya_Kemin_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Glavnaya_Kemin_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Glavnaya_Kemin_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Glavnaya_Kemin_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Glavnaya_Kemin_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Glavnaya_Kemin_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Glavnaya_Kemin_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Glavnaya_Kemin_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Glavnaya_Kemin_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},



		"7215" => #{ 
		"num" => fun(_TS)->  	7215		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Frunze_Kemin_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Frunze_Kemin_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Frunze_Kemin_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Frunze_Kemin_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Frunze_Kemin_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Frunze_Kemin_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Frunze_Kemin_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Frunze_Kemin_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Frunze_Kemin_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"7216" => #{ 
		"num" => fun(_TS)->  	7216 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Zhambyl_Shymkent_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Zhambyl_Shymkent_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Zhambyl_Shymkent_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Zhambyl_Shymkent_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Zhambyl_Shymkent_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Zhambyl_Shymkent_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Zhambyl_Shymkent_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Zhambyl_Shymkent_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Zhambyl_Shymkent_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"7217" =>  #{ 
		"num" => fun(_TS)->  	7217 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Glavnaya_Frunze_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Glavnaya_Frunze_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Glavnaya_Frunze_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Glavnaya_Frunze_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Glavnaya_Frunze_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Glavnaya_Frunze_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Glavnaya_Frunze_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Glavnaya_Frunze_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Glavnaya_Frunze_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"7218" => #{ 
		"num" => fun(_TS)->  	7218 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Taldykorgan_Robot_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Taldykorgan_Robot_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Taldykorgan_Robot_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Taldykorgan_Robot_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Taldykorgan_Robot_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Taldykorgan_Robot_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Taldykorgan_Robot_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Taldykorgan_Robot_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Taldykorgan_Robot_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "7219" =>  #{ 
		"num" => fun(_TS)->   	7219 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Mointy_UkGRES_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Mointy_UkGRES_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Mointy_UkGRES_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Mointy_UkGRES_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Mointy_UkGRES_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Mointy_UkGRES_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Mointy_UkGRES_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Mointy_UkGRES_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Mointy_UkGRES_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "7221" =>  #{ 
		"num" => fun(_TS)->  	7221 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Zhezkazgan_Karazhal_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Zhezkazgan_Karazhal_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Zhezkazgan_Karazhal_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Zhezkazgan_Karazhal_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Zhezkazgan_Karazhal_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Zhezkazgan_Karazhal_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Zhezkazgan_Karazhal_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Zhezkazgan_Karazhal_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Zhezkazgan_Karazhal_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"842" => #{ 
		"num" => fun(_TS)->  	842 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Mirgalimsai_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Mirgalimsai_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Mirgalimsai_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Mirgalimsai_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Mirgalimsai_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Mirgalimsai_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Mirgalimsai_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Mirgalimsai_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Mirgalimsai_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"840" => #{ 
		"num" => fun(_TS)->  	840 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Kentau_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Kentau_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kentau_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kentau_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kentau_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kentau_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kentau_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kentau_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kentau_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"869" =>  #{ 
		"num" => fun(_TS)->  	869 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Ru-6_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Ru-6_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Ru-6_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Ru-6_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Ru-6_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Ru-6_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Ru-6_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Ru-6_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Ru-6_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "862" => #{ 
		"num" => fun(_TS)->   	862		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Kyzylorda_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Kyzylorda_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kyzylorda_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kyzylorda_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kyzylorda_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kyzylorda_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kyzylorda_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kyzylorda_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kyzylorda_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"467" =>  #{ 
		"num" => fun(_TS)->  	467 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Kumkol_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Kumkol_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kumkol_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kumkol_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kumkol_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kumkol_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kumkol_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kumkol_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kumkol_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"928" => #{ 
		"num" => fun(_TS)->  	928 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Medeo_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Medeo_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Medeo_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Medeo_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Medeo_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Medeo_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Medeo_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Medeo_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Medeo_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"953" => #{ 
		"num" => fun(_TS)->  	953 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Koyan-Kos_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Koyan-Kos_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Koyan-Kos_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Koyan-Kos_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Koyan-Kos_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Koyan-Kos_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Koyan-Kos_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Koyan-Kos_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Koyan-Kos_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"7227" => #{ 
		"num" => fun(_TS)->  	7227 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Kyzylorda_Kumkol_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Kyzylorda_Kumkol_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Kyzylorda_Kumkol_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Kyzylorda_Kumkol_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Kyzylorda_Kumkol_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Kyzylorda_Kumkol_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Kyzylorda_Kumkol_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Kyzylorda_Kumkol_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Kyzylorda_Kumkol_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "805" => #{ 
		"num" => fun(_TS)->   	805		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/ZhGRES_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/ZhGRES_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/ZhGRES_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/ZhGRES_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/ZhGRES_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/ZhGRES_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/ZhGRES_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/ZhGRES_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/ZhGRES_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "814" =>  #{ 
		"num" => fun(_TS)->  	814 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Aspara_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Aspara_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Aspara_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Aspara_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Aspara_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Aspara_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Aspara_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Aspara_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Aspara_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"7223" =>  #{ 
		"num" => fun(_TS)->  	7223 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_ZhGRES_Frunze_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_ZhGRES_Frunze_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_ZhGRES_Frunze_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_ZhGRES_Frunze_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_ZhGRES_Frunze_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_ZhGRES_Frunze_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_ZhGRES_Frunze_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_ZhGRES_Frunze_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_ZhGRES_Frunze_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"839" => #{ 
		"num" => fun(_TS)->  	839 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Zhylga_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Zhylga_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhylga_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhylga_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhylga_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhylga_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhylga_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhylga_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhylga_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"843" => #{ 
		"num" => fun(_TS)->  	843 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Shymkentskaya_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Shymkentskaya_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shymkentskaya_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shymkentskaya_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shymkentskaya_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shymkentskaya_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shymkentskaya_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shymkentskaya_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shymkentskaya_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "7224" => #{ 
		"num" => fun(_TS)->   	7224 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Kentau_Zhambyl_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Kentau_Zhambyl_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Kentau_Zhambyl_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Kentau_Zhambyl_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Kentau_Zhambyl_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Kentau_Zhambyl_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Kentau_Zhambyl_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Kentau_Zhambyl_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Kentau_Zhambyl_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},



		"865" => #{ 
		"num" => fun(_TS)->  	865 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Zhanakorgan_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Zhanakorgan_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhanakorgan_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhanakorgan_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhanakorgan_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhanakorgan_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zhanakorgan_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhanakorgan_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zhanakorgan_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},  


		"7225" =>  #{ 
		"num" => fun(_TS)->  	7225		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Shymkent_220_Zhylga_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Shymkent_220_Zhylga_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Shymkent_220_Zhylga_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Shymkent_220_Zhylga_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Shymkent_220_Zhylga_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Shymkent_220_Zhylga_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Shymkent_220_Zhylga_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Shymkent_220_Zhylga_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Shymkent_220_Zhylga_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"7226" => #{ 
		"num" => fun(_TS)->  	7226 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_Ru-6_Kyzylorda_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_Ru-6_Kyzylorda_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Ru-6_Kyzylorda_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Ru-6_Kyzylorda_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Ru-6_Kyzylorda_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Ru-6_Kyzylorda_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_Ru-6_Kyzylorda_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Ru-6_Kyzylorda_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_Ru-6_Kyzylorda_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},

		
		"7228" => #{ 
		"num" => fun(_TS)->  	7228 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_GNPS_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_GNPS_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_GNPS_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_GNPS_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_GNPS_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_GNPS_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_GNPS_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_GNPS_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_GNPS_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "7229" => #{ 
		"num" => fun(_TS)->   	7229 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/pu_KarGRES_Oskarovka_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Oskarovka_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Oskarovka_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Oskarovka_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Oskarovka_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Oskarovka_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/pu_KarGRES_Oskarovka_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Oskarovka_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/pu_KarGRES_Oskarovka_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "356" => #{ 
		"num" => fun(_TS)->   	356 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Kar.GRES-2_F/u110/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Kar.GRES-2_F/u110/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kar.GRES-2_F/u110/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kar.GRES-2_F/u110/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kar.GRES-2_F/u110/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kar.GRES-2_F/u110/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kar.GRES-2_F/u110/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kar.GRES-2_F/u110/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kar.GRES-2_F/u110/ntag">>, 	<<"q_genmax">> ) 		end
      	},



        "455" => #{ 
		"num" => fun(_TS)->   	455 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Kumkol_220_F/u110/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Kumkol_220_F/u110/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kumkol_220_F/u110/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kumkol_220_F/u110/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kumkol_220_F/u110/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kumkol_220_F/u110/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kumkol_220_F/u110/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kumkol_220_F/u110/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kumkol_220_F/u110/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "472" => #{ 
		"num" => fun(_TS)->   	472 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Balkhashskaya_F/u110/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Balkhashskaya_F/u110/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Balkhashskaya_F/u110/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Balkhashskaya_F/u110/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Balkhashskaya_F/u110/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Balkhashskaya_F/u110/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Balkhashskaya_F/u110/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Balkhashskaya_F/u110/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Balkhashskaya_F/u110/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "905" => #{ 
		"num" => fun(_TS)->   	905 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/ATETS-3_F/u110/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/ATETS-3_F/u110/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/ATETS-3_F/u110/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/ATETS-3_F/u110/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/ATETS-3_F/u110/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/ATETS-3_F/u110/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/ATETS-3_F/u110/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/ATETS-3_F/u110/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/ATETS-3_F/u110/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "911" => #{ 
		"num" => fun(_TS)->   	911 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Сhilik_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Сhilik_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Сhilik_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Сhilik_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Сhilik_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Сhilik_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Сhilik_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Сhilik_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Сhilik_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "913" => #{ 
		"num" => fun(_TS)->   	913 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/PS7-AHBK_F/u110/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/PS7-AHBK_F/u110/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/PS7-AHBK_F/u110/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/PS7-AHBK_F/u110/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/PS7-AHBK_F/u110/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/PS7-AHBK_F/u110/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/PS7-AHBK_F/u110/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/PS7-AHBK_F/u110/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/PS7-AHBK_F/u110/ntag">>, 	<<"q_genmax">> ) 		end
      	},



        "935" => #{ 
		"num" => fun(_TS)->   	935 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/MoinakGES_220_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/MoinakGES_220_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/MoinakGES_220_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/MoinakGES_220_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/MoinakGES_220_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/MoinakGES_220_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/MoinakGES_220_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/MoinakGES_220_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/MoinakGES_220_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "4799" => #{ 
		"num" => fun(_TS)->   	4799		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/TrGRES_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/TrGRES_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/TrGRES_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/TrGRES_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/TrGRES_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/TrGRES_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/TrGRES_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/TrGRES_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/TrGRES_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"4727" => #{ 
		"num" => fun(_TS)->   	4727		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Kurgan_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Kurgan_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kurgan_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kurgan_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kurgan_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kurgan_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kurgan_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kurgan_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kurgan_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "4703" => #{ 
		"num" => fun(_TS)->   	4703		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Kozirevo_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Kozirevo_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kozirevo_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kozirevo_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kozirevo_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kozirevo_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Kozirevo_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kozirevo_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Kozirevo_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"4782" => #{ 
		"num" => fun(_TS)->   	4782 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Shagol_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Shagol_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shagol_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shagol_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shagol_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shagol_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Shagol_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shagol_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Shagol_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "4705" => #{ 
		"num" => fun(_TS)->   	4705		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Cheliabinsk_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Cheliabinsk_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Cheliabinsk_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Cheliabinsk_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Cheliabinsk_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Cheliabinsk_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Cheliabinsk_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Cheliabinsk_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Cheliabinsk_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		 "10000" => #{ 
		"num" => fun(_TS)->   	10000 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/UjUrGRES_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/UjUrGRES_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/UjUrGRES_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/UjUrGRES_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/UjUrGRES_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/UjUrGRES_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/UjUrGRES_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/UjUrGRES_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/UjUrGRES_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "4790" => #{ 
		"num" => fun(_TS)->   	4790		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/IrGRES_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/IrGRES_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/IrGRES_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/IrGRES_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/IrGRES_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/IrGRES_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/IrGRES_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/IrGRES_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/IrGRES_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"4785" => #{ 
		"num" => fun(_TS)->   	4785 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/MagnitRF_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/MagnitRF_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/MagnitRF_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/MagnitRF_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/MagnitRF_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/MagnitRF_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/MagnitRF_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/MagnitRF_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/MagnitRF_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "1852" => #{ 
		"num" => fun(_TS)->   	1852 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/VityazRF_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/VityazRF_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/VityazRF_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/VityazRF_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/VityazRF_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/VityazRF_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/VityazRF_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/VityazRF_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/VityazRF_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"1853" => #{ 
		"num" => fun(_TS)->   	1853		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/VoshodRF_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/VoshodRF_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/VoshodRF_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/VoshodRF_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/VoshodRF_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/VoshodRF_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/VoshodRF_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/VoshodRF_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/VoshodRF_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


        "9917" => #{ 
		"num" => fun(_TS)->   	9917 		end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Barabinskaya_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Barabinskaya_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Barabinskaya_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Barabinskaya_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Barabinskaya_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Barabinskaya_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Barabinskaya_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Barabinskaya_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Barabinskaya_F/ntag">>, 	<<"q_genmax">> ) 		end
      	},


		"1631" => #{ 
		"num" => fun(_TS)->   	1631	end,
      	"state" => fun(_TS)-> 	wacs_snapshot:read_tag(		<<"/Zarya_F/ntag">>, 	<<"state_n">> ) 			end,
      	"U" => fun(_TS)-> 		wacs_snapshot:read_tag(  	<<"/Zarya_F/ntag">>, 	<<"u_nom_n">> ) 				end,
      	"fiU" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zarya_F/ntag">>, 	<<"u_fi_n">> ) 	end,
      	"Pn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zarya_F/futureP/predict_p_load">>, TS )		end,
      	"Qn" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zarya_F/futureP/predict_q_load">>, TS )		end,
     	"Pg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zarya_F/futureP/predict_p_gen">>, TS )		end,
      	"Qg" => fun(TS)-> 		wacs_snapshot:read_archive( <<"/Zarya_F/futureP/predict_q_gen">>, TS )		end,
      	"Qmin" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zarya_F/ntag">>, 	<<"q_genmin">> ) 		end,
      	"Qmax" => fun(_TS)-> 	wacs_snapshot:read_tag(  	<<"/Zarya_F/ntag">>, 	<<"q_genmax">> ) 		end
      	}

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END NODES
    },

    %----------------------------------------------------------------------------------
    %     LINKS/Branches (ВЕТВИ)
    %----------------------------------------------------------------------------------
    "links" => #{

      %	"_______" => #{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %		"start" => fun(_TS)->		__	 		end,
      %		"end" => fun(_TS)->			__ 			end,
      %		"num" => fun(_TS)->			__			end,
      %		"state" => fun(TS)->   wacs_snapshot:read_archive(   <<"//__________/futureP/predict_state_b">>, TS ) 	end,
      %		"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/root/PROJECT/TAGS/__________/futureP/predict_p_start">>, TS ) 	end,
      %		"Qn" => fun(_TS)-> 			0   end, % wacs_snapshot:read_archive(   <<"/root/PROJECT/TAGS/__________/futureP/predict_q_start">>, TS ) 	end,
      %		"Pk" => fun(_TS)-> 			0   end, % wacs_snapshot:read_archive(   <<"/root/PROJECT/TAGS/__________/futureP/predict_p_end">>, TS ) 	end,
      %		"Qk" => fun(_TS)-> 			0   end, % wacs_snapshot:read_archive(   <<"/root/PROJECT/TAGS/__________/futureP/predict_q_end">>, TS ) 	end,
      %	},

      % TODO...
	  "26_25_1" => #{
      	"start" => fun(_TS)->		26	 		end,
      	"end" => fun(_TS)->			25 			end,
      	"num" => fun(_TS)->			1			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5107_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5107_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5107_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5107_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5107_F/futureP/predict_q_end">>, TS ) 	end
      	},


	    "26_25_2" => #{
      	"start" => fun(_TS)->		26	 		end,
      	"end" => fun(_TS)->			25 			end,
      	"num" => fun(_TS)->			2			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5117_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5117_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5117_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5117_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5117_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"25_325_3" => #{
      	"start" => fun(_TS)->		25	 		end,
      	"end" => fun(_TS)->			325 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5120_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5120_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5120_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5120_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5120_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"325_469_3" => #{
      	"start" => fun(_TS)->		325	 		end,
      	"end" => fun(_TS)->			469 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5138_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5138_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5138_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5138_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5138_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"26_469_3" => #{
      	"start" => fun(_TS)->		26	 		end,
      	"end" => fun(_TS)->			469 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5170_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5170_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5170_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5170_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5170_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"469_900_1" => #{
      	"start" => fun(_TS)->		469	 		end,
      	"end" => fun(_TS)->			900 		end,
      	"num" => fun(_TS)->			1			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5300_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5300_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5300_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5300_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5300_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"469_900_2" => #{
      	"start" => fun(_TS)->		469	 		end,
      	"end" => fun(_TS)->			900 		end,
      	"num" => fun(_TS)->			2			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5320_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5320_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5320_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5320_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5320_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"900_902_3" => #{
      	"start" => fun(_TS)->		900	 		end,
      	"end" => fun(_TS)->			902 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5313_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5313_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5313_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5313_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5313_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"900_938_3" => #{
      	"start" => fun(_TS)->		900	 		end,
      	"end" => fun(_TS)->			938 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5363_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5363_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5363_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5363_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5363_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"900_9932_3" => #{
      	"start" => fun(_TS)->		900	 		end,
      	"end" => fun(_TS)->			9932 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5333_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5333_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5333_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5333_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5333_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"902_9932_3" => #{
      	"start" => fun(_TS)->		902	 		end,
      	"end" => fun(_TS)->			9932 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5343_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5343_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5343_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5343_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5343_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"902_938_3" => #{
      	"start" => fun(_TS)->		902	 		end,
      	"end" => fun(_TS)->			938 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5353_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5353_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5353_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5353_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5353_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"9932_2919_3" => #{
      	"start" => fun(_TS)->		9932	 		end,
      	"end" => fun(_TS)->			2919 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5143_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5143_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5143_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5143_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5143_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"2919_800_3" => #{
      	"start" => fun(_TS)->		2919	 	end,
      	"end" => fun(_TS)->			800 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5159_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5159_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5159_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5159_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5159_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"800_830_3" => #{
      	"start" => fun(_TS)->		800	 		end,
      	"end" => fun(_TS)->			830 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5169_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5169_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5169_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5169_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5169_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"980_26_3" => #{
      	"start" => fun(_TS)->		980	 		end,
      	"end" => fun(_TS)->			26 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5370_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5370_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5370_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5370_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5370_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"980_986_3" => #{
      	"start" => fun(_TS)->		980	 		end,
      	"end" => fun(_TS)->			986 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5394_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5394_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5394_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5394_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5394_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"986_987_3" => #{
      	"start" => fun(_TS)->		986	 		end,
      	"end" => fun(_TS)->			987 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5400_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5400_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5400_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5400_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5400_F/futureP/predict_q_end">>, TS ) 	end
      	},


		
		"987_938_3" => #{
      	"start" => fun(_TS)->		987	 		end,
      	"end" => fun(_TS)->			938 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5413_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5413_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5413_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5413_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5413_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"980_240_3" => #{
      	"start" => fun(_TS)->		980	 		end,
      	"end" => fun(_TS)->			240 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5384_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5384_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5384_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5384_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5384_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"224_830_3" => #{
      	"start" => fun(_TS)->		224	 		end,
      	"end" => fun(_TS)->			830 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5019_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5019_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5019_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5019_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5019_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"25_129_3" => #{
      	"start" => fun(_TS)->		25	 		end,
      	"end" => fun(_TS)->			129 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5050_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5050_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5050_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5050_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5050_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"25_31_3" => #{
      	"start" => fun(_TS)->		25	 		end,
      	"end" => fun(_TS)->			31 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5017_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5017_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5017_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5017_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5017_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"31_1621_3" => #{
      	"start" => fun(_TS)->		31	 		end,
      	"end" => fun(_TS)->			1621 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5527_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5527_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5527_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5527_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5527_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"1621_240_3" => #{
      	"start" => fun(_TS)->		1621	 		end,
      	"end" => fun(_TS)->			240 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5544_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5544_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5544_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5544_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5544_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"26_1660_3" => #{
      	"start" => fun(_TS)->		26	 		end,
      	"end" => fun(_TS)->			1660 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL1104_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL1104_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL1104_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL1104_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL1104_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"240_241_1" => #{
      	"start" => fun(_TS)->		240	 		end,
      	"end" => fun(_TS)->			241			end,
      	"num" => fun(_TS)->			1			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Ust-Kam_220_AT1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Ust-Kam_220_AT1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Ust-Kam_220_AT1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Ust-Kam_220_AT1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Ust-Kam_220_AT1_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"240_241_2" => #{
      	"start" => fun(_TS)->		240	 		end,
      	"end" => fun(_TS)->			241 			end,
      	"num" => fun(_TS)->			2			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Ust-Kam_220_AT2_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Ust-Kam_220_AT2_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Ust-Kam_220_AT2_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Ust-Kam_220_AT2_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Ust-Kam_220_AT2_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"980_981_3" => #{
      	"start" => fun(_TS)->		980	 		end,
      	"end" => fun(_TS)->			981 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Semei_220_AT1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Semei_220_AT1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Semei_220_AT1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Semei_220_AT1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Semei_220_AT1_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"31_33_3" => #{
      	"start" => fun(_TS)->		31	 		end,
      	"end" => fun(_TS)->			33 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/AksuGRES_220_AT3_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/AksuGRES_220_AT3_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/AksuGRES_220_AT3_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/AksuGRES_220_AT3_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/AksuGRES_220_AT3_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"25_39_3" => #{
      	"start" => fun(_TS)->		25	 		end,
      	"end" => fun(_TS)->			39 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/EGRES1_220_AT11_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EGRES1_220_AT11_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EGRES1_220_AT11_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EGRES1_220_AT11_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/EGRES1_220_AT11_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"129_130_1" => #{
      	"start" => fun(_TS)->		129	 		end,
      	"end" => fun(_TS)->			130 			end,
      	"num" => fun(_TS)->			1			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/CGPP_500_FAT3_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/CGPP_500_FAT3_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/CGPP_500_FAT3_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/CGPP_500_FAT3_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/CGPP_500_FAT3_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"129_130_2" => #{
      	"start" => fun(_TS)->		129	 		end,
      	"end" => fun(_TS)->			130 		end,
      	"num" => fun(_TS)->			2			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/CGPP_500_FAT4_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/CGPP_500_FAT4_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/CGPP_500_FAT4_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/CGPP_500_FAT4_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/CGPP_500_FAT4_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"325_310_3" => #{
      	"start" => fun(_TS)->		325	 		end,
      	"end" => fun(_TS)->			310 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Nura_500_AT-1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Nura_500_AT-1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Nura_500_AT-1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Nura_500_AT-1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Nura_500_AT-1_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"469_468_3" => #{
      	"start" => fun(_TS)->		469	 		end,
      	"end" => fun(_TS)->			468			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Agadyr_500_AT1H_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Agadyr_500_AT1H_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Agadyr_500_AT1H_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Agadyr_500_AT1H_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Agadyr_500_AT1H_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"900_901_3" => #{
      	"start" => fun(_TS)->		900	 		end,
      	"end" => fun(_TS)->			901 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/UKGRES_220_AT1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/UKGRES_220_AT1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/UKGRES_220_AT1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/UKGRES_220_AT1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/UKGRES_220_AT1_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"9932_932_3" => #{
      	"start" => fun(_TS)->		9932	 		end,
      	"end" => fun(_TS)->			932 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Shu500_220_FAT3_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Shu500_220_FAT3_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Shu500_220_FAT3_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Shu500_220_FAT3_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Shu500_220_FAT3_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"932_1916_3" => #{
      	"start" => fun(_TS)->		932	 		end,
      	"end" => fun(_TS)->			1916 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2163_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2163_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2163_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2163_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2163_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"1916_903_3" => #{
      	"start" => fun(_TS)->		1916	 		end,
      	"end" => fun(_TS)->			903 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2193_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2193_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2193_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2193_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2193_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"902_903_1" => #{
      	"start" => fun(_TS)->		902	 		end,
      	"end" => fun(_TS)->			903 			end,
      	"num" => fun(_TS)->			1			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Almaty_220_AT-1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Almaty_220_AT-1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Almaty_220_AT-1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Almaty_220_AT-1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Almaty_220_AT-1_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"902_903_2" => #{
      	"start" => fun(_TS)->		902	 		end,
      	"end" => fun(_TS)->			903 			end,
      	"num" => fun(_TS)->			2			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Almaty_220_AT-2_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Almaty_220_AT-2_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Almaty_220_AT-2_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Almaty_220_AT-2_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Almaty_220_AT-2_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"938_939_1" => #{
      	"start" => fun(_TS)->		938	 		end,
      	"end" => fun(_TS)->			939			end,
      	"num" => fun(_TS)->			1			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Alma_220_AT-1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Alma_220_AT-1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Alma_220_AT-1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Alma_220_AT-1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Alma_220_AT-1_F/futureP/predict_q_end">>, TS ) 	end
      	},


		
		"938_939_2" => #{
      	"start" => fun(_TS)->		938	 		end,
      	"end" => fun(_TS)->			939			end,
      	"num" => fun(_TS)->			2			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Alma_220_AT-2_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Alma_220_AT-2_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Alma_220_AT-2_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Alma_220_AT-2_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Alma_220_AT-2_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"987_988_3" => #{
      	"start" => fun(_TS)->		987	 		end,
      	"end" => fun(_TS)->			988 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/TaldK500_220_AT-1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/TaldK500_220_AT-1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/TaldK500_220_AT-1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/TaldK500_220_AT-1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/TaldK500_220_AT-1_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"918_2952_3" => #{
      	"start" => fun(_TS)->		918	 		end,
      	"end" => fun(_TS)->			2952			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2183_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2183_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2183_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2183_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2183_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"800_801_3" => #{
      	"start" => fun(_TS)->		800	 		end,
      	"end" => fun(_TS)->			801 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Zambul_220_AT-1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Zambul_220_AT-1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Zambul_220_AT-1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Zambul_220_AT-1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Zambul_220_AT-1_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"2919_2918_1" => #{
      	"start" => fun(_TS)->		2919	 	end,
      	"end" => fun(_TS)->			2918		end,
      	"num" => fun(_TS)->			1			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Frunze500_200_AT1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Frunze500_200_AT1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Frunze500_200_AT1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Frunze500_200_AT1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Frunze500_200_AT1_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"2919_2918_2" => #{
      	"start" => fun(_TS)->		2919	 	end,
      	"end" => fun(_TS)->			2918		end,
      	"num" => fun(_TS)->			2			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Frunze500_200_AT2_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Frunze500_200_AT2_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Frunze500_200_AT2_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Frunze500_200_AT2_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Frunze500_200_AT2_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"224_2925_3" => #{
      	"start" => fun(_TS)->		224	 		end,
      	"end" => fun(_TS)->			2925 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/TashTES_AT_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/TashTES_AT_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/TashTES_AT_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/TashTES_AT_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/TashTES_AT_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"830_831_1" => #{
      	"start" => fun(_TS)->		830	 		end,
      	"end" => fun(_TS)->			831 			end,
      	"num" => fun(_TS)->			1			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Shym500_220_AT1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Shym500_220_AT1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Shym500_220_AT1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Shym500_220_AT1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Shym500_220_AT1_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"830_831_2" => #{
      	"start" => fun(_TS)->		830	 		end,
      	"end" => fun(_TS)->			831 			end,
      	"num" => fun(_TS)->			2			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Shym500_220_AT3_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Shym500_220_AT3_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Shym500_220_AT3_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Shym500_220_AT3_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Shym500_220_AT3_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"1660_1630_1" => #{
      	"start" => fun(_TS)->		1660	 		end,
      	"end" => fun(_TS)->			1630 			end,
      	"num" => fun(_TS)->			1			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL595_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL595_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL595_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL595_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL595_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"1630_1621_3" => #{
      	"start" => fun(_TS)->		1630	 		end,
      	"end" => fun(_TS)->			1621 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL551_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL551_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL551_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL551_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL551_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"26_175_3" => #{
      	"start" => fun(_TS)->		26	 		end,
      	"end" => fun(_TS)->			175 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL1101_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL1101_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL1101_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL1101_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL1101_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"175_590_3" => #{
      	"start" => fun(_TS)->		175	 		end,
      	"end" => fun(_TS)->			590 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL1102_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL1102_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL1102_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL1102_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL1102_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"590_576_3" => #{
      	"start" => fun(_TS)->		590	 		end,
      	"end" => fun(_TS)->			576	    	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5096_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5096_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5096_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5096_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5096_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"129_180_3" => #{
      	"start" => fun(_TS)->		129	 		end,
      	"end" => fun(_TS)->			180			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5071_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5071_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5071_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5071_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5071_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"180_576_3" => #{
      	"start" => fun(_TS)->		180	 		end,
      	"end" => fun(_TS)->			576 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5086_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5086_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5086_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5086_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5086_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"576_577_3" => #{
      	"start" => fun(_TS)->		576	 		end,
      	"end" => fun(_TS)->			577			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5726_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5726_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5726_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5726_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5726_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"1660_1636_3" => #{
      	"start" => fun(_TS)->		1660	 		end,
      	"end" => fun(_TS)->			1636 			end,
      	"num" => fun(_TS)->			3			    end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_altai_sibir_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_altai_sibir_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_altai_sibir_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_altai_sibir_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_altai_sibir_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"469_480_3" => #{
      	"start" => fun(_TS)->		469	 		end,
      	"end" => fun(_TS)->			480			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5148_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5148_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5148_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5148_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5148_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"1660_1630_2" => #{
      	"start" => fun(_TS)->		1660	 		end,
      	"end" => fun(_TS)->			1630 			end,
      	"num" => fun(_TS)->			2			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL596_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL596_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL596_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL596_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL596_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"28_60_3" => #{
      	"start" => fun(_TS)->		28	 		end,
      	"end" => fun(_TS)->			60 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/EGRES-2_500_20_T_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EGRES-2_500_20_T_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EGRES-2_500_20_T_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EGRES-2_500_20_T_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/EGRES-2_500_20_T_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"25_51_3" => #{
      	"start" => fun(_TS)->		25	 		end,
      	"end" => fun(_TS)->			51 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/EGRES-1_500_T3-8_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EGRES-1_500_T3-8_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EGRES-1_500_T3-8_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EGRES-1_500_T3-8_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/EGRES-1_500_T3-8_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"28_26_1" => #{
      	"start" => fun(_TS)->		28	 		end,
      	"end" => fun(_TS)->			26 			end,
      	"num" => fun(_TS)->			1			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5817_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5817_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5817_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5817_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5817_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"28_26_2" => #{
      	"start" => fun(_TS)->		28	 		end,
      	"end" => fun(_TS)->			26 			end,
      	"num" => fun(_TS)->			2			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5827_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5827_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5827_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5827_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5827_F/futureP/predict_q_end">>, TS ) 	end
      	},


		
		"480_466_3" => #{
      	"start" => fun(_TS)->		480	 		end,
      	"end" => fun(_TS)->			466			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Zhezkaz_500_AT1H_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Zhezkaz_500_AT1H_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Zhezkaz_500_AT1H_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Zhezkaz_500_AT1H_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Zhezkaz_500_AT1H_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"904_939_3" => #{
      	"start" => fun(_TS)->		904 		end,
      	"end" => fun(_TS)->			939 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_ATETS-3-Alma220_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ATETS-3-Alma220_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ATETS-3-Alma220_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ATETS-3-Alma220_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_ATETS-3-Alma220_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"912_953_3" => #{
      	"start" => fun(_TS)->		912	 		end,
      	"end" => fun(_TS)->			953 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_ps7_ahbk-koyan_kos_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ps7_ahbk-koyan_kos_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ps7_ahbk-koyan_kos_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ps7_ahbk-koyan_kos_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_ps7_ahbk-koyan_kos_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"918_912_3" => #{
      	"start" => fun(_TS)->		918	 		end,
      	"end" => fun(_TS)->			912			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2173_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2173_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2173_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2173_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2173_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"939_907_3" => #{
      	"start" => fun(_TS)->		939	 		end,
      	"end" => fun(_TS)->			907			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2463_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2463_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2463_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2463_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2463_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"903_904_3" => #{
      	"start" => fun(_TS)->		903	 		end,
      	"end" => fun(_TS)->			904			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2013_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2013_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2013_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2013_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2013_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"468_473_3" => #{
      	"start" => fun(_TS)->		468	 		end,
      	"end" => fun(_TS)->			473			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2208_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2208_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2208_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2208_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2208_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"473_475_3" => #{
      	"start" => fun(_TS)->		473	 		end,
      	"end" => fun(_TS)->			475  		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2448_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2448_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2448_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2448_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2448_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"475_468_3" => #{
      	"start" => fun(_TS)->		475	 		end,
      	"end" => fun(_TS)->			468 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2438_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2438_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2438_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2438_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2438_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"39_50_3" => #{
      	"start" => fun(_TS)->		39	 		end,
      	"end" => fun(_TS)->			50			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/EGRES-1_T1-2_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EGRES-1_T1-2_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EGRES-1_T1-2_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EGRES-1_T1-2_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/EGRES-1_T1-2_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"903_907_3" => #{
      	"start" => fun(_TS)->		903	 		end,
      	"end" => fun(_TS)->			907			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2053_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2053_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2053_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2053_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2053_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"224_63_3" => #{
      	"start" => fun(_TS)->		224	 		end,
      	"end" => fun(_TS)->			63 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL522_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL522_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL522_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL522_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL522_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"175_147_3" => #{
      	"start" => fun(_TS)->		175	 		end,
      	"end" => fun(_TS)->			147 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5191_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5191_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5191_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5191_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5191_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"147_1817_3" => #{
      	"start" => fun(_TS)->		147	 		end,
      	"end" => fun(_TS)->			1817			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5561_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5561_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5561_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5561_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5561_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"25_1817_3" => #{
      	"start" => fun(_TS)->		25	 		end,
      	"end" => fun(_TS)->			1817		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5577_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5577_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5577_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5577_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5577_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"31_1850_3" => #{
      	"start" => fun(_TS)->		31	 		end,
      	"end" => fun(_TS)->			1850 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5537_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5537_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5537_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5537_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5537_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"1817_1850_3" => #{
      	"start" => fun(_TS)->		1817	 		end,
      	"end" => fun(_TS)->			1850			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL555_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL555_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL555_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL555_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL555_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"31_30_3" => #{
      	"start" => fun(_TS)->		31	 		end,
      	"end" => fun(_TS)->			30			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/EEC_500_blok_5-8_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EEC_500_blok_5-8_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EEC_500_blok_5-8_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/EEC_500_blok_5-8_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/EEC_500_blok_5-8_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"241_7201_3" => #{
      	"start" => fun(_TS)->		241	 		end,
      	"end" => fun(_TS)->			7201 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_uk-pu_semey_uk_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_uk-pu_semey_uk_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_uk-pu_semey_uk_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_uk-pu_semey_uk_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_uk-pu_semey_uk_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"981_7201_3" => #{
      	"start" => fun(_TS)->		981	 		end,
      	"end" => fun(_TS)->			7201 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_semey-pu_semey_uk_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_semey-pu_semey_uk_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_semey-pu_semey_uk_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_semey-pu_semey_uk_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_semey-pu_semey_uk_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"981_7202_3" => #{
      	"start" => fun(_TS)->		981	 		end,
      	"end" => fun(_TS)->			7202 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_semey-pu_eec_semey_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_semey-pu_eec_semey_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_semey-pu_eec_semey_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_semey-pu_eec_semey_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_semey-pu_eec_semey_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"33_7202_3" => #{
      	"start" => fun(_TS)->		33	 		end,
      	"end" => fun(_TS)->			7202 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_eec-pu_eec_semey_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_eec-pu_eec_semey_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_eec-pu_eec_semey_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_eec-pu_eec_semey_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_eec-pu_eec_semey_F/futureP/predict_q_end">>, TS ) 	end
      	},


		
		"33_7203_3" => #{
      	"start" => fun(_TS)->		33	 		end,
      	"end" => fun(_TS)->			7203 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_eec-pu_egres_eec_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_eec-pu_egres_eec_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_eec-pu_egres_eec_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_eec-pu_egres_eec_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_eec-pu_egres_eec_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"39_7203_3" => #{
      	"start" => fun(_TS)->		39	 		end,
      	"end" => fun(_TS)->			7203 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_egres-pu_egres_eec_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_egres-pu_egres_eec_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_egres-pu_egres_eec_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_egres-pu_egres_eec_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_egres-pu_egres_eec_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"39_7204_3" => #{
      	"start" => fun(_TS)->		39	 		end,
      	"end" => fun(_TS)->			7204 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_egres_1-pu_cgpp_egres_1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_egres_1-pu_cgpp_egres_1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_egres_1-pu_cgpp_egres_1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_egres_1-pu_cgpp_egres_1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_egres_1-pu_cgpp_egres_1_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"130_7204_3" => #{
      	"start" => fun(_TS)->		130	 		end,
      	"end" => fun(_TS)->			7204 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_cgpp-pu_cgpp_egres_1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_cgpp-pu_cgpp_egres_1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_cgpp-pu_cgpp_egres_1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_cgpp-pu_cgpp_egres_1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_cgpp-pu_cgpp_egres_1_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"130_7205_3" => #{
      	"start" => fun(_TS)->		130	 		end,
      	"end" => fun(_TS)->			7205 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_cgpp-pu_cgpp_oskarovka_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_cgpp-pu_cgpp_oskarovka_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_cgpp-pu_cgpp_oskarovka_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_cgpp-pu_cgpp_oskarovka_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_cgpp-pu_cgpp_oskarovka_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"306_7205_3" => #{
      	"start" => fun(_TS)->		306	 		end,
      	"end" => fun(_TS)->			7205 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_cgpp_oskarovka_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"39_7206_3" => #{
      	"start" => fun(_TS)->		39	 		end,
      	"end" => fun(_TS)->			7206 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_egres_1-pu_egres_1_oskarovka_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_egres_1-pu_egres_1_oskarovka_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_egres_1-pu_egres_1_oskarovka_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_egres_1-pu_egres_1_oskarovka_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_egres_1-pu_egres_1_oskarovka_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"306_7206_3" => #{
      	"start" => fun(_TS)->		306	 		end,
      	"end" => fun(_TS)->			7206 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_egres_1_oskarovka_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"306_7229_3" => #{
      	"start" => fun(_TS)->		306	 		end,
      	"end" => fun(_TS)->			7229 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_oskarovka-pu_kargres_2_oskarovka_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"310_7208_3" => #{
      	"start" => fun(_TS)->		310	 		end,
      	"end" => fun(_TS)->			7208 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_nura-pu_kargres_nura_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_nura-pu_kargres_nura_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_nura-pu_kargres_nura_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_nura-pu_kargres_nura_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_nura-pu_kargres_nura_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"355_7208_3" => #{
      	"start" => fun(_TS)->		355	 		end,
      	"end" => fun(_TS)->			7208 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_nura_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_nura_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_nura_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_nura_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_nura_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"355_7209_3" => #{
      	"start" => fun(_TS)->		355	 		end,
      	"end" => fun(_TS)->			7209 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_karazhal_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_karazhal_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_karazhal_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_karazhal_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_karazhal_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"465_7209_3" => #{
      	"start" => fun(_TS)->		465	 		end,
      	"end" => fun(_TS)->			7209 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_karazhal-pu_kargres_karazhal_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_karazhal-pu_kargres_karazhal_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_karazhal-pu_kargres_karazhal_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_karazhal-pu_kargres_karazhal_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_karazhal-pu_kargres_karazhal_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"355_7210_3" => #{
      	"start" => fun(_TS)->		355	 		end,
      	"end" => fun(_TS)->			7210 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_agadyr_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_agadyr_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_agadyr_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_agadyr_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_agadyr_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"468_7210_3" => #{
      	"start" => fun(_TS)->		468	 		end,
      	"end" => fun(_TS)->			7210 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_agadyr-pu_kargres_agadyr _F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_agadyr-pu_kargres_agadyr _F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_agadyr-pu_kargres_agadyr _F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_agadyr-pu_kargres_agadyr _F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_agadyr-pu_kargres_agadyr _F/futureP/predict_q_end">>, TS ) 	end
      	},


		"355_7211_3" => #{
      	"start" => fun(_TS)->		355	 		end,
      	"end" => fun(_TS)->			7211 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_balkhashskaya_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_balkhashskaya_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_balkhashskaya_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_balkhashskaya_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres-pu_kargres_balkhashskaya_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"473_7211_3" => #{
      	"start" => fun(_TS)->		473	 		end,
      	"end" => fun(_TS)->			7211 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_balkhashskaya-pu_kargres_balkhashskaya_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"901_7212_3" => #{
      	"start" => fun(_TS)->		901	 		end,
      	"end" => fun(_TS)->			7212 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_ukgres-pu_ukgres_shu_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ukgres-pu_ukgres_shu_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ukgres-pu_ukgres_shu_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ukgres-pu_ukgres_shu_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_ukgres-pu_ukgres_shu_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"932_7212_3" => #{
      	"start" => fun(_TS)->		932	 		end,
      	"end" => fun(_TS)->			7212 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_shu-pu_ukgres_shu_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_shu-pu_ukgres_shu_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_shu-pu_ukgres_shu_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_shu-pu_ukgres_shu_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_shu-pu_ukgres_shu_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"903_7213_3" => #{
      	"start" => fun(_TS)->		903 		end,
      	"end" => fun(_TS)->			7213 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_almaty-pu_almaty_alma_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_almaty-pu_almaty_alma_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_almaty-pu_almaty_alma_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_almaty-pu_almaty_alma_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_almaty-pu_almaty_alma_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"939_7213_3" => #{
      	"start" => fun(_TS)->		939 		end,
      	"end" => fun(_TS)->			7213 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_alma-pu_almaty_alma_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_alma-pu_almaty_alma_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_alma-pu_almaty_alma_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_alma-pu_almaty_alma_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_alma-pu_almaty_alma_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"1916_7214_3" => #{
      	"start" => fun(_TS)->		1916 		end,
      	"end" => fun(_TS)->			7214 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_glavnaya-pu_glavnaya_kemin_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_glavnaya-pu_glavnaya_kemin_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_glavnaya-pu_glavnaya_kemin_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_glavnaya-pu_glavnaya_kemin_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_glavnaya-pu_glavnaya_kemin_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"2952_7214_3" => #{
      	"start" => fun(_TS)->		2952 		end,
      	"end" => fun(_TS)->			7214 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_kemin-pu_glavnaya_kemin _F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kemin-pu_glavnaya_kemin _F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kemin-pu_glavnaya_kemin _F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kemin-pu_glavnaya_kemin _F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_kemin-pu_glavnaya_kemin _F/futureP/predict_q_end">>, TS ) 	end
      	},


		"2952_7215_3" => #{
      	"start" => fun(_TS)->		2952 		end,
      	"end" => fun(_TS)->			7215 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_kemin-pu_kemin_frunze_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kemin-pu_kemin_frunze_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kemin-pu_kemin_frunze_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kemin-pu_kemin_frunze_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_kemin-pu_kemin_frunze_F/futureP/predict_q_end">>, TS ) 	end
      	},


		
		"2918_7215_3" => #{
      	"start" => fun(_TS)->		2918 		end,
      	"end" => fun(_TS)->			7215 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-pu_kemin_frunze_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-pu_kemin_frunze_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-pu_kemin_frunze_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-pu_kemin_frunze_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-pu_kemin_frunze_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"801_7216_3" => #{
      	"start" => fun(_TS)->		801 		end,
      	"end" => fun(_TS)->			7216 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl-pu_shymkent_zhambyl_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"831_7216_3" => #{
      	"start" => fun(_TS)->		831 		end,
      	"end" => fun(_TS)->			7216 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_shymkent-pu_shymkent_zhambyl_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_shymkent-pu_shymkent_zhambyl_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_shymkent-pu_shymkent_zhambyl_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_shymkent-pu_shymkent_zhambyl_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_shymkent-pu_shymkent_zhambyl_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"2918_7217_3" => #{
      	"start" => fun(_TS)->		2918 		end,
      	"end" => fun(_TS)->			7217 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-pu_glavnaya_frunze_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-pu_glavnaya_frunze_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-pu_glavnaya_frunze_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-pu_glavnaya_frunze_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-pu_glavnaya_frunze_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"1916_7217_3" => #{
      	"start" => fun(_TS)->		1916 		end,
      	"end" => fun(_TS)->			7217 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_glavnaya-pu_glavnaya_frunze_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_glavnaya-pu_glavnaya_frunze_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_glavnaya-pu_glavnaya_frunze_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_glavnaya-pu_glavnaya_frunze_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_glavnaya-pu_glavnaya_frunze_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"907_7218_3" => #{
      	"start" => fun(_TS)->		907 		end,
      	"end" => fun(_TS)->			7218 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_robot-pu_taldykorgan_robot_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_robot-pu_taldykorgan_robot_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_robot-pu_taldykorgan_robot_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_robot-pu_taldykorgan_robot_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_robot-pu_taldykorgan_robot_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"988_7218_3" => #{
      	"start" => fun(_TS)->		988 		end,
      	"end" => fun(_TS)->			7218 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_taldykorgan-pu_taldykorgan_robot_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"475_7219_3" => #{
      	"start" => fun(_TS)->		475 		end,
      	"end" => fun(_TS)->			7219 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_moiynty-pu_moiynty_ukgres_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_moiynty-pu_moiynty_ukgres_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_moiynty-pu_moiynty_ukgres_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_moiynty-pu_moiynty_ukgres_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_moiynty-pu_moiynty_ukgres_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"901_7219_3" => #{
      	"start" => fun(_TS)->		901		end,
      	"end" => fun(_TS)->			7219 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_ukgres-pu_moiynty_ukgres_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ukgres-pu_moiynty_ukgres_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ukgres-pu_moiynty_ukgres_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ukgres-pu_moiynty_ukgres_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_ukgres-pu_moiynty_ukgres_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"465_468_3" => #{
      	"start" => fun(_TS)->		465	 		end,
      	"end" => fun(_TS)->			468			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2428_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2428_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2428_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2428_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2428_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"465_7221_3" => #{
      	"start" => fun(_TS)->		465		end,
      	"end" => fun(_TS)->			7221 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_karazhal-pu_zhezkazgan_karazhal_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"466_7221_3" => #{
      	"start" => fun(_TS)->		466		end,
      	"end" => fun(_TS)->			7221 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_zhezkazgan-pu_zhezkazgan_karazhal_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"831_7228_3" => #{
      	"start" => fun(_TS)->		831		end,
      	"end" => fun(_TS)->			7228 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2309_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2309_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2309_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2309_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2309_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"7228_840_3" => #{
      	"start" => fun(_TS)->		7228	 		end,
      	"end" => fun(_TS)->			840			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2439_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2439_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2439_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2439_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2439_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"840_869_3" => #{
      	"start" => fun(_TS)->		840	 		end,
      	"end" => fun(_TS)->			869 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2519_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2519_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2519_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2519_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2519_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"869_7226_3" => #{
      	"start" => fun(_TS)->		869	 		end,
      	"end" => fun(_TS)->			7226 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_ru_6-pu_ru_6_kyzylorda_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"862_7227_3" => #{
      	"start" => fun(_TS)->		862	 		end,
      	"end" => fun(_TS)->			7227 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_kyzylorda-pu_kyzylorda_kumkol_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"466_467_3" => #{
      	"start" => fun(_TS)->		466	 		end,
      	"end" => fun(_TS)->			467 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_zhezkazgan-kumkol_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhezkazgan-kumkol_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhezkazgan-kumkol_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhezkazgan-kumkol_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_zhezkazgan-kumkol_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"928_904_3" => #{
      	"start" => fun(_TS)->		928	 		end,
      	"end" => fun(_TS)->			904 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2023_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2023_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2023_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2023_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2023_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"903_928_3" => #{
      	"start" => fun(_TS)->		903	 		end,
      	"end" => fun(_TS)->			928 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2773_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2773_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2773_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2773_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2773_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"953_904_3" => #{
      	"start" => fun(_TS)->		953	 		end,
      	"end" => fun(_TS)->			904 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_koyan_kos-atets_3_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_koyan_kos-atets_3_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_koyan_kos-atets_3_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_koyan_kos-atets_3_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_koyan_kos-atets_3_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"467_7227_3" => #{
      	"start" => fun(_TS)->		467	 		end,
      	"end" => fun(_TS)->			7227		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_kumkol-pu_kyzylorda_kumkol_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"801_805_3" => #{
      	"start" => fun(_TS)->		801 		end,
      	"end" => fun(_TS)->			805			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl_zhgres/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl_zhgres/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl_zhgres/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl_zhgres/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl_zhgres/futureP/predict_q_end">>, TS ) 	end
      	},


		"932_814_3" => #{
      	"start" => fun(_TS)->		932	 		end,
      	"end" => fun(_TS)->			814 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2233_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2233_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2233_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2233_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2233_F/futureP/predict_q_end">>, TS ) 	end
      	},


		
		"814_805_3" => #{
      	"start" => fun(_TS)->		814	 		end,
      	"end" => fun(_TS)->			805 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2249_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2249_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2249_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2249_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2249_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"7223_805_3" => #{
      	"start" => fun(_TS)->		7223	 		end,
      	"end" => fun(_TS)->			805 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_zhgres-pu_frunze_zhgres_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhgres-pu_frunze_zhgres_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhgres-pu_frunze_zhgres_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhgres-pu_frunze_zhgres_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_zhgres-pu_frunze_zhgres_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"7223_2918_3" => #{
      	"start" => fun(_TS)->		7223	 		end,
      	"end" => fun(_TS)->			2918 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_pu_zhgres_frunze-frunze220_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_pu_zhgres_frunze-frunze220_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_pu_zhgres_frunze-frunze220_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_pu_zhgres_frunze-frunze220_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_pu_zhgres_frunze-frunze220_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"839_843_3" => #{
      	"start" => fun(_TS)->		839	 		end,
      	"end" => fun(_TS)->			843 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2449_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2449_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2449_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2449_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2449_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"839_7225_3" => #{
      	"start" => fun(_TS)->		839	 		end,
      	"end" => fun(_TS)->			7225 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_zhylga-pu_shymkent_zhylga_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhylga-pu_shymkent_zhylga_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhylga-pu_shymkent_zhylga_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhylga-pu_shymkent_zhylga_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_zhylga-pu_shymkent_zhylga_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"839_2925_3" => #{
      	"start" => fun(_TS)->		839	 		end,
      	"end" => fun(_TS)->			2925 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2429_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2429_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2429_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2429_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2429_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"2925_843_3" => #{
      	"start" => fun(_TS)->		2925	 	end,
      	"end" => fun(_TS)->			843			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2419_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2419_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2419_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2419_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2419_F/futureP/predict_q_end">>, TS ) 	end
      	},

		
		"831_843_3" => #{
      	"start" => fun(_TS)->		831	 		end,
      	"end" => fun(_TS)->			843 		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2349_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2349_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2349_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2349_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2349_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"842_843_3" => #{
      	"start" => fun(_TS)->		842	 		end,
      	"end" => fun(_TS)->			843 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2319_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2319_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2319_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2319_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2319_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"842_840_3" => #{
      	"start" => fun(_TS)->		842	 		end,
      	"end" => fun(_TS)->			840			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2549_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2549_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2549_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2549_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2549_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"840_7224_3" => #{
      	"start" => fun(_TS)->		840	 		end,
      	"end" => fun(_TS)->			7224 			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_kentau-pu_kentau_zhambyl_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kentau-pu_kentau_zhambyl_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kentau-pu_kentau_zhambyl_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kentau-pu_kentau_zhambyl_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_kentau-pu_kentau_zhambyl_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"7224_801_3" => #{
      	"start" => fun(_TS)->		7224 		end,
      	"end" => fun(_TS)->			801			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl-pu_kentau_zhambyl_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl-pu_kentau_zhambyl_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl-pu_kentau_zhambyl_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl-pu_kentau_zhambyl_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_zhambyl-pu_kentau_zhambyl_F/futureP/predict_q_end">>, TS ) 	end
      	},



		"2919_2921_3" => #{
      	"start" => fun(_TS)->		2919		end,
      	"end" => fun(_TS)->			2921		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-ca_1_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-ca_1_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-ca_1_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-ca_1_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_frunze-ca_1_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"865_842_3" => #{
      	"start" => fun(_TS)->		865	 		end,
      	"end" => fun(_TS)->			842			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2539_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2539_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2539_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2539_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2539_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"865_869_3" => #{
      	"start" => fun(_TS)->		865	 		end,
      	"end" => fun(_TS)->			869			end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2529_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2529_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2529_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2529_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2529_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"831_7225_3" => #{
      	"start" => fun(_TS)->		831	 		end,
      	"end" => fun(_TS)->			7225		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_shymkent-pu_shymkent_zhylga_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_shymkent-pu_shymkent_zhylga_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_shymkent-pu_shymkent_zhylga_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_shymkent-pu_shymkent_zhylga_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_shymkent-pu_shymkent_zhylga_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"862_7226_3" => #{
      	"start" => fun(_TS)->		862 		end,
      	"end" => fun(_TS)->			7226		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_kyzylorda-pu_ru_6_kyzylorda_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"355_7229_3" => #{
      	"start" => fun(_TS)->		355	 		end,
      	"end" => fun(_TS)->			7229		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ekv_kargres_2-pu_kargres_2_oskarovka_F/futureP/predict_q_end">>, TS ) 	end
      	},




        "355_356_3" => #{
      	"start" => fun(_TS)->		355	 		end,
      	"end" => fun(_TS)->			356		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Kar.GRES-2_AT_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Kar.GRES-2_AT_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Kar.GRES-2_AT_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Kar.GRES-2_AT_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Kar.GRES-2_AT_F/futureP/predict_q_end">>, TS ) 	end
      	},
        



        "904_905_3" => #{
      	"start" => fun(_TS)->		904 		end,
      	"end" => fun(_TS)->			905		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/ATETS-3_AT_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/ATETS-3_AT_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/ATETS-3_AT_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/ATETS-3_AT_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/ATETS-3_AT_F/futureP/predict_q_end">>, TS ) 	end
      	},



        "912_913_3" => #{
      	"start" => fun(_TS)->		912 		end,
      	"end" => fun(_TS)->			913		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/PS7-AHBK_AT_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/PS7-AHBK_AT_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/PS7-AHBK_AT_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/PS7-AHBK_AT_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/PS7-AHBK_AT_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "939_911_3" => #{
      	"start" => fun(_TS)->		939	 		end,
      	"end" => fun(_TS)->			911		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2433_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2433_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2433_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2433_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2433_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "935_911_3" => #{
      	"start" => fun(_TS)->		935	 		end,
      	"end" => fun(_TS)->			911		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2373_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2373_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2373_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2373_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2373_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "935_907_3" => #{
      	"start" => fun(_TS)->		935	 		end,
      	"end" => fun(_TS)->			907		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL2383_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2383_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2383_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL2383_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL2383_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "473_472_3" => #{
      	"start" => fun(_TS)->		473 		end,
      	"end" => fun(_TS)->			472		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Balkhashskaya_AT_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Balkhashskaya_AT_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Balkhashskaya_AT_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Balkhashskaya_AT_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Balkhashskaya_AT_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "467_455_3" => #{
      	"start" => fun(_TS)->		467 		end,
      	"end" => fun(_TS)->			455		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/Kumkol_AT_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Kumkol_AT_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Kumkol_AT_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/Kumkol_AT_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/Kumkol_AT_F/futureP/predict_q_end">>, TS ) 	end
      	},
        

		"1660_1631_3" => #{
      	"start" => fun(_TS)->		1660 		end,
      	"end" => fun(_TS)->			1631		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL533_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL533_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL533_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL533_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL533_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "4799_576_3" => #{
      	"start" => fun(_TS)->		4799	end,
      	"end" => fun(_TS)->			576		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5716_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5716_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5716_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5716_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5716_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "4790_577_3" => #{
      	"start" => fun(_TS)->		4790 		end,
      	"end" => fun(_TS)->			577		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5736_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5736_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5736_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5736_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5736_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "4790_4785_3" => #{
      	"start" => fun(_TS)->		4790 		end,
      	"end" => fun(_TS)->			4785	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_irgres_magnitrf_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_irgres_magnitrf_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_irgres_magnitrf_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_irgres_magnitrf_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_irgres_magnitrf_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "4785_4799_3" => #{
      	"start" => fun(_TS)->		4785		end,
      	"end" => fun(_TS)->			4799	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_magnitrf_trgres_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_magnitrf_trgres_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_magnitrf_trgres_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_magnitrf_trgres_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_magnitrf_trgres_F/futureP/predict_q_end">>, TS ) 	end
      	},


		"10000_4799_3" => #{
      	"start" => fun(_TS)->		10000 		end,
      	"end" => fun(_TS)->			4799		end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_ujurgres_trgres_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ujurgres_trgres_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ujurgres_trgres_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_ujurgres_trgres_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_ujurgres_trgres_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "4782_10000_3" => #{
      	"start" => fun(_TS)->		4782 		end,
      	"end" => fun(_TS)->			10000	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_shagol_ujurgres_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_shagol_ujurgres_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_shagol_ujurgres_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_shagol_ujurgres_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_shagol_ujurgres_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "4782_4705_3" => #{
      	"start" => fun(_TS)->		4782		end,
      	"end" => fun(_TS)->			4705	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_shagol_cheliabinsk_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_shagol_cheliabinsk_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_shagol_cheliabinsk_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_shagol_cheliabinsk_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_shagol_cheliabinsk_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "590_4705_3" => #{
      	"start" => fun(_TS)->		590 		end,
      	"end" => fun(_TS)->			4705	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL1103_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL1103_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL1103_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL1103_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL1103_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "4703_4782_3" => #{
      	"start" => fun(_TS)->		4703		end,
      	"end" => fun(_TS)->			4782	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_kozirevo_shagol_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_kozirevo_shagol_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_kozirevo_shagol_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_kozirevo_shagol_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_kozirevo_shagol_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "4703_4727_3" => #{
      	"start" => fun(_TS)->		4703 		end,
      	"end" => fun(_TS)->			4727	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL_kozirevo_kurgan_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_kozirevo_kurgan_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_kozirevo_kurgan_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL_kozirevo_kurgan_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL_kozirevo_kurgan_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "147_4727_3" => #{
      	"start" => fun(_TS)->		147 		end,
      	"end" => fun(_TS)->			4727	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL5201_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5201_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5201_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL5201_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL5201_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "4727_1852_3" => #{
      	"start" => fun(_TS)->		4727		end,
      	"end" => fun(_TS)->			1852	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL559_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL559_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL559_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL559_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL559_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "1852_1853_3" => #{
      	"start" => fun(_TS)->		1852		end,
      	"end" => fun(_TS)->			1853	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL558_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL558_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL558_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL558_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL558_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "1853_1817_3" => #{
      	"start" => fun(_TS)->		1853		end,
      	"end" => fun(_TS)->			1817	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL567_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL567_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL567_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL567_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL567_F/futureP/predict_q_end">>, TS ) 	end
      	},


	     "1853_9917_3" => #{
      	"start" => fun(_TS)->		1853		end,
      	"end" => fun(_TS)->			9917	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL534_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL534_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL534_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL534_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL534_F/futureP/predict_q_end">>, TS ) 	end
      	},


        "1631_9917_3" => #{
      	"start" => fun(_TS)->		1631		end,
      	"end" => fun(_TS)->			9917	end,
      	"num" => fun(_TS)->			3			end,
      	"state" => fun(TS)->  		wacs_snapshot:read_archive(   <<"/VL532_F/futureP/predict_state_b">>, TS ) 	end,
     	"Pn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL532_F/futureP/predict_p_start">>, TS ) 	end,
     	"Qn" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL532_F/futureP/predict_q_start">>, TS ) 	end,
     	"Pk" => fun(_TS)-> 	   		0   end, % wacs_snapshot:read_archive(   <<"/VL532_F/futureP/predict_p_end">>, TS ) 	end,
      	"Qk" => fun(_TS)-> 	   		0   end % wacs_snapshot:read_archive(   <<"/VL532_F/futureP/predict_q_end">>, TS ) 	end
      	}
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END LINES (BRANCHES)
    },

    %----------------------------------------------------------------------------------
    %     NODES REACT
    %----------------------------------------------------------------------------------
    "nr" => #{

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %	"_____" => #{  %% номер узла _ номер реактора
      %		"node_num" => fun(_TS)-> 	__ 		end,
      %		"num" => fun(_TS)-> 		__ 		end,
      %		"conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/nodes_react/______/rtag">>, 	<<"b_r">> ) 			end,
      %		"state" => fun(TS)->	wacs_snapshot:read_archive(   <<"/nodes_react/_____/futureP/predict_state_r">>, TS ) 			end,
      %		"weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/nodes_react/____/rtag">>, 	<<"wt_reactor_r">> ) 	end
      %	},

      "26_1" => #{  %% номер узла _ номер реактора
        "node_num" => fun(_TS)-> 	26 		end,
        "num" => fun(_TS)-> 		1 		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/nodes_react/N_26-1_F/rtag">>, 	<<"b_r">> ) 			end,
        "state" => fun(TS)->	wacs_snapshot:read_archive(   <<"/nodes_react/N_26-1_F/futureP/predict_state_r">>, TS ) 			end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/nodes_react/N_26-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


      "9932_2" => #{  %% номер узла _ номер реактора
        "node_num" => fun(_TS)-> 	9932 	end,
        "num" => fun(_TS)-> 		2 		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/nodes_react/N_9932-2_F/rtag">>, 	<<"b_r">> ) 			end,
        "state" => fun(TS)->	wacs_snapshot:read_archive(   <<"/nodes_react/N_9932-2_F/futureP/predict_state_r">>, TS ) 			end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/nodes_react/N_9932-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },

      "2919_2" => #{  %% номер узла _ номер реактора
        "node_num" => fun(_TS)-> 	2919 	end,
        "num" => fun(_TS)-> 		2 		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/nodes_react/N_2919-2_F/rtag">>, 	<<"b_r">> ) 			end,
        "state" => fun(TS)->	wacs_snapshot:read_archive(   <<"/nodes_react/N_2919-2_F/futureP/predict_state_r">>, TS ) 			end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/nodes_react/N_2919-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },

      "1621_2" => #{  %% номер узла _ номер реактора
        "node_num" => fun(_TS)-> 	1621 	end,
        "num" => fun(_TS)-> 		2 		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/nodes_react/N_1621-2_F/rtag">>, 	<<"b_r">> ) 			end,
        "state" => fun(TS)->	wacs_snapshot:read_archive(   <<"/nodes_react/N_1621-2_F/futureP/predict_state_r">>, TS ) 			end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/nodes_react/N_1621-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


      "180_3" => #{  %% номер узла _ номер реактора
        "node_num" => fun(_TS)-> 	180		end,
        "num" => fun(_TS)-> 		3 		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/nodes_react/N_180-3_F/rtag">>, 	<<"b_r">> ) 			end,
        "state" => fun(TS)->	wacs_snapshot:read_archive(   <<"/nodes_react/N_180-3_F/futureP/predict_state_r">>, TS ) 			end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/nodes_react/N_180-3_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },

      "1631_1" => #{  %% номер узла _ номер реактора
        "node_num" => fun(_TS)-> 	1631	end,
        "num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/nodes_react/N_1631-1_F/rtag">>, 	<<"b_r">> ) 			end,
        "state" => fun(TS)->	wacs_snapshot:read_archive(   <<"/nodes_react/N_1631-1_F/futureP/predict_state_r">>, TS ) 			end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/nodes_react/N_1631-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },

    
      "1817_1" => #{  %% номер узла _ номер реактора
        "node_num" => fun(_TS)-> 	1817	end,
        "num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/nodes_react/N_1817-1_F/rtag">>, 	<<"b_r">> ) 			end,
        "state" => fun(TS)->	wacs_snapshot:read_archive(   <<"/nodes_react/N_1817-1_F/futureP/predict_state_r">>, TS ) 			end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/nodes_react/N_1817-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },

      "1850_1" => #{  %% номер узла _ номер реактора
        "node_num" => fun(_TS)-> 	1850 	end,
        "num" => fun(_TS)-> 		1 		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/nodes_react/N_1850-1_F/rtag">>, 	<<"b_r">> ) 			end,
        "state" => fun(TS)->	wacs_snapshot:read_archive(   <<"/nodes_react/N_1850-1_F/futureP/predict_state_r">>, TS ) 			end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/nodes_react/N_1850-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },

	   "1817_2" => #{  %% номер узла _ номер реактора
        "node_num" => fun(_TS)-> 	1817	end,
        "num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/nodes_react/N_1817-2_F/rtag">>, 	<<"b_r">> ) 			end,
        "state" => fun(TS)->	wacs_snapshot:read_archive(   <<"/nodes_react/N_1817-2_F/futureP/predict_state_r">>, TS ) 			end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/nodes_react/N_1817-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },

      "938_2" => #{  %% номер узла _ номер реактора
        "node_num" => fun(_TS)-> 	938	end,
        "num" => fun(_TS)-> 		2 		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/nodes_react/N_938-2_F/rtag">>, 	<<"b_r">> ) 			end,
        "state" => fun(TS)->	wacs_snapshot:read_archive(   <<"/nodes_react/N_938-2_F/futureP/predict_state_r">>, TS ) 			end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/nodes_react/N_938-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      }
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END NODES REACT

    },
    %----------------------------------------------------------------------------------
    %     LINKS REACT
    %----------------------------------------------------------------------------------
    "lr" => #{

      %	"_______" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
      %		"start" => fun(_TS)-> 		__ 		end,
      %		"end" => fun(_TS)-> 		__		end,
      %		"num" => fun(_TS)-> 		__ 		end,
      %		"r_num" => fun(_TS)-> 		__ 		end,
      %		"conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/root/PROJECT/TAGS/______/rtag">>, 	<<"b_r">> ) 				end,
      %		"state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/______/futureP/predict_state_r">>, 	TS ) 		end,
      %		"weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/______/rtag">>, 	<<"wt_reactor_r">> ) 	end
      %	},

      
      "26_1660_3_4" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		26 		end,
        "end" => fun(_TS)-> 		1660	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		4 		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_26-1660-3-4_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_26-1660-3-4_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_26-1660-3-4_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "26_1660_3_5" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		26 		end,
        "end" => fun(_TS)-> 		1660	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		5		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_26-1660-3-5_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_26-1660-3-5_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_26-1660-3-5_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


       "26_469_3_3" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		26 		end,
        "end" => fun(_TS)-> 		469	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		3		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_26-469-3-3_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_26-469-3-3_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_26-469-3-3_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },



		"26_980_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		26 		end,
        "end" => fun(_TS)-> 		980	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_26-980-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_26-980-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_26-980-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	  	"26_175_3_6" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		26 		end,
        "end" => fun(_TS)-> 		175		end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		6		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_26-175-3-6_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_26-175-3-6_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_26-175-3-6_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "25_129_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		25 		end,
        "end" => fun(_TS)-> 		129	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_25-129-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_25-129-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_25-129-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "25_325_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		25 		end,
        "end" => fun(_TS)-> 		325	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_25-325-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_25-325-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_25-325-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "25_1817_3_3" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		25 		end,
        "end" => fun(_TS)-> 		1817	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		3		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_25-1817-3-3_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_25-1817-3-3_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_25-1817-3-3_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "325_25_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		325		end,
        "end" => fun(_TS)-> 		25  	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_325-25-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_325-25-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_325-25-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "469_325_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		469 	end,
        "end" => fun(_TS)-> 		325 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_469-325-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_469-325-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_469-325-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "469_900_1_5" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		469		end,
        "end" => fun(_TS)-> 		900 	end,
        "num" => fun(_TS)-> 		1 		end,
        "r_num" => fun(_TS)-> 		5		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_469-900-1-5_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_469-900-1-5_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_469-900-1-5_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },

	    "469_26_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		469 		end,
        "end" => fun(_TS)-> 		26	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_469-26-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_469-26-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_469-26-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "469_26_3_4" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		469 		end,
        "end" => fun(_TS)-> 		26	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		4		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_469-26-3-4_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_469-26-3-4_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_469-26-3-4_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "469_900_2_5" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		469 		end,
        "end" => fun(_TS)-> 		900	end,
        "num" => fun(_TS)-> 		2 		end,
        "r_num" => fun(_TS)-> 		5		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_469-900-2-5_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_469-900-2-5_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_469-900-2-5_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },  


	    "900_469_1_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		900 	end,
        "end" => fun(_TS)-> 		469 	end,
        "num" => fun(_TS)-> 		1 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_900-469-1-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_900-469-1-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_900-469-1-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


       "900_469_2_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		900		end,
        "end" => fun(_TS)-> 		469 	end,
        "num" => fun(_TS)-> 		2 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_900-469-2-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_900-469-2-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_900-469-2-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },

		"900_902_3_3" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		900		end,
        "end" => fun(_TS)-> 		902	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		3		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_900-902-3-3_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_900-902-3-3_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_900-902-3-3_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


        "900_938_3_5" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		900		end,
        "end" => fun(_TS)-> 		938	    end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		5		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_900-938-3-5_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_900-938-3-5_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_900-938-3-5_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	  	"902_900_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		902 		end,
        "end" => fun(_TS)-> 		900		end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_902-900-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_902-900-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_902-900-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "902_9932_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		902 		end,
        "end" => fun(_TS)-> 		9932	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_902-9932-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_902-9932-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_902-9932-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "938_900_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		938		end,
        "end" => fun(_TS)-> 		900 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_938-900-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_938-900-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_938-900-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "9932_900_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		9932	end,
        "end" => fun(_TS)-> 		900 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_9932-900-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_9932-900-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_9932-900-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "2919_9932_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		2919	end,
        "end" => fun(_TS)-> 		9932	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_2919-9932-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_2919-9932-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_2919-9932-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "800_2919_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		800 		end,
        "end" => fun(_TS)-> 		2919	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_800-2919-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_800-2919-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_800-2919-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "800_830_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		800 	end,
        "end" => fun(_TS)-> 		830	    end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_800-830-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_800-830-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_800-830-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },

	    "980_26_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		980		end,
        "end" => fun(_TS)-> 		26	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_980-26-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_980-26-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_980-26-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "980_240_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		980		end,
        "end" => fun(_TS)-> 		240 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_980-240-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_980-240-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_980-240-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "986_980_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		986		end,
        "end" => fun(_TS)-> 		980 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_986-980-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_986-980-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_986-980-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "987_986_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		987		end,
        "end" => fun(_TS)-> 		986 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_987-986-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_987-986-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_987-986-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


       "129_180_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		129		end,
        "end" => fun(_TS)-> 		180 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_129-180-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_129-180-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_129-180-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },



		"129_25_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		129 	end,
        "end" => fun(_TS)-> 		25  	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_129-25-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_129-25-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_129-25-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	  	"31_1850_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		31 		end,
        "end" => fun(_TS)-> 		1850	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_31-1850-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_31-1850-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_31-1850-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "1621_31_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		1621	end,
        "end" => fun(_TS)-> 		31  	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_1621-31-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_1621-31-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_1621-31-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "1660_26_3_3" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		1660	end,
        "end" => fun(_TS)-> 		26  	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		3		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_1660-26-3-3_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_1660-26-3-3_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_1660-26-3-3_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "1660_26_3_4" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		1660	end,
        "end" => fun(_TS)-> 		26  	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		4		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_1660-26-3-4_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_1660-26-3-4_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_1660-26-3-4_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "1630_1660_1_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		1630	end,
        "end" => fun(_TS)-> 		1660	end,
        "num" => fun(_TS)-> 		1 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_1630-1660-1-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_1630-1660-1-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_1630-1660-1-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	     "1630_1660_1_3" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		1630	end,
        "end" => fun(_TS)-> 		1660	end,
        "num" => fun(_TS)-> 		1 		end,
        "r_num" => fun(_TS)-> 		3		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_1630-1660-1-3_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_1630-1660-1-3_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_1630-1660-1-3_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },

	  
	     "1630_1621_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		1630	end,
        "end" => fun(_TS)-> 		1621	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_1630-1621-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_1630-1621-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_1630-1621-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "175_26_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		175		end,
        "end" => fun(_TS)-> 		26  	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_175-26-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_175-26-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_175-26-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "175_26_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		175		end,
        "end" => fun(_TS)-> 		26  	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_175-26-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_175-26-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_175-26-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "175_590_3_3" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		175		end,
        "end" => fun(_TS)-> 		590 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		3		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_175-590-3-3_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_175-590-3-3_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_175-590-3-3_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "175_590_3_4" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		175		end,
        "end" => fun(_TS)-> 		590 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		4		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_175-590-3-4_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_175-590-3-4_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_175-590-3-4_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "175_590_3_5" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		175		end,
        "end" => fun(_TS)-> 		590 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		5		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_175-590-3-5_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_175-590-3-5_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_175-590-3-5_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


       "175_26_3_6" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		175 	end,
        "end" => fun(_TS)-> 		26  	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		6		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_175-26-3-6_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_175-26-3-6_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_175-26-3-6_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },



		"590_175_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		590		end,
        "end" => fun(_TS)-> 		175 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_590-175-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_590-175-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_590-175-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	  	"576_577_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		576		end,
        "end" => fun(_TS)-> 		577  	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_576-577-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_576-577-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_576-577-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "576_180_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		576		end,
        "end" => fun(_TS)-> 		180 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_576-180-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_576-180-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_576-180-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "180_129_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		180		end,
        "end" => fun(_TS)-> 		129	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_180-129-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_180-129-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_180-129-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "180_576_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		180 	end,
        "end" => fun(_TS)-> 		576 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_180-576-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_180-576-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_180-576-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "480_469_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		480		end,
        "end" => fun(_TS)-> 		469 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_480-469-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_480-469-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_480-469-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "2921_2919_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		2921	end,
        "end" => fun(_TS)-> 		2919	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_2921-2919-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_2921-2919-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_2921-2919-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "147_175_3_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		147		end,
        "end" => fun(_TS)-> 		175 	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_147-175-3-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_147-175-3-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_147-175-3-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },

	    "147_1817_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		147		end,
        "end" => fun(_TS)-> 		1817	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_147-1817-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_147-1817-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_147-1817-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


	    "31_25_3_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		31 		end,
        "end" => fun(_TS)-> 		25  	end,
        "num" => fun(_TS)-> 		3 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_31-25-3-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_31-25-3-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_31-25-3-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },


       "1660_1631_1_1" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		1660 	end,
        "end" => fun(_TS)-> 		1631 	end,
        "num" => fun(_TS)-> 		1 		end,
        "r_num" => fun(_TS)-> 		1		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_1660-1631-1-1_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_1660-1631-1-1_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_1660-1631-1-1_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      },
        

		"1660_1631_2_2" => #{ %% номер начало узла _ конец узла _ номер цепи _ номер реактора
        "start" => fun(_TS)-> 		1660 	end,
        "end" => fun(_TS)-> 		1631 	end,
        "num" => fun(_TS)-> 		2 		end,
        "r_num" => fun(_TS)-> 		2		end,
        "conduct" => fun(_TS)-> 	wacs_snapshot:read_tag(   <<"/links_react/V_1660-1631-2-2_F/rtag">>, 	<<"b_r">> ) 				end,
        "state" => fun(TS)-> 	wacs_snapshot:read_archive(   <<"/links_react/V_1660-1631-2-2_F/futureP/predict_state_r">>, 	TS ) 		end,
        "weight" => fun(_TS)-> 		wacs_snapshot:read_tag(   <<"/links_react/V_1660-1631-2-2_F/rtag">>, 	<<"wt_reactor_r">> ) 	end
      }
       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END LINES REACT

    },

    %%StrSec = "/", %%% общий путь где будет лежать

    "pred" => #{
      "ss1" => #{
        "p_pp_uz_s" =>  null,
        "p_pp_kz_s" =>      null,
        "p_pp_kn_s" =>      null,
        "kzu_norm_s" =>     null,
        "kzu_jakobian_s" => null,
        "kn_s" =>           null,
        "p_mdp_kzu_s" =>    null,
        "p_mdp_kz_s" =>     null,
        "p_mdp_kn_s" =>     null
      },
      "ss2" => #{
        "p_pp_uz_s" => null,
        "p_pp_kz_s" => null,
        "p_pp_kn_s" => null,
        "kzu_norm_s" => null,
        "kzu_jakobian_s" => null,
        "kn_s" => null,
        "p_mdp_kzu_s" => null,
        "p_mdp_kz_s" => null,
        "p_mdp_kn_s" => null
      },

      "ss3" => #{
        "p_pp_uz_s" => null,
        "p_pp_kz_s" => null,
        "p_pp_kn_s" => null,
        "kzu_norm_s" => null,
        "kzu_jakobian_s" => null,
        "kn_s" => null,
        "p_mdp_kzu_s" => null,
        "p_mdp_kz_s" => null,
        "p_mdp_kn_s" => null
      }



      % TODO...
    },
    "drts" => {
      % TODO
    }
  }.

%%==========================================================================
%%    CSV templates
%%==========================================================================
csv_template( "nodes.csv" )->
  [
    [ {const,"node_number"},  {const,"node_state"}, {const,"U"},  {const,"fiU"},  {const,"node_Pn"},  {const,"node_Qn"},  {const,"node_Pg"},  {const,"node_Qg"},  {const,"Qmin"},   {const,"Qmax"}  ]
    %, [ "nodes/___/num",         "nodes/___/state",     "nodes/___/U", "nodes/___/fiU", "nodes/___/Pn",      "nodes/___/Qn",      "nodes/___/Pg",      "nodes/___/Qg",      "nodes/___/Qmin",  "nodes/___/Qmax" ]
    , [ "nodes/26/num",           "nodes/26/state",         "nodes/26/U",        "nodes/26/fiU",         "nodes/26/Pn",        "nodes/26/Qn",         "nodes/26/Pg",         "nodes/26/Qg",         "nodes/26/Qmin",         "nodes/26/Qmax" ]
    , [ "nodes/25/num",           "nodes/25/state",         "nodes/25/U",        "nodes/25/fiU",         "nodes/25/Pn",        "nodes/25/Qn",         "nodes/25/Pg",         "nodes/25/Qg",         "nodes/25/Qmin",         "nodes/25/Qmax" ]
    , [ "nodes/325/num",          "nodes/325/state",        "nodes/325/U",       "nodes/325/fiU",        "nodes/325/Pn",       "nodes/325/Qn",        "nodes/325/Pg",        "nodes/325/Qg",        "nodes/325/Qmin",        "nodes/325/Qmax" ]
    , [ "nodes/469/num",          "nodes/469/state",        "nodes/469/U",       "nodes/469/fiU",        "nodes/469/Pn",       "nodes/469/Qn",        "nodes/469/Pg",        "nodes/469/Qg",        "nodes/469/Qmin",        "nodes/469/Qmax" ]
    , [ "nodes/900/num",          "nodes/900/state",        "nodes/900/U",       "nodes/900/fiU",        "nodes/900/Pn",       "nodes/900/Qn",        "nodes/900/Pg",        "nodes/900/Qg",        "nodes/900/Qmin",        "nodes/900/Qmax" ]
    , [ "nodes/902/num",          "nodes/902/state",        "nodes/902/U",       "nodes/902/fiU",        "nodes/902/Pn",       "nodes/902/Qn",        "nodes/902/Pg",        "nodes/902/Qg",        "nodes/902/Qmin",        "nodes/902/Qmax" ]
	, [ "nodes/938/num",          "nodes/938/state",        "nodes/938/U",       "nodes/938/fiU",        "nodes/938/Pn",       "nodes/938/Qn",        "nodes/938/Pg",        "nodes/938/Qg",        "nodes/938/Qmin",        "nodes/938/Qmax" ]
	, [ "nodes/9932/num",         "nodes/9932/state",       "nodes/9932/U",      "nodes/9932/fiU",       "nodes/9932/Pn",      "nodes/9932/Qn",       "nodes/9932/Pg",       "nodes/9932/Qg",       "nodes/9932/Qmin",       "nodes/9932/Qmax" ]
	, [ "nodes/2919/num",         "nodes/2919/state",       "nodes/2919/U",      "nodes/2919/fiU",       "nodes/2919/Pn",      "nodes/2919/Qn",       "nodes/2919/Pg",       "nodes/2919/Qg",       "nodes/2919/Qmin",       "nodes/2919/Qmax" ]
	, [ "nodes/800/num",          "nodes/800/state",        "nodes/800/U",       "nodes/800/fiU",        "nodes/800/Pn",       "nodes/800/Qn",        "nodes/800/Pg",        "nodes/800/Qg",        "nodes/800/Qmin",        "nodes/800/Qmax" ]
	, [ "nodes/830/num",          "nodes/830/state",        "nodes/830/U",       "nodes/830/fiU",        "nodes/830/Pn",       "nodes/830/Qn",        "nodes/830/Pg",        "nodes/830/Qg",        "nodes/830/Qmin",        "nodes/830/Qmax" ]
	, [ "nodes/980/num",          "nodes/980/state",        "nodes/980/U",       "nodes/980/fiU",        "nodes/980/Pn",       "nodes/980/Qn",        "nodes/980/Pg",        "nodes/980/Qg",        "nodes/980/Qmin",        "nodes/980/Qmax" ]
	, [ "nodes/986/num",          "nodes/986/state",        "nodes/986/U",       "nodes/986/fiU",        "nodes/986/Pn",       "nodes/986/Qn",        "nodes/986/Pg",        "nodes/986/Qg",        "nodes/986/Qmin",        "nodes/986/Qmax" ]
	, [ "nodes/987/num",          "nodes/987/state",        "nodes/987/U",       "nodes/987/fiU",        "nodes/987/Pn",       "nodes/987/Qn",        "nodes/987/Pg",        "nodes/987/Qg",        "nodes/987/Qmin",        "nodes/987/Qmax" ]
	, [ "nodes/240/num",          "nodes/240/state",        "nodes/240/U",       "nodes/240/fiU",        "nodes/240/Pn",       "nodes/240/Qn",        "nodes/240/Pg",        "nodes/240/Qg",        "nodes/240/Qmin",        "nodes/240/Qmax" ]
  	, [ "nodes/224/num",          "nodes/224/state",        "nodes/224/U",       "nodes/224/fiU",        "nodes/224/Pn",       "nodes/224/Qn",        "nodes/224/Pg",        "nodes/224/Qg",        "nodes/224/Qmin",        "nodes/224/Qmax" ]
	, [ "nodes/129/num",          "nodes/129/state",        "nodes/129/U",       "nodes/129/fiU",        "nodes/129/Pn",       "nodes/129/Qn",        "nodes/129/Pg",        "nodes/129/Qg",        "nodes/129/Qmin",        "nodes/129/Qmax" ]
	, [ "nodes/31/num",           "nodes/31/state",         "nodes/31/U",        "nodes/31/fiU",         "nodes/31/Pn",        "nodes/31/Qn",         "nodes/31/Pg",         "nodes/31/Qg",         "nodes/31/Qmin",         "nodes/31/Qmax" ]
	, [ "nodes/1621/num",         "nodes/1621/state",       "nodes/1621/U",      "nodes/1621/fiU",       "nodes/1621/Pn",      "nodes/1621/Qn",       "nodes/1621/Pg",       "nodes/1621/Qg",       "nodes/1621/Qmin",       "nodes/1621/Qmax" ]
  	, [ "nodes/1660/num",         "nodes/1660/state",       "nodes/1660/U",      "nodes/1660/fiU",       "nodes/1660/Pn",      "nodes/1660/Qn",       "nodes/1660/Pg",       "nodes/1660/Qg",       "nodes/1660/Qmin",       "nodes/1660/Qmax" ]
  	, [ "nodes/241/num",          "nodes/241/state",        "nodes/241/U",       "nodes/241/fiU",        "nodes/241/Pn",       "nodes/241/Qn",        "nodes/241/Pg",        "nodes/241/Qg",        "nodes/241/Qmin",        "nodes/241/Qmax" ]
	, [ "nodes/981/num",          "nodes/981/state",        "nodes/981/U",       "nodes/981/fiU",        "nodes/981/Pn",       "nodes/981/Qn",        "nodes/981/Pg",        "nodes/981/Qg",        "nodes/981/Qmin",        "nodes/981/Qmax" ]
  	, [ "nodes/33/num",           "nodes/33/state",         "nodes/33/U",        "nodes/33/fiU",         "nodes/33/Pn",        "nodes/33/Qn",         "nodes/33/Pg",         "nodes/33/Qg",         "nodes/33/Qmin",         "nodes/33/Qmax" ]
  	, [ "nodes/39/num",           "nodes/39/state",         "nodes/39/U",        "nodes/39/fiU",         "nodes/39/Pn",        "nodes/39/Qn",         "nodes/39/Pg",         "nodes/39/Qg",         "nodes/39/Qmin",         "nodes/39/Qmax" ]
  	, [ "nodes/130/num",          "nodes/130/state",        "nodes/130/U",       "nodes/130/fiU",        "nodes/130/Pn",       "nodes/130/Qn",        "nodes/130/Pg",        "nodes/130/Qg",        "nodes/130/Qmin",        "nodes/130/Qmax" ]
  	, [ "nodes/306/num",          "nodes/306/state",        "nodes/306/U",       "nodes/306/fiU",        "nodes/306/Pn",       "nodes/306/Qn",        "nodes/306/Pg",        "nodes/306/Qg",        "nodes/306/Qmin",        "nodes/306/Qmax" ]
  	, [ "nodes/310/num",          "nodes/310/state",        "nodes/310/U",       "nodes/310/fiU",        "nodes/310/Pn",       "nodes/310/Qn",        "nodes/310/Pg",        "nodes/310/Qg",        "nodes/310/Qmin",        "nodes/310/Qmax" ]
  	, [ "nodes/468/num",          "nodes/468/state",        "nodes/468/U",       "nodes/468/fiU",        "nodes/468/Pn",       "nodes/468/Qn",        "nodes/468/Pg",        "nodes/468/Qg",        "nodes/468/Qmin",        "nodes/468/Qmax" ]
  	, [ "nodes/901/num",          "nodes/901/state",        "nodes/901/U",       "nodes/901/fiU",        "nodes/901/Pn",       "nodes/901/Qn",        "nodes/901/Pg",        "nodes/901/Qg",        "nodes/901/Qmin",        "nodes/901/Qmax" ]
  	, [ "nodes/932/num",          "nodes/932/state",        "nodes/932/U",       "nodes/932/fiU",        "nodes/932/Pn",       "nodes/932/Qn",        "nodes/932/Pg",        "nodes/932/Qg",        "nodes/932/Qmin",        "nodes/932/Qmax" ]
  	, [ "nodes/1916/num",         "nodes/1916/state",       "nodes/1916/U",      "nodes/1916/fiU",       "nodes/1916/Pn",      "nodes/1916/Qn",       "nodes/1916/Pg",       "nodes/1916/Qg",       "nodes/1916/Qmin",       "nodes/1916/Qmax" ]
  	, [ "nodes/903/num",          "nodes/903/state",        "nodes/903/U",       "nodes/903/fiU",        "nodes/903/Pn",       "nodes/903/Qn",        "nodes/903/Pg",        "nodes/903/Qg",        "nodes/903/Qmin",        "nodes/903/Qmax" ]
  	, [ "nodes/939/num",          "nodes/939/state",        "nodes/939/U",       "nodes/939/fiU",        "nodes/939/Pn",       "nodes/939/Qn",        "nodes/939/Pg",        "nodes/939/Qg",        "nodes/939/Qmin",        "nodes/939/Qmax" ]
  	, [ "nodes/988/num",          "nodes/988/state",        "nodes/988/U",       "nodes/988/fiU",        "nodes/988/Pn",       "nodes/988/Qn",        "nodes/988/Pg",        "nodes/988/Qg",        "nodes/988/Qmin",        "nodes/988/Qmax" ]
  	, [ "nodes/918/num",          "nodes/918/state",        "nodes/918/U",       "nodes/918/fiU",        "nodes/918/Pn",       "nodes/918/Qn",        "nodes/918/Pg",        "nodes/918/Qg",        "nodes/918/Qmin",        "nodes/918/Qmax" ]
  	, [ "nodes/2952/num",         "nodes/2952/state",       "nodes/2952/U",      "nodes/2952/fiU",       "nodes/2952/Pn",      "nodes/2952/Qn",       "nodes/2952/Pg",       "nodes/2952/Qg",       "nodes/2952/Qmin",       "nodes/2952/Qmax" ]
  	, [ "nodes/2918/num",         "nodes/2918/state",       "nodes/2918/U",      "nodes/2918/fiU",       "nodes/2918/Pn",      "nodes/2918/Qn",       "nodes/2918/Pg",       "nodes/2918/Qg",       "nodes/2918/Qmin",       "nodes/2918/Qmax" ]
  	, [ "nodes/801/num",          "nodes/801/state",        "nodes/801/U",       "nodes/801/fiU",        "nodes/801/Pn",       "nodes/801/Qn",        "nodes/801/Pg",        "nodes/801/Qg",        "nodes/801/Qmin",        "nodes/801/Qmax" ]
	, [ "nodes/831/num",          "nodes/831/state",        "nodes/831/U",       "nodes/831/fiU",        "nodes/831/Pn",       "nodes/831/Qn",        "nodes/831/Pg",        "nodes/831/Qg",        "nodes/831/Qmin",        "nodes/831/Qmax" ]
  	, [ "nodes/2925/num",         "nodes/2925/state",       "nodes/2925/U",      "nodes/2925/fiU",       "nodes/2925/Pn",      "nodes/2925/Qn",       "nodes/2925/Pg",       "nodes/2925/Qg",       "nodes/2925/Qmin",       "nodes/2925/Qmax" ]
  	, [ "nodes/1630/num",         "nodes/1630/state",       "nodes/1630/U",      "nodes/1630/fiU",       "nodes/1630/Pn",      "nodes/1630/Qn",       "nodes/1630/Pg",       "nodes/1630/Qg",       "nodes/1630/Qmin",       "nodes/1630/Qmax" ]
  	, [ "nodes/175/num",          "nodes/175/state",        "nodes/175/U",       "nodes/175/fiU",        "nodes/175/Pn",       "nodes/175/Qn",        "nodes/175/Pg",        "nodes/175/Qg",        "nodes/175/Qmin",        "nodes/175/Qmax" ]
  	, [ "nodes/590/num",          "nodes/590/state",        "nodes/590/U",       "nodes/590/fiU",        "nodes/590/Pn",       "nodes/590/Qn",        "nodes/590/Pg",        "nodes/590/Qg",        "nodes/590/Qmin",        "nodes/590/Qmax" ]
  	, [ "nodes/576/num",          "nodes/576/state",        "nodes/576/U",       "nodes/576/fiU",        "nodes/576/Pn",       "nodes/576/Qn",        "nodes/576/Pg",        "nodes/576/Qg",        "nodes/576/Qmin",        "nodes/576/Qmax" ]
	, [ "nodes/180/num",          "nodes/180/state",        "nodes/180/U",       "nodes/180/fiU",        "nodes/180/Pn",       "nodes/180/Qn",        "nodes/180/Pg",        "nodes/180/Qg",        "nodes/180/Qmin",        "nodes/180/Qmax" ]
  	, [ "nodes/1636/num",         "nodes/1636/state",       "nodes/1636/U",      "nodes/1636/fiU",       "nodes/1636/Pn",      "nodes/1636/Qn",       "nodes/1636/Pg",       "nodes/1636/Qg",       "nodes/1636/Qmin",       "nodes/1636/Qmax" ]
  	, [ "nodes/577/num",          "nodes/577/state",        "nodes/577/U",       "nodes/577/fiU",        "nodes/577/Pn",       "nodes/577/Qn",        "nodes/577/Pg",        "nodes/577/Qg",        "nodes/577/Qmin",        "nodes/577/Qmax" ]
	, [ "nodes/480/num",          "nodes/480/state",        "nodes/480/U",       "nodes/480/fiU",        "nodes/480/Pn",       "nodes/480/Qn",        "nodes/480/Pg",        "nodes/480/Qg",        "nodes/480/Qmin",        "nodes/480/Qmax" ]
  	, [ "nodes/2921/num",         "nodes/2921/state",       "nodes/2921/U",      "nodes/2921/fiU",       "nodes/2921/Pn",      "nodes/2921/Qn",       "nodes/2921/Pg",       "nodes/2921/Qg",       "nodes/2921/Qmin",       "nodes/2921/Qmax" ]
  	, [ "nodes/51/num",           "nodes/51/state",         "nodes/51/U",        "nodes/51/fiU",         "nodes/51/Pn",        "nodes/51/Qn",         "nodes/51/Pg",         "nodes/51/Qg",         "nodes/51/Qmin",         "nodes/51/Qmax" ]
  	, [ "nodes/28/num",           "nodes/28/state",         "nodes/28/U",        "nodes/28/fiU",         "nodes/28/Pn",        "nodes/28/Qn",         "nodes/28/Pg",         "nodes/28/Qg",         "nodes/28/Qmin",         "nodes/28/Qmax" ]
  	, [ "nodes/60/num",           "nodes/60/state",         "nodes/60/U",        "nodes/60/fiU",         "nodes/60/Pn",        "nodes/60/Qn",         "nodes/60/Pg",         "nodes/60/Qg",         "nodes/60/Qmin",         "nodes/60/Qmax" ]
  	, [ "nodes/466/num",          "nodes/466/state",        "nodes/466/U",       "nodes/466/fiU",        "nodes/466/Pn",       "nodes/466/Qn",        "nodes/466/Pg",        "nodes/466/Qg",        "nodes/466/Qmin",        "nodes/466/Qmax" ]
	, [ "nodes/355/num",          "nodes/355/state",        "nodes/355/U",       "nodes/355/fiU",        "nodes/355/Pn",       "nodes/355/Qn",        "nodes/355/Pg",        "nodes/355/Qg",        "nodes/355/Qmin",        "nodes/355/Qmax" ]
  	, [ "nodes/912/num",          "nodes/912/state",        "nodes/912/U",       "nodes/912/fiU",        "nodes/912/Pn",       "nodes/912/Qn",        "nodes/912/Pg",        "nodes/912/Qg",        "nodes/912/Qmin",        "nodes/912/Qmax" ]
	, [ "nodes/904/num",          "nodes/904/state",        "nodes/904/U",       "nodes/904/fiU",        "nodes/904/Pn",       "nodes/904/Qn",        "nodes/904/Pg",        "nodes/904/Qg",        "nodes/904/Qmin",        "nodes/904/Qmax" ]
  	, [ "nodes/907/num",          "nodes/907/state",        "nodes/907/U",       "nodes/907/fiU",        "nodes/907/Pn",       "nodes/907/Qn",        "nodes/907/Pg",        "nodes/907/Qg",        "nodes/907/Qmin",        "nodes/907/Qmax" ]
  	, [ "nodes/475/num",          "nodes/475/state",        "nodes/475/U",       "nodes/475/fiU",        "nodes/475/Pn",       "nodes/475/Qn",        "nodes/475/Pg",        "nodes/475/Qg",        "nodes/475/Qmin",        "nodes/475/Qmax" ]
  	, [ "nodes/473/num",          "nodes/473/state",        "nodes/473/U",       "nodes/473/fiU",        "nodes/473/Pn",       "nodes/473/Qn",        "nodes/473/Pg",        "nodes/473/Qg",        "nodes/473/Qmin",        "nodes/473/Qmax" ]
  	, [ "nodes/50/num",           "nodes/50/state",         "nodes/50/U",        "nodes/50/fiU",         "nodes/50/Pn",        "nodes/50/Qn",         "nodes/50/Pg",         "nodes/50/Qg",         "nodes/50/Qmin",         "nodes/50/Qmax" ]
  	, [ "nodes/465/num",          "nodes/465/state",        "nodes/465/U",       "nodes/465/fiU",        "nodes/465/Pn",       "nodes/465/Qn",        "nodes/465/Pg",        "nodes/465/Qg",        "nodes/465/Qmin",        "nodes/465/Qmax" ]
	, [ "nodes/63/num",           "nodes/63/state",         "nodes/63/U",        "nodes/63/fiU",         "nodes/63/Pn",        "nodes/63/Qn",         "nodes/63/Pg",         "nodes/63/Qg",         "nodes/63/Qmin",         "nodes/63/Qmax" ]
	, [ "nodes/147/num",          "nodes/147/state",        "nodes/147/U",       "nodes/147/fiU",        "nodes/147/Pn",       "nodes/147/Qn",        "nodes/147/Pg",        "nodes/147/Qg",        "nodes/147/Qmin",        "nodes/147/Qmax" ]
  	, [ "nodes/1817/num",         "nodes/1817/state",       "nodes/1817/U",      "nodes/1817/fiU",       "nodes/1817/Pn",      "nodes/1817/Qn",       "nodes/1817/Pg",       "nodes/1817/Qg",       "nodes/1817/Qmin",       "nodes/1817/Qmax" ]
  	, [ "nodes/1850/num",         "nodes/1850/state",       "nodes/1850/U",      "nodes/1850/fiU",       "nodes/1850/Pn",      "nodes/1850/Qn",       "nodes/1850/Pg",       "nodes/1850/Qg",       "nodes/1850/Qmin",       "nodes/1850/Qmax" ]
  	, [ "nodes/30/num",           "nodes/30/state",         "nodes/30/U",        "nodes/30/fiU",         "nodes/30/Pn",        "nodes/30/Qn",         "nodes/30/Pg",         "nodes/30/Qg",         "nodes/30/Qmin",         "nodes/30/Qmax" ]
    , [ "nodes/7201/num",         "nodes/7201/state",       "nodes/7201/U",      "nodes/7201/fiU",       "nodes/7201/Pn",      "nodes/7201/Qn",       "nodes/7201/Pg",       "nodes/7201/Qg",       "nodes/7201/Qmin",       "nodes/7201/Qmax" ]
    , [ "nodes/7202/num",         "nodes/7202/state",       "nodes/7202/U",      "nodes/7202/fiU",       "nodes/7202/Pn",      "nodes/7202/Qn",       "nodes/7202/Pg",       "nodes/7202/Qg",       "nodes/7202/Qmin",       "nodes/7202/Qmax" ] 
    , [ "nodes/7203/num",         "nodes/7203/state",       "nodes/7203/U",      "nodes/7203/fiU",       "nodes/7203/Pn",      "nodes/7203/Qn",       "nodes/7203/Pg",       "nodes/7203/Qg",       "nodes/7203/Qmin",       "nodes/7203/Qmax" ]
  	, [ "nodes/7204/num",         "nodes/7204/state",       "nodes/7204/U",      "nodes/7204/fiU",       "nodes/7204/Pn",      "nodes/7204/Qn",       "nodes/7204/Pg",       "nodes/7204/Qg",       "nodes/7204/Qmin",       "nodes/7204/Qmax" ]
  	, [ "nodes/7205/num",         "nodes/7205/state",       "nodes/7205/U",      "nodes/7205/fiU",       "nodes/7205/Pn",      "nodes/7205/Qn",       "nodes/7205/Pg",       "nodes/7205/Qg",       "nodes/7205/Qmin",       "nodes/7205/Qmax" ]
  	, [ "nodes/7206/num",         "nodes/7206/state",       "nodes/7206/U",      "nodes/7206/fiU",       "nodes/7206/Pn",      "nodes/7206/Qn",       "nodes/7206/Pg",       "nodes/7206/Qg",       "nodes/7206/Qmin",       "nodes/7206/Qmax" ]
  	, [ "nodes/7208/num",         "nodes/7208/state",       "nodes/7208/U",      "nodes/7208/fiU",       "nodes/7208/Pn",      "nodes/7208/Qn",       "nodes/7208/Pg",       "nodes/7208/Qg",       "nodes/7208/Qmin",       "nodes/7208/Qmax" ]
	, [ "nodes/7209/num",         "nodes/7209/state",       "nodes/7209/U",      "nodes/7209/fiU",       "nodes/7209/Pn",      "nodes/7209/Qn",       "nodes/7209/Pg",       "nodes/7209/Qg",       "nodes/7209/Qmin",       "nodes/7209/Qmax" ]
  	, [ "nodes/7210/num",         "nodes/7210/state",       "nodes/7210/U",      "nodes/7210/fiU",       "nodes/7210/Pn",      "nodes/7210/Qn",       "nodes/7210/Pg",       "nodes/7210/Qg",       "nodes/7210/Qmin",       "nodes/7210/Qmax" ]
  	, [ "nodes/7211/num",         "nodes/7211/state",       "nodes/7211/U",      "nodes/7211/fiU",       "nodes/7211/Pn",      "nodes/7211/Qn",       "nodes/7211/Pg",       "nodes/7211/Qg",       "nodes/7211/Qmin",       "nodes/7211/Qmax" ]
  	, [ "nodes/7212/num",         "nodes/7212/state",       "nodes/7212/U",      "nodes/7212/fiU",       "nodes/7212/Pn",      "nodes/7212/Qn",       "nodes/7212/Pg",       "nodes/7212/Qg",       "nodes/7212/Qmin",       "nodes/7212/Qmax" ]
  	, [ "nodes/7213/num",         "nodes/7213/state",       "nodes/7213/U",      "nodes/7213/fiU",       "nodes/7213/Pn",      "nodes/7213/Qn",       "nodes/7213/Pg",       "nodes/7213/Qg",       "nodes/7213/Qmin",       "nodes/7213/Qmax" ]
  	, [ "nodes/7214/num",         "nodes/7214/state",       "nodes/7214/U",      "nodes/7214/fiU",       "nodes/7214/Pn",      "nodes/7214/Qn",       "nodes/7214/Pg",       "nodes/7214/Qg",       "nodes/7214/Qmin",       "nodes/7214/Qmax" ]
    , [ "nodes/7215/num",         "nodes/7215/state",       "nodes/7215/U",      "nodes/7215/fiU",       "nodes/7215/Pn",      "nodes/7215/Qn",       "nodes/7215/Pg",       "nodes/7215/Qg",       "nodes/7215/Qmin",       "nodes/7215/Qmax" ]
  	, [ "nodes/7216/num",         "nodes/7216/state",       "nodes/7216/U",      "nodes/7216/fiU",       "nodes/7216/Pn",      "nodes/7216/Qn",       "nodes/7216/Pg",       "nodes/7216/Qg",       "nodes/7216/Qmin",       "nodes/7216/Qmax" ]
	, [ "nodes/7217/num",         "nodes/7217/state",       "nodes/7217/U",      "nodes/7217/fiU",       "nodes/7217/Pn",      "nodes/7217/Qn",       "nodes/7217/Pg",       "nodes/7217/Qg",       "nodes/7217/Qmin",       "nodes/7217/Qmax" ]
	, [ "nodes/7218/num",         "nodes/7218/state",       "nodes/7218/U",      "nodes/7218/fiU",       "nodes/7218/Pn",      "nodes/7218/Qn",       "nodes/7218/Pg",       "nodes/7218/Qg",       "nodes/7218/Qmin",       "nodes/7218/Qmax" ]
  	, [ "nodes/7219/num",         "nodes/7219/state",       "nodes/7219/U",      "nodes/7219/fiU",       "nodes/7219/Pn",      "nodes/7219/Qn",       "nodes/7219/Pg",       "nodes/7219/Qg",       "nodes/7219/Qmin",       "nodes/7219/Qmax" ]
	, [ "nodes/7221/num",         "nodes/7221/state",       "nodes/7221/U",      "nodes/7221/fiU",       "nodes/7221/Pn",      "nodes/7221/Qn",       "nodes/7221/Pg",       "nodes/7221/Qg",       "nodes/7221/Qmin",       "nodes/7221/Qmax" ]
    , [ "nodes/842/num",          "nodes/842/state",        "nodes/842/U",       "nodes/842/fiU",        "nodes/842/Pn",       "nodes/842/Qn",        "nodes/842/Pg",        "nodes/842/Qg",        "nodes/842/Qmin",        "nodes/842/Qmax" ]
    , [ "nodes/840/num",          "nodes/840/state",        "nodes/840/U",       "nodes/840/fiU",        "nodes/840/Pn",       "nodes/840/Qn",        "nodes/840/Pg",        "nodes/840/Qg",        "nodes/840/Qmin",        "nodes/840/Qmax" ]
  	, [ "nodes/869/num",          "nodes/869/state",        "nodes/869/U",       "nodes/869/fiU",        "nodes/869/Pn",       "nodes/869/Qn",        "nodes/869/Pg",        "nodes/869/Qg",        "nodes/869/Qmin",        "nodes/869/Qmax" ]
	, [ "nodes/862/num",          "nodes/862/state",        "nodes/862/U",       "nodes/862/fiU",        "nodes/862/Pn",       "nodes/862/Qn",        "nodes/862/Pg",        "nodes/862/Qg",        "nodes/862/Qmin",        "nodes/862/Qmax" ]
	, [ "nodes/467/num",          "nodes/467/state",        "nodes/467/U",       "nodes/467/fiU",        "nodes/467/Pn",       "nodes/467/Qn",        "nodes/467/Pg",        "nodes/467/Qg",        "nodes/467/Qmin",        "nodes/467/Qmax" ]
	, [ "nodes/928/num",          "nodes/928/state",        "nodes/928/U",       "nodes/928/fiU",        "nodes/928/Pn",       "nodes/928/Qn",        "nodes/928/Pg",        "nodes/928/Qg",        "nodes/928/Qmin",        "nodes/928/Qmax" ]
    , [ "nodes/953/num",          "nodes/953/state",        "nodes/953/U",       "nodes/953/fiU",        "nodes/953/Pn",       "nodes/953/Qn",        "nodes/953/Pg",        "nodes/953/Qg",        "nodes/953/Qmin",        "nodes/953/Qmax" ]
    , [ "nodes/7227/num",         "nodes/7227/state",       "nodes/7227/U",      "nodes/7227/fiU",       "nodes/7227/Pn",      "nodes/7227/Qn",       "nodes/7227/Pg",       "nodes/7227/Qg",       "nodes/7227/Qmin",       "nodes/7227/Qmax" ]
    , [ "nodes/805/num",          "nodes/805/state",        "nodes/805/U",       "nodes/805/fiU",        "nodes/805/Pn",       "nodes/805/Qn",        "nodes/805/Pg",        "nodes/805/Qg",        "nodes/805/Qmin",        "nodes/805/Qmax" ]
    , [ "nodes/814/num",          "nodes/814/state",        "nodes/814/U",       "nodes/814/fiU",        "nodes/814/Pn",       "nodes/814/Qn",        "nodes/814/Pg",        "nodes/814/Qg",        "nodes/814/Qmin",        "nodes/814/Qmax" ]
    , [ "nodes/7223/num",         "nodes/7223/state",       "nodes/7223/U",      "nodes/7223/fiU",       "nodes/7223/Pn",      "nodes/7223/Qn",       "nodes/7223/Pg",       "nodes/7223/Qg",       "nodes/7223/Qmin",       "nodes/7223/Qmax" ]
    , [ "nodes/839/num",          "nodes/839/state",        "nodes/839/U",       "nodes/839/fiU",        "nodes/839/Pn",       "nodes/839/Qn",        "nodes/839/Pg",        "nodes/839/Qg",        "nodes/839/Qmin",        "nodes/839/Qmax" ]
    , [ "nodes/843/num",          "nodes/843/state",        "nodes/843/U",       "nodes/843/fiU",        "nodes/843/Pn",       "nodes/843/Qn",        "nodes/843/Pg",        "nodes/843/Qg",        "nodes/843/Qmin",        "nodes/843/Qmax" ]
    , [ "nodes/7224/num",         "nodes/7224/state",       "nodes/7224/U",      "nodes/7224/fiU",       "nodes/7224/Pn",      "nodes/7224/Qn",       "nodes/7224/Pg",       "nodes/7224/Qg",       "nodes/7224/Qmin",       "nodes/7224/Qmax" ]
    , [ "nodes/865/num",          "nodes/865/state",        "nodes/865/U",       "nodes/865/fiU",        "nodes/865/Pn",       "nodes/865/Qn",        "nodes/865/Pg",        "nodes/865/Qg",        "nodes/865/Qmin",        "nodes/865/Qmax" ]
  	, [ "nodes/7225/num",         "nodes/7225/state",       "nodes/7225/U",      "nodes/7225/fiU",       "nodes/7225/Pn",      "nodes/7225/Qn",       "nodes/7225/Pg",       "nodes/7225/Qg",       "nodes/7225/Qmin",       "nodes/7225/Qmax" ]
    , [ "nodes/7226/num",         "nodes/7226/state",       "nodes/7226/U",      "nodes/7226/fiU",       "nodes/7226/Pn",      "nodes/7226/Qn",       "nodes/7226/Pg",       "nodes/7226/Qg",       "nodes/7226/Qmin",       "nodes/7226/Qmax" ]
    , [ "nodes/7228/num",         "nodes/7228/state",       "nodes/7228/U",      "nodes/7228/fiU",       "nodes/7228/Pn",      "nodes/7228/Qn",       "nodes/7228/Pg",       "nodes/7228/Qg",       "nodes/7228/Qmin",       "nodes/7228/Qmax" ] 
    , [ "nodes/7229/num",         "nodes/7229/state",       "nodes/7229/U",      "nodes/7229/fiU",       "nodes/7229/Pn",      "nodes/7229/Qn",       "nodes/7229/Pg",       "nodes/7229/Qg",       "nodes/7229/Qmin",       "nodes/7229/Qmax" ]
    , [ "nodes/4799/num",         "nodes/4799/state",       "nodes/4799/U",      "nodes/4799/fiU",       "nodes/4799/Pn",      "nodes/4799/Qn",       "nodes/4799/Pg",       "nodes/4799/Qg",       "nodes/4799/Qmin",       "nodes/4799/Qmax" ]
    , [ "nodes/4727/num",         "nodes/4727/state",       "nodes/4727/U",      "nodes/4727/fiU",       "nodes/4727/Pn",      "nodes/4727/Qn",       "nodes/4727/Pg",       "nodes/4727/Qg",       "nodes/4727/Qmin",       "nodes/4727/Qmax" ]
    , [ "nodes/4703/num",         "nodes/4703/state",       "nodes/4703/U",      "nodes/4703/fiU",       "nodes/4703/Pn",      "nodes/4703/Qn",       "nodes/4703/Pg",       "nodes/4703/Qg",       "nodes/4703/Qmin",       "nodes/4703/Qmax" ]
    , [ "nodes/4782/num",         "nodes/4782/state",       "nodes/4782/U",      "nodes/4782/fiU",       "nodes/4782/Pn",      "nodes/4782/Qn",       "nodes/4782/Pg",       "nodes/4782/Qg",       "nodes/4782/Qmin",       "nodes/4782/Qmax" ]
    , [ "nodes/4705/num",         "nodes/4705/state",       "nodes/4705/U",      "nodes/4705/fiU",       "nodes/4705/Pn",      "nodes/4705/Qn",       "nodes/4705/Pg",       "nodes/4705/Qg",       "nodes/4705/Qmin",       "nodes/4705/Qmax" ]
    , [ "nodes/10000/num",        "nodes/10000/state",      "nodes/10000/U",     "nodes/10000/fiU",      "nodes/10000/Pn",     "nodes/10000/Qn",      "nodes/10000/Pg",      "nodes/10000/Qg",      "nodes/10000/Qmin",      "nodes/10000/Qmax" ]
    , [ "nodes/4790/num",         "nodes/4790/state",       "nodes/4790/U",      "nodes/4790/fiU",       "nodes/4790/Pn",      "nodes/4790/Qn",       "nodes/4790/Pg",       "nodes/4790/Qg",       "nodes/4790/Qmin",       "nodes/4790/Qmax" ]
    , [ "nodes/4785/num",         "nodes/4785/state",       "nodes/4785/U",      "nodes/4785/fiU",       "nodes/4785/Pn",      "nodes/4785/Qn",       "nodes/4785/Pg",       "nodes/4785/Qg",       "nodes/4785/Qmin",       "nodes/4785/Qmax" ]
    , [ "nodes/1852/num",         "nodes/1852/state",       "nodes/1852/U",      "nodes/1852/fiU",       "nodes/1852/Pn",      "nodes/1852/Qn",       "nodes/1852/Pg",       "nodes/1852/Qg",       "nodes/1852/Qmin",       "nodes/1852/Qmax" ]
    , [ "nodes/1853/num",         "nodes/1853/state",       "nodes/1853/U",      "nodes/1853/fiU",       "nodes/1853/Pn",      "nodes/1853/Qn",       "nodes/1853/Pg",       "nodes/1853/Qg",       "nodes/1853/Qmin",       "nodes/9917/Qmax" ]
    , [ "nodes/9917/num",         "nodes/9917/state",       "nodes/9917/U",      "nodes/9917/fiU",       "nodes/9917/Pn",      "nodes/9917/Qn",       "nodes/9917/Pg",       "nodes/9917/Qg",       "nodes/9917/Qmin",       "nodes/9917/Qmax" ]
    , [ "nodes/1631/num",         "nodes/1631/state",       "nodes/1631/U",      "nodes/1631/fiU",       "nodes/1631/Pn",      "nodes/1631/Qn",       "nodes/1631/Pg",       "nodes/1631/Qg",       "nodes/1631/Qmin",       "nodes/1631/Qmax" ]
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END NODES
  ];

csv_template( "links.csv" )->
  [
    [ {const,"link_start"},   {const,"link_end"},   {const,"link_num"},   {const,"link_state"},   {const,"link_Pn"},    {const,"link_Qn"},    {const,"link_Pk"},    {const,"link_Qk"}   ]
    %%, [ "links/_________/start", "links/_________/end", "links/_________/num", "links/_________/state", "links/_________/Pn",  "links/_________/Qn",  "links/_________/Pk",  "links/_________/Qk"  ]
    , [ "links/26_25_1/start",              "links/26_25_1/end",            "links/26_25_1/num",            "links/26_25_1/state",            "links/26_25_1/Pn",            "links/26_25_1/Qn",           "links/26_25_1/Pk",           "links/26_25_1/Qk" ]
    , [ "links/26_25_2/start",              "links/26_25_2/end",            "links/26_25_2/num",            "links/26_25_2/state",            "links/26_25_2/Pn",            "links/26_25_2/Qn",           "links/26_25_2/Pk",           "links/26_25_2/Qk" ]
    , [ "links/25_325_3/start",             "links/25_325_3/end",           "links/25_325_3/num",           "links/25_325_3/state",           "links/25_325_3/Pn",           "links/25_325_3/Qn",          "links/25_325_3/Pk",          "links/25_325_3/Qk" ]
    , [ "links/325_469_3/start",            "links/325_469_3/end",          "links/325_469_3/num",          "links/325_469_3/state",          "links/325_469_3/Pn",          "links/325_469_3/Qn",         "links/325_469_3/Pk",         "links/325_469_3/Qk" ]
    , [ "links/26_469_3/start",             "links/26_469_3/end",           "links/26_469_3/num",           "links/26_469_3/state",           "links/26_469_3/Pn",           "links/26_469_3/Qn",          "links/26_469_3/Pk",          "links/26_469_3/Qk"  ]
    , [ "links/469_900_1/start",            "links/469_900_1/end",          "links/469_900_1/num",          "links/469_900_1/state",          "links/469_900_1/Pn",          "links/469_900_1/Qn",         "links/469_900_1/Pk",         "links/469_900_1/Qk" ]
    , [ "links/469_900_2/start",            "links/469_900_2/end",          "links/469_900_2/num",          "links/469_900_2/state",          "links/469_900_2/Pn",          "links/469_900_2/Qn",         "links/469_900_2/Pk",         "links/469_900_2/Qk" ]
    , [ "links/900_902_3/start",            "links/900_902_3/end",          "links/900_902_3/num",          "links/900_902_3/state",          "links/900_902_3/Pn",          "links/900_902_3/Qn",         "links/900_902_3/Pk",         "links/900_902_3/Qk" ]
    , [ "links/900_938_3/start",            "links/900_938_3/end",          "links/900_938_3/num",          "links/900_938_3/state",          "links/900_938_3/Pn",          "links/900_938_3/Qn",         "links/900_938_3/Pk",         "links/900_938_3/Qk" ]
    , [ "links/900_9932_3/start",           "links/900_9932_3/end",         "links/900_9932_3/num",         "links/900_9932_3/state",         "links/900_9932_3/Pn",         "links/900_9932_3/Qn",        "links/900_9932_3/Pk",        "links/900_9932_3/Qk" ]
    , [ "links/902_9932_3/start",           "links/902_9932_3/end",         "links/902_9932_3/num",         "links/902_9932_3/state",         "links/902_9932_3/Pn",         "links/902_9932_3/Qn",        "links/902_9932_3/Pk",        "links/902_9932_3/Qk" ]
    , [ "links/902_938_3/start",            "links/902_938_3/end",          "links/902_938_3/num",          "links/902_938_3/state",          "links/902_938_3/Pn",          "links/902_938_3/Qn",         "links/902_938_3/Pk",         "links/902_938_3/Qk" ]
    , [ "links/9932_2919_3/start",          "links/9932_2919_3/end",        "links/9932_2919_3/num",        "links/9932_2919_3/state",        "links/9932_2919_3/Pn",        "links/9932_2919_3/Qn",       "links/9932_2919_3/Pk",       "links/9932_2919_3/Qk" ]
    , [ "links/2919_800_3/start",           "links/2919_800_3/end",         "links/2919_800_3/num",         "links/2919_800_3/state",         "links/2919_800_3/Pn",         "links/2919_800_3/Qn",        "links/2919_800_3/Pk",        "links/2919_800_3/Qk" ]
    , [ "links/800_830_3/start",            "links/800_830_3/end",          "links/800_830_3/num",          "links/800_830_3/state",          "links/800_830_3/Pn",          "links/800_830_3/Qn",         "links/800_830_3/Pk",         "links/800_830_3/Qk" ]
    , [ "links/980_26_3/start",             "links/980_26_3/end",           "links/980_26_3/num",           "links/980_26_3/state",           "links/980_26_3/Pn",           "links/980_26_3/Qn",          "links/980_26_3/Pk",          "links/980_26_3/Qk" ]
    , [ "links/980_986_3/start",            "links/980_986_3/end",          "links/980_986_3/num",          "links/980_986_3/state",          "links/980_986_3/Pn",          "links/980_986_3/Qn",         "links/980_986_3/Pk",         "links/980_986_3/Qk" ]
    , [ "links/986_987_3/start",            "links/986_987_3/end",          "links/986_987_3/num",          "links/986_987_3/state",          "links/986_987_3/Pn",          "links/986_987_3/Qn",         "links/986_987_3/Pk",         "links/986_987_3/Qk" ]
    , [ "links/987_938_3/start",            "links/987_938_3/end",          "links/987_938_3/num",          "links/987_938_3/state",          "links/987_938_3/Pn",          "links/987_938_3/Qn",         "links/987_938_3/Pk",         "links/987_938_3/Qk" ]
    , [ "links/980_240_3/start",            "links/980_240_3/end",          "links/980_240_3/num",          "links/980_240_3/state",          "links/980_240_3/Pn",          "links/980_240_3/Qn",         "links/980_240_3/Pk",         "links/980_240_3/Qk" ]
    , [ "links/224_830_3/start",            "links/224_830_3/end",          "links/224_830_3/num",          "links/224_830_3/state",          "links/224_830_3/Pn",          "links/224_830_3/Qn",         "links/224_830_3/Pk",         "links/224_830_3/Qk" ]
    , [ "links/25_129_3/start",             "links/25_129_3/end",           "links/25_129_3/num",           "links/25_129_3/state",           "links/25_129_3/Pn",           "links/25_129_3/Qn",          "links/25_129_3/Pk",          "links/25_129_3/Qk" ]
    , [ "links/25_31_3/start",              "links/25_31_3/end",            "links/25_31_3/num",            "links/25_31_3/state",            "links/25_31_3/Pn",            "links/25_31_3/Qn",           "links/25_31_3/Pk",           "links/25_31_3/Qk" ]
    , [ "links/31_1621_3/start",            "links/31_1621_3/end",          "links/31_1621_3/num",          "links/31_1621_3/state",          "links/31_1621_3/Pn",          "links/31_1621_3/Qn",         "links/31_1621_3/Pk",         "links/31_1621_3/Qk" ]
    , [ "links/1621_240_3/start",           "links/1621_240_3/end",         "links/1621_240_3/num",         "links/1621_240_3/state",         "links/1621_240_3/Pn",         "links/1621_240_3/Qn",        "links/1621_240_3/Pk",        "links/1621_240_3/Qk" ]
    , [ "links/26_1660_3/start",            "links/26_1660_3/end",          "links/26_1660_3/num",          "links/26_1660_3/state",          "links/26_1660_3/Pn",          "links/26_1660_3/Qn",         "links/26_1660_3/Pk",         "links/26_1660_3/Qk" ]
    , [ "links/240_241_1/start",            "links/240_241_1/end",          "links/240_241_1/num",          "links/240_241_1/state",          "links/240_241_1/Pn",          "links/240_241_1/Qn",         "links/240_241_1/Pk",         "links/240_241_1/Qk" ]
    , [ "links/240_241_2/start",            "links/240_241_2/end",          "links/240_241_2/num",          "links/240_241_2/state",          "links/240_241_2/Pn",          "links/240_241_2/Qn",         "links/240_241_2/Pk",         "links/240_241_2/Qk" ]
    , [ "links/980_981_3/start",            "links/980_981_3/end",          "links/980_981_3/num",          "links/980_981_3/state",          "links/980_981_3/Pn",          "links/980_981_3/Qn",         "links/980_981_3/Pk",         "links/980_981_3/Qk" ]
    , [ "links/31_33_3/start",              "links/31_33_3/end",            "links/31_33_3/num",            "links/31_33_3/state",            "links/31_33_3/Pn",            "links/31_33_3/Qn",           "links/31_33_3/Pk",           "links/31_33_3/Qk" ]
    , [ "links/25_39_3/start",              "links/25_39_3/end",            "links/25_39_3/num",            "links/25_39_3/state",            "links/25_39_3/Pn",            "links/25_39_3/Qn",           "links/25_39_3/Pk",           "links/25_39_3/Qk" ]
    , [ "links/129_130_1/start",            "links/129_130_1/end",          "links/129_130_1/num",          "links/129_130_1/state",          "links/129_130_1/Pn",          "links/129_130_1/Qn",         "links/129_130_1/Pk",         "links/129_130_1/Qk" ]
    , [ "links/129_130_2/start",            "links/129_130_2/end",          "links/129_130_2/num",          "links/129_130_2/state",          "links/129_130_2/Pn",          "links/129_130_2/Qn",         "links/129_130_2/Pk",         "links/129_130_2/Qk" ]
    , [ "links/325_310_3/start",            "links/325_310_3/end",          "links/325_310_3/num",          "links/325_310_3/state",          "links/325_310_3/Pn",          "links/325_310_3/Qn",         "links/325_310_3/Pk",         "links/325_310_3/Qk" ]
    , [ "links/469_468_3/start",            "links/469_468_3/end",          "links/469_468_3/num",          "links/469_468_3/state",          "links/469_468_3/Pn",          "links/469_468_3/Qn",         "links/469_468_3/Pk",         "links/469_468_3/Qk" ]
    , [ "links/900_901_3/start",            "links/900_901_3/end",          "links/900_901_3/num",          "links/900_901_3/state",          "links/900_901_3/Pn",          "links/900_901_3/Qn",         "links/900_901_3/Pk",         "links/900_901_3/Qk" ]
    , [ "links/9932_932_3/start",           "links/9932_932_3/end",         "links/9932_932_3/num",         "links/9932_932_3/state",         "links/9932_932_3/Pn",         "links/9932_932_3/Qn",        "links/9932_932_3/Pk",        "links/9932_932_3/Qk" ]
    , [ "links/932_1916_3/start",           "links/932_1916_3/end",         "links/932_1916_3/num",         "links/932_1916_3/state",         "links/932_1916_3/Pn",         "links/932_1916_3/Qn",        "links/932_1916_3/Pk",        "links/932_1916_3/Qk" ]
    , [ "links/1916_903_3/start",           "links/1916_903_3/end",         "links/1916_903_3/num",         "links/1916_903_3/state",         "links/1916_903_3/Pn",         "links/1916_903_3/Qn",        "links/1916_903_3/Pk",        "links/1916_903_3/Qk" ]
    , [ "links/902_903_1/start",            "links/902_903_1/end",          "links/902_903_1/num",          "links/902_903_1/state",          "links/902_903_1/Pn",          "links/902_903_1/Qn",         "links/902_903_1/Pk",         "links/902_903_1/Qk" ]
    , [ "links/902_903_2/start",            "links/902_903_2/end",          "links/902_903_2/num",          "links/902_903_2/state",          "links/902_903_2/Pn",          "links/902_903_2/Qn",         "links/902_903_2/Pk",         "links/902_903_2/Qk" ]
    , [ "links/938_939_1/start",            "links/938_939_1/end",          "links/938_939_1/num",          "links/938_939_1/state",          "links/938_939_1/Pn",          "links/938_939_1/Qn",         "links/938_939_1/Pk",         "links/938_939_1/Qk" ]
    , [ "links/938_939_2/start",            "links/938_939_2/end",          "links/938_939_2/num",          "links/938_939_2/state",          "links/938_939_2/Pn",          "links/938_939_2/Qn",         "links/938_939_2/Pk",         "links/938_939_2/Qk" ]
    , [ "links/987_988_3/start",            "links/987_988_3/end",          "links/987_988_3/num",          "links/987_988_3/state",          "links/987_988_3/Pn",          "links/987_988_3/Qn",         "links/987_988_3/Pk",         "links/987_988_3/Qk" ]
    , [ "links/918_2952_3/start",           "links/918_2952_3/end",         "links/918_2952_3/num",         "links/918_2952_3/state",         "links/918_2952_3/Pn",         "links/918_2952_3/Qn",        "links/918_2952_3/Pk",        "links/918_2952_3/Qk" ]
    , [ "links/800_801_3/start",            "links/800_801_3/end",          "links/800_801_3/num",          "links/800_801_3/state",          "links/800_801_3/Pn",          "links/800_801_3/Qn",         "links/800_801_3/Pk",         "links/800_801_3/Qk" ]
    , [ "links/2919_2918_1/start",          "links/2919_2918_1/end",        "links/2919_2918_1/num",        "links/2919_2918_1/state",        "links/2919_2918_1/Pn",        "links/2919_2918_1/Qn",       "links/2919_2918_1/Pk",       "links/2919_2918_1/Qk" ]
    , [ "links/2919_2918_2/start",          "links/2919_2918_2/end",        "links/2919_2918_2/num",        "links/2919_2918_2/state",        "links/2919_2918_2/Pn",        "links/2919_2918_2/Qn",       "links/2919_2918_2/Pk",       "links/2919_2918_2/Qk" ]
    , [ "links/224_2925_3/start",           "links/224_2925_3/end",         "links/224_2925_3/num",         "links/224_2925_3/state",         "links/224_2925_3/Pn",         "links/224_2925_3/Qn",        "links/224_2925_3/Pk",        "links/224_2925_3/Qk"]
    , [ "links/830_831_1/start",            "links/830_831_1/end",          "links/830_831_1/num",          "links/830_831_1/state",          "links/830_831_1/Pn",          "links/830_831_1/Qn",         "links/830_831_1/Pk",         "links/830_831_1/Qk" ]
    , [ "links/830_831_2/start",            "links/830_831_2/end",          "links/830_831_2/num",          "links/830_831_2/state",          "links/830_831_2/Pn",          "links/830_831_2/Qn",         "links/830_831_2/Pk",         "links/830_831_2/Qk"  ]
    , [ "links/1660_1630_1/start",          "links/1660_1630_1/end",        "links/1660_1630_1/num",        "links/1660_1630_1/state",        "links/1660_1630_1/Pn",        "links/1660_1630_1/Qn",       "links/1660_1630_1/Pk",       "links/1660_1630_1/Qk"  ]
    , [ "links/1630_1621_3/start",          "links/1630_1621_3/end",        "links/1630_1621_3/num",        "links/1630_1621_3/state",        "links/1630_1621_3/Pn",        "links/1630_1621_3/Qn",       "links/1630_1621_3/Pk",       "links/1630_1621_3/Qk"  ]
    , [ "links/26_175_3/start",             "links/26_175_3/end",           "links/26_175_3/num",           "links/26_175_3/state",           "links/26_175_3/Pn",           "links/26_175_3/Qn",          "links/26_175_3/Pk",          "links/26_175_3/Qk" ]
    , [ "links/175_590_3/start",            "links/175_590_3/end",          "links/175_590_3/num",          "links/175_590_3/state",          "links/175_590_3/Pn",          "links/175_590_3/Qn",         "links/175_590_3/Pk",         "links/175_590_3/Qk" ]
    , [ "links/590_576_3/start",            "links/590_576_3/end",          "links/590_576_3/num",          "links/590_576_3/state",          "links/590_576_3/Pn",          "links/590_576_3/Qn",         "links/590_576_3/Pk",         "links/590_576_3/Qk" ]
    , [ "links/129_180_3/start",            "links/129_180_3/end",          "links/129_180_3/num",          "links/129_180_3/state",          "links/129_180_3/Pn",          "links/129_180_3/Qn",         "links/129_180_3/Pk",         "links/129_180_3/Qk" ]
    , [ "links/180_576_3/start",            "links/180_576_3/end",          "links/180_576_3/num",          "links/180_576_3/state",          "links/180_576_3/Pn",          "links/180_576_3/Qn",         "links/180_576_3/Pk",         "links/180_576_3/Qk"  ]
    , [ "links/576_577_3/start",            "links/576_577_3/end",          "links/576_577_3/num",          "links/576_577_3/state",          "links/576_577_3/Pn",          "links/576_577_3/Qn",         "links/576_577_3/Pk",         "links/576_577_3/Qk"  ]
    , [ "links/1660_1636_3/start",          "links/1660_1636_3/end",        "links/1660_1636_3/num",        "links/1660_1636_3/state",        "links/1660_1636_3/Pn",        "links/1660_1636_3/Qn",       "links/1660_1636_3/Pk",       "links/1660_1636_3/Qk" ]
    , [ "links/469_480_3/start",            "links/469_480_3/end",          "links/469_480_3/num",          "links/469_480_3/state",          "links/469_480_3/Pn",          "links/469_480_3/Qn",         "links/469_480_3/Pk",         "links/469_480_3/Qk" ]
    , [ "links/1660_1630_2/start",          "links/1660_1630_2/end",        "links/1660_1630_2/num",        "links/1660_1630_2/state",        "links/1660_1630_2/Pn",        "links/1660_1630_2/Qn",       "links/1660_1630_2/Pk",       "links/1660_1630_2/Qk" ]  
    , [ "links/28_60_3/start",              "links/28_60_3/end",            "links/28_60_3/num",            "links/28_60_3/state",            "links/28_60_3/Pn",            "links/28_60_3/Qn",           "links/28_60_3/Pk",           "links/28_60_3/Qk" ]
    , [ "links/25_51_3/start",              "links/25_51_3/end",            "links/25_51_3/num",            "links/25_51_3/state",            "links/25_51_3/Pn",            "links/25_51_3/Qn",           "links/25_51_3/Pk",           "links/25_51_3/Qk" ]
    , [ "links/28_26_1/start",              "links/28_26_1/end",            "links/28_26_1/num",            "links/28_26_1/state",            "links/28_26_1/Pn",            "links/28_26_1/Qn",           "links/28_26_1/Pk",           "links/28_26_1/Qk" ]
    , [ "links/28_26_2/start",              "links/28_26_2/end",            "links/28_26_2/num",            "links/28_26_2/state",            "links/28_26_2/Pn",            "links/28_26_2/Qn",           "links/28_26_2/Pk",           "links/28_26_2/Qk" ]   
    , [ "links/480_466_3/start",            "links/480_466_3/end",          "links/480_466_3/num",          "links/480_466_3/state",          "links/480_466_3/Pn",          "links/480_466_3/Qn",         "links/480_466_3/Pk",         "links/480_466_3/Qk" ]
    , [ "links/904_939_3/start",            "links/904_939_3/end",          "links/904_939_3/num",          "links/904_939_3/state",          "links/904_939_3/Pn",          "links/904_939_3/Qn",         "links/904_939_3/Pk",         "links/904_939_3/Qk" ]
    , [ "links/912_953_3/start",            "links/912_953_3/end",          "links/912_953_3/num",          "links/912_953_3/state",          "links/912_953_3/Pn",          "links/912_953_3/Qn",         "links/912_953_3/Pk",         "links/912_953_3/Qk" ] 
    , [ "links/918_912_3/start",            "links/918_912_3/end",          "links/918_912_3/num",          "links/918_912_3/state",          "links/918_912_3/Pn",          "links/918_912_3/Qn",         "links/918_912_3/Pk",         "links/918_912_3/Qk"]
    , [ "links/939_907_3/start",            "links/939_907_3/end",          "links/939_907_3/num",          "links/939_907_3/state",          "links/939_907_3/Pn",          "links/939_907_3/Qn",         "links/939_907_3/Pk",         "links/939_907_3/Qk" ]
    , [ "links/903_904_3/start",            "links/903_904_3/end",          "links/903_904_3/num",          "links/903_904_3/state",          "links/903_904_3/Pn",          "links/903_904_3/Qn",         "links/903_904_3/Pk",         "links/903_904_3/Qk" ]
    , [ "links/468_473_3/start",            "links/468_473_3/end",          "links/468_473_3/num",          "links/468_473_3/state",          "links/468_473_3/Pn",          "links/468_473_3/Qn",         "links/468_473_3/Pk",         "links/468_473_3/Qk" ]
    , [ "links/473_475_3/start",            "links/473_475_3/end",          "links/473_475_3/num",          "links/473_475_3/state",          "links/473_475_3/Pn",          "links/473_475_3/Qn",         "links/473_475_3/Pk",         "links/473_475_3/Qk" ]
    , [ "links/475_468_3/start",            "links/475_468_3/end",          "links/475_468_3/num",          "links/475_468_3/state",          "links/475_468_3/Pn",          "links/475_468_3/Qn",         "links/475_468_3/Pk",         "links/475_468_3/Qk" ]
    , [ "links/39_50_3/start",              "links/39_50_3/end",            "links/39_50_3/num",            "links/39_50_3/state",            "links/39_50_3/Pn",            "links/39_50_3/Qn",           "links/39_50_3/Pk",           "links/39_50_3/Qk" ]
    , [ "links/903_907_3/start",            "links/903_907_3/end",          "links/903_907_3/num",          "links/903_907_3/state",          "links/903_907_3/Pn",          "links/903_907_3/Qn",         "links/903_907_3/Pk",         "links/903_907_3/Qk" ] 
    , [ "links/224_63_3/start",             "links/224_63_3/end",           "links/224_63_3/num",           "links/224_63_3/state",           "links/224_63_3/Pn",           "links/224_63_3/Qn",          "links/224_63_3/Pk",          "links/224_63_3/Qk" ]
    , [ "links/175_147_3/start",            "links/175_147_3/end",          "links/175_147_3/num",          "links/175_147_3/state",          "links/175_147_3/Pn",          "links/175_147_3/Qn",         "links/175_147_3/Pk",         "links/175_147_3/Qk"]
    , [ "links/147_1817_3/start",           "links/147_1817_3/end",         "links/147_1817_3/num",         "links/147_1817_3/state",         "links/147_1817_3/Pn",         "links/147_1817_3/Qn",        "links/147_1817_3/Pk",        "links/147_1817_3/Qk" ]
    , [ "links/25_1817_3/start",            "links/25_1817_3/end",          "links/25_1817_3/num",          "links/25_1817_3/state",          "links/25_1817_3/Pn",          "links/25_1817_3/Qn",         "links/25_1817_3/Pk",         "links/25_1817_3/Qk" ]
    , [ "links/31_1850_3/start",            "links/31_1850_3/end",          "links/31_1850_3/num",          "links/31_1850_3/state",          "links/31_1850_3/Pn",          "links/31_1850_3/Qn",         "links/31_1850_3/Pk",         "links/31_1850_3/Qk" ]
    , [ "links/1817_1850_3/start",          "links/1817_1850_3/end",        "links/1817_1850_3/num",        "links/1817_1850_3/state",        "links/1817_1850_3/Pn",        "links/1817_1850_3/Qn",       "links/1817_1850_3/Pk",       "links/1817_1850_3/Qk"  ]
    , [ "links/31_30_3/start",              "links/31_30_3/end",            "links/31_30_3/num",            "links/31_30_3/state",            "links/31_30_3/Pn",            "links/31_30_3/Qn",           "links/31_30_3/Pk",           "links/31_30_3/Qk" ]
    , [ "links/241_7201_3/start",           "links/241_7201_3/end",         "links/241_7201_3/num",         "links/241_7201_3/state",         "links/241_7201_3/Pn",         "links/241_7201_3/Qn",        "links/241_7201_3/Pk",        "links/241_7201_3/Qk" ]
    , [ "links/981_7201_3/start",           "links/981_7201_3/end",         "links/981_7201_3/num",         "links/981_7201_3/state",         "links/981_7201_3/Pn",         "links/981_7201_3/Qn",        "links/981_7201_3/Pk",        "links/981_7201_3/Qk" ]
    , [ "links/981_7202_3/start",           "links/981_7202_3/end",         "links/981_7202_3/num",         "links/981_7202_3/state",         "links/981_7202_3/Pn",         "links/981_7202_3/Qn",        "links/981_7202_3/Pk",        "links/981_7202_3/Qk" ]
    , [ "links/33_7202_3/start",            "links/33_7202_3/end",          "links/33_7202_3/num",          "links/33_7202_3/state",          "links/33_7202_3/Pn",          "links/33_7202_3/Qn",         "links/33_7202_3/Pk",         "links/33_7202_3/Qk" ]
    , [ "links/33_7203_3/start",            "links/33_7203_3/end",          "links/33_7203_3/num",          "links/33_7203_3/state",          "links/33_7203_3/Pn",          "links/33_7203_3/Qn",         "links/33_7203_3/Pk",         "links/33_7203_3/Qk"]
    , [ "links/39_7203_3/start",            "links/39_7203_3/end",          "links/39_7203_3/num",          "links/39_7203_3/state",          "links/39_7203_3/Pn",          "links/39_7203_3/Qn",         "links/39_7203_3/Pk",         "links/39_7203_3/Qk" ]
    , [ "links/39_7204_3/start",            "links/39_7204_3/end",          "links/39_7204_3/num",          "links/39_7204_3/state",          "links/39_7204_3/Pn",          "links/39_7204_3/Qn",         "links/39_7204_3/Pk",         "links/39_7204_3/Qk" ]
    , [ "links/130_7204_3/start",           "links/130_7204_3/end",         "links/130_7204_3/num",         "links/130_7204_3/state",         "links/130_7204_3/Pn",         "links/130_7204_3/Qn",        "links/130_7204_3/Pk",        "links/130_7204_3/Qk" ]
    , [ "links/130_7205_3/start",           "links/130_7205_3/end",         "links/130_7205_3/num",         "links/130_7205_3/state",         "links/130_7205_3/Pn",         "links/130_7205_3/Qn",        "links/130_7205_3/Pk",        "links/130_7205_3/Qk" ]
    , [ "links/306_7205_3/start",           "links/306_7205_3/end",         "links/306_7205_3/num",         "links/306_7205_3/state",         "links/306_7205_3/Pn",         "links/306_7205_3/Qn",        "links/306_7205_3/Pk",        "links/306_7205_3/Qk" ]
    , [ "links/39_7206_3/start",            "links/39_7206_3/end",          "links/39_7206_3/num",          "links/39_7206_3/state",          "links/39_7206_3/Pn",          "links/39_7206_3/Qn",         "links/39_7206_3/Pk",         "links/39_7206_3/Qk"  ]  
    , [ "links/306_7206_3/start",           "links/306_7206_3/end",         "links/306_7206_3/num",         "links/306_7206_3/state",         "links/306_7206_3/Pn",         "links/306_7206_3/Qn",        "links/306_7206_3/Pk",        "links/306_7206_3/Qk"  ] 
    , [ "links/306_7229_3/start",           "links/306_7229_3/end",         "links/306_7229_3/num",         "links/306_7229_3/state",         "links/306_7229_3/Pn",         "links/306_7229_3/Qn",        "links/306_7229_3/Pk",        "links/306_7229_3/Qk" ] 
    , [ "links/310_7208_3/start",           "links/310_7208_3/end",         "links/310_7208_3/num",         "links/310_7208_3/state",         "links/310_7208_3/Pn",         "links/310_7208_3/Qn",        "links/310_7208_3/Pk",        "links/310_7208_3/Qk" ]
    , [ "links/355_7208_3/start",           "links/355_7208_3/end",         "links/355_7208_3/num",         "links/355_7208_3/state",         "links/355_7208_3/Pn",         "links/355_7208_3/Qn",        "links/355_7208_3/Pk",        "links/355_7208_3/Qk" ]
    , [ "links/355_7209_3/start",           "links/355_7209_3/end",         "links/355_7209_3/num",         "links/355_7209_3/state",         "links/355_7209_3/Pn",         "links/355_7209_3/Qn",        "links/355_7209_3/Pk",        "links/355_7209_3/Qk" ]
    , [ "links/465_7209_3/start",           "links/465_7209_3/end",         "links/465_7209_3/num",         "links/465_7209_3/state",         "links/465_7209_3/Pn",         "links/465_7209_3/Qn",        "links/465_7209_3/Pk",        "links/465_7209_3/Qk" ]
    , [ "links/355_7210_3/start",           "links/355_7210_3/end",         "links/355_7210_3/num",         "links/355_7210_3/state",         "links/355_7210_3/Pn",         "links/355_7210_3/Qn",        "links/355_7210_3/Pk",        "links/355_7210_3/Qk" ]
    , [ "links/468_7210_3/start",           "links/468_7210_3/end",         "links/468_7210_3/num",         "links/468_7210_3/state",         "links/468_7210_3/Pn",         "links/468_7210_3/Qn",        "links/468_7210_3/Pk",        "links/468_7210_3/Qk" ]
    , [ "links/355_7211_3/start",           "links/355_7211_3/end",         "links/355_7211_3/num",         "links/355_7211_3/state",         "links/355_7211_3/Pn",         "links/355_7211_3/Qn",        "links/355_7211_3/Pk",        "links/355_7211_3/Qk" ]
    , [ "links/473_7211_3/start",           "links/473_7211_3/end",         "links/473_7211_3/num",         "links/473_7211_3/state",         "links/473_7211_3/Pn",         "links/473_7211_3/Qn",        "links/473_7211_3/Pk",        "links/473_7211_3/Qk" ]
    , [ "links/901_7212_3/start",           "links/901_7212_3/end",         "links/901_7212_3/num",         "links/901_7212_3/state",         "links/901_7212_3/Pn",         "links/901_7212_3/Qn",        "links/901_7212_3/Pk",        "links/901_7212_3/Qk" ]
    , [ "links/932_7212_3/start",           "links/932_7212_3/end",         "links/932_7212_3/num",         "links/932_7212_3/state",         "links/932_7212_3/Pn",         "links/932_7212_3/Qn",        "links/932_7212_3/Pk",        "links/932_7212_3/Qk" ]
    , [ "links/903_7213_3/start",           "links/903_7213_3/end",         "links/903_7213_3/num",         "links/903_7213_3/state",         "links/903_7213_3/Pn",         "links/903_7213_3/Qn",        "links/903_7213_3/Pk",        "links/903_7213_3/Qk" ]
    , [ "links/939_7213_3/start",           "links/939_7213_3/end",         "links/939_7213_3/num",         "links/939_7213_3/state",         "links/939_7213_3/Pn",         "links/939_7213_3/Qn",        "links/939_7213_3/Pk",        "links/939_7213_3/Qk" ]
    , [ "links/1916_7214_3/start",          "links/1916_7214_3/end",        "links/1916_7214_3/num",        "links/1916_7214_3/state",        "links/1916_7214_3/Pn",        "links/1916_7214_3/Qn",       "links/1916_7214_3/Pk",       "links/1916_7214_3/Qk" ]
    , [ "links/2952_7214_3/start",          "links/2952_7214_3/end",        "links/2952_7214_3/num",        "links/2952_7214_3/state",        "links/2952_7214_3/Pn",        "links/2952_7214_3/Qn",       "links/2952_7214_3/Pk",       "links/2952_7214_3/Qk" ]
    , [ "links/2952_7215_3/start",          "links/2952_7215_3/end",        "links/2952_7215_3/num",        "links/2952_7215_3/state",        "links/2952_7215_3/Pn",        "links/2952_7215_3/Qn",       "links/2952_7215_3/Pk",       "links/2952_7215_3/Qk" ]
    , [ "links/2918_7215_3/start",          "links/2918_7215_3/end",        "links/2918_7215_3/num",        "links/2918_7215_3/state",        "links/2918_7215_3/Pn",        "links/2918_7215_3/Qn",       "links/2918_7215_3/Pk",       "links/2918_7215_3/Qk" ]
    , [ "links/801_7216_3/start",           "links/801_7216_3/end",         "links/801_7216_3/num",         "links/801_7216_3/state",         "links/801_7216_3/Pn",         "links/801_7216_3/Qn",        "links/801_7216_3/Pk",        "links/801_7216_3/Qk" ]
    , [ "links/831_7216_3/start",           "links/831_7216_3/end",         "links/831_7216_3/num",         "links/831_7216_3/state",         "links/831_7216_3/Pn",         "links/831_7216_3/Qn",        "links/831_7216_3/Pk",        "links/831_7216_3/Qk" ]
    , [ "links/2918_7217_3/start",          "links/2918_7217_3/end",        "links/2918_7217_3/num",        "links/2918_7217_3/state",        "links/2918_7217_3/Pn",        "links/2918_7217_3/Qn",       "links/2918_7217_3/Pk",       "links/2918_7217_3/Qk" ]
    , [ "links/1916_7217_3/start",          "links/1916_7217_3/end",        "links/1916_7217_3/num",        "links/1916_7217_3/state",        "links/1916_7217_3/Pn",        "links/1916_7217_3/Qn",       "links/1916_7217_3/Pk",       "links/1916_7217_3/Qk" ]
    , [ "links/907_7218_3/start",           "links/907_7218_3/end",         "links/907_7218_3/num",         "links/907_7218_3/state",         "links/907_7218_3/Pn",         "links/907_7218_3/Qn",        "links/907_7218_3/Pk",        "links/907_7218_3/Qk" ]
    , [ "links/988_7218_3/start",           "links/988_7218_3/end",         "links/988_7218_3/num",         "links/988_7218_3/state",         "links/988_7218_3/Pn",         "links/988_7218_3/Qn",        "links/988_7218_3/Pk",        "links/988_7218_3/Qk" ]
    , [ "links/475_7219_3/start",           "links/475_7219_3/end",         "links/475_7219_3/num",         "links/475_7219_3/state",         "links/475_7219_3/Pn",         "links/475_7219_3/Qn",        "links/475_7219_3/Pk",        "links/475_7219_3/Qk" ]
    , [ "links/901_7219_3/start",           "links/901_7219_3/end",         "links/901_7219_3/num",         "links/901_7219_3/state",         "links/901_7219_3/Pn",         "links/901_7219_3/Qn",        "links/901_7219_3/Pk",        "links/901_7219_3/Qk"  ]
    , [ "links/465_468_3/start",            "links/465_468_3/end",          "links/465_468_3/num",          "links/465_468_3/state",          "links/465_468_3/Pn",          "links/465_468_3/Qn",         "links/465_468_3/Pk",         "links/465_468_3/Qk"  ]
    , [ "links/465_7221_3/start",           "links/465_7221_3/end",         "links/465_7221_3/num",         "links/465_7221_3/state",         "links/465_7221_3/Pn",         "links/465_7221_3/Qn",        "links/465_7221_3/Pk",        "links/465_7221_3/Qk"  ]
    , [ "links/466_7221_3/start",           "links/466_7221_3/end",         "links/466_7221_3/num",         "links/466_7221_3/state",         "links/466_7221_3/Pn",         "links/466_7221_3/Qn",        "links/466_7221_3/Pk",        "links/466_7221_3/Qk" ]
    , [ "links/831_7228_3/start",           "links/831_7228_3/end",         "links/831_7228_3/num",         "links/831_7228_3/state",         "links/831_7228_3/Pn",         "links/831_7228_3/Qn",        "links/831_7228_3/Pk",        "links/831_7228_3/Qk" ] 
    , [ "links/7228_840_3/start",           "links/7228_840_3/end",         "links/7228_840_3/num",         "links/7228_840_3/state",         "links/7228_840_3/Pn",         "links/7228_840_3/Qn",        "links/7228_840_3/Pk",        "links/7228_840_3/Qk" ] 
    , [ "links/840_869_3/start",            "links/840_869_3/end",          "links/840_869_3/num",          "links/840_869_3/state",          "links/840_869_3/Pn",          "links/840_869_3/Qn",         "links/840_869_3/Pk",         "links/840_869_3/Qk" ]
    , [ "links/869_7226_3/start",           "links/869_7226_3/end",         "links/869_7226_3/num",         "links/869_7226_3/state",         "links/869_7226_3/Pn",         "links/869_7226_3/Qn",        "links/869_7226_3/Pk",        "links/869_7226_3/Qk"  ]
    , [ "links/862_7227_3/start",           "links/862_7227_3/end",         "links/862_7227_3/num",         "links/862_7227_3/state",         "links/862_7227_3/Pn",         "links/862_7227_3/Qn",        "links/862_7227_3/Pk",        "links/862_7227_3/Qk" ]
    , [ "links/466_467_3/start",            "links/466_467_3/end",          "links/466_467_3/num",          "links/466_467_3/state",          "links/466_467_3/Pn",          "links/466_467_3/Qn",         "links/466_467_3/Pk",         "links/466_467_3/Qk"  ]
    , [ "links/928_904_3/start",            "links/928_904_3/end",          "links/928_904_3/num",          "links/928_904_3/state",          "links/928_904_3/Pn",          "links/928_904_3/Qn",         "links/928_904_3/Pk",         "links/928_904_3/Qk"  ]
    , [ "links/903_928_3/start",            "links/903_928_3/end",          "links/903_928_3/num",          "links/903_928_3/state",          "links/903_928_3/Pn",          "links/903_928_3/Qn",         "links/903_928_3/Pk",         "links/903_928_3/Qk"  ]
    , [ "links/953_904_3/start",            "links/953_904_3/end",          "links/953_904_3/num",          "links/953_904_3/state",          "links/953_904_3/Pn",          "links/953_904_3/Qn",         "links/953_904_3/Pk",         "links/953_904_3/Qk" ]
    , [ "links/467_7227_3/start",           "links/467_7227_3/end",         "links/467_7227_3/num",         "links/467_7227_3/state",         "links/467_7227_3/Pn",         "links/467_7227_3/Qn",        "links/467_7227_3/Pk",        "links/467_7227_3/Qk"  ]
    , [ "links/801_805_3/start",            "links/801_805_3/end",          "links/801_805_3/num",          "links/801_805_3/state",          "links/801_805_3/Pn",          "links/801_805_3/Qn",         "links/801_805_3/Pk",         "links/801_805_3/Qk" ]
    , [ "links/932_814_3/start",            "links/932_814_3/end",          "links/932_814_3/num",          "links/932_814_3/state",          "links/932_814_3/Pn",          "links/932_814_3/Qn",         "links/932_814_3/Pk",         "links/932_814_3/Qk" ]
    , [ "links/814_805_3/start",            "links/814_805_3/end",          "links/814_805_3/num",          "links/814_805_3/state",          "links/814_805_3/Pn",          "links/814_805_3/Qn",         "links/814_805_3/Pk",         "links/814_805_3/Qk" ]
    , [ "links/7223_805_3/start",           "links/7223_805_3/end",         "links/7223_805_3/num",         "links/7223_805_3/state",         "links/7223_805_3/Pn",         "links/7223_805_3/Qn",        "links/7223_805_3/Pk",        "links/7223_805_3/Qk" ]
    , [ "links/7223_2918_3/start",          "links/7223_2918_3/end",        "links/7223_2918_3/num",        "links/7223_2918_3/state",        "links/7223_2918_3/Pn",        "links/7223_2918_3/Qn",       "links/7223_2918_3/Pk",       "links/7223_2918_3/Qk" ]
    , [ "links/839_843_3/start",            "links/839_843_3/end",          "links/839_843_3/num",          "links/839_843_3/state",          "links/839_843_3/Pn",          "links/839_843_3/Qn",         "links/839_843_3/Pk",         "links/839_843_3/Qk" ]
    , [ "links/839_7225_3/start",           "links/839_7225_3/end",         "links/839_7225_3/num",         "links/839_7225_3/state",         "links/839_7225_3/Pn",         "links/839_7225_3/Qn",        "links/839_7225_3/Pk",        "links/839_7225_3/Qk" ]
    , [ "links/839_2925_3/start",           "links/839_2925_3/end",         "links/839_2925_3/num",         "links/839_2925_3/state",         "links/839_2925_3/Pn",         "links/839_2925_3/Qn",        "links/839_2925_3/Pk",        "links/839_2925_3/Qk" ]
    , [ "links/2925_843_3/start",           "links/2925_843_3/end",         "links/2925_843_3/num",         "links/2925_843_3/state",         "links/2925_843_3/Pn",         "links/2925_843_3/Qn",        "links/2925_843_3/Pk",        "links/2925_843_3/Qk" ]
    , [ "links/831_843_3/start",            "links/831_843_3/end",          "links/831_843_3/num",          "links/831_843_3/state",          "links/831_843_3/Pn",          "links/831_843_3/Qn",         "links/831_843_3/Pk",         "links/831_843_3/Qk"  ]
    , [ "links/842_843_3/start",            "links/842_843_3/end",          "links/842_843_3/num",          "links/842_843_3/state",          "links/842_843_3/Pn",          "links/842_843_3/Qn",         "links/842_843_3/Pk",         "links/842_843_3/Qk"  ]
    , [ "links/842_840_3/start",            "links/842_840_3/end",          "links/842_840_3/num",          "links/842_840_3/state",          "links/842_840_3/Pn",          "links/842_840_3/Qn",         "links/842_840_3/Pk",         "links/842_840_3/Qk" ]
    , [ "links/840_7224_3/start",           "links/840_7224_3/end",         "links/840_7224_3/num",         "links/840_7224_3/state",         "links/840_7224_3/Pn",         "links/840_7224_3/Qn",        "links/840_7224_3/Pk",        "links/840_7224_3/Qk" ]
    , [ "links/7224_801_3/start",           "links/7224_801_3/end",         "links/7224_801_3/num",         "links/7224_801_3/state",         "links/7224_801_3/Pn",         "links/7224_801_3/Qn",        "links/7224_801_3/Pk",        "links/7224_801_3/Qk" ]
    , [ "links/2919_2921_3/start",          "links/2919_2921_3/end",        "links/2919_2921_3/num",        "links/2919_2921_3/state",        "links/2919_2921_3/Pn",        "links/2919_2921_3/Qn",       "links/2919_2921_3/Pk",       "links/2919_2921_3/Qk" ]
    , [ "links/865_842_3/start",            "links/865_842_3/end",          "links/865_842_3/num",          "links/865_842_3/state",          "links/865_842_3/Pn",          "links/865_842_3/Qn",         "links/865_842_3/Pk",         "links/865_842_3/Qk" ]
    , [ "links/865_869_3/start",            "links/865_869_3/end",          "links/865_869_3/num",          "links/865_869_3/state",          "links/865_869_3/Pn",          "links/865_869_3/Qn",         "links/865_869_3/Pk",         "links/865_869_3/Qk" ]
    , [ "links/831_7225_3/start",           "links/831_7225_3/end",         "links/831_7225_3/num",         "links/831_7225_3/state",         "links/831_7225_3/Pn",         "links/831_7225_3/Qn",        "links/831_7225_3/Pk",        "links/831_7225_3/Qk" ]
    , [ "links/862_7226_3/start",           "links/862_7226_3/end",         "links/862_7226_3/num",         "links/862_7226_3/state",         "links/862_7226_3/Pn",         "links/862_7226_3/Qn",        "links/862_7226_3/Pk",        "links/862_7226_3/Qk" ]
    , [ "links/355_7229_3/start",           "links/355_7229_3/end",         "links/355_7229_3/num",         "links/355_7229_3/state",         "links/355_7229_3/Pn",         "links/355_7229_3/Qn",        "links/355_7229_3/Pk",        "links/355_7229_3/Qk" ]
    , [ "links/1660_1631_3/start",          "links/1660_1631_3/end",        "links/1660_1631_3/num",        "links/1660_1631_3/state",        "links/1660_1631_3/Pn",        "links/1660_1631_3/Qn",       "links/1660_1631_3/Pk",       "links/1660_1631_3/Qk" ]
    , [ "links/4799_576_3/start",           "links/4799_576_3/end",         "links/4799_576_3/num",         "links/4799_576_3/state",         "links/4799_576_3/Pn",         "links/4799_576_3/Qn",        "links/4799_576_3/Pk",        "links/4799_576_3/Qk" ]
    , [ "links/4790_577_3/start",           "links/4790_577_3/end",         "links/4790_577_3/num",         "links/4790_577_3/state",         "links/4790_577_3/Pn",         "links/4790_577_3/Qn",        "links/4790_577_3/Pk",        "links/4790_577_3/Qk" ]
    , [ "links/4790_4785_3/start",          "links/4790_4785_3/end",        "links/4790_4785_3/num",        "links/4790_4785_3/state",        "links/4790_4785_3/Pn",        "links/4790_4785_3/Qn",       "links/4790_4785_3/Pk",       "links/4790_4785_3/Qk" ]
    , [ "links/10000_4799_3/start",         "links/10000_4799_3/end",       "links/10000_4799_3/num",       "links/10000_4799_3/state",       "links/10000_4799_3/Pn",       "links/10000_4799_3/Qn",      "links/10000_4799_3/Pk",      "links/10000_4799_3/Qk" ]
    , [ "links/4782_10000_3/start",         "links/4782_10000_3/end",       "links/4782_10000_3/num",       "links/4782_10000_3/state",       "links/4782_10000_3/Pn",       "links/4782_10000_3/Qn",      "links/4782_10000_3/Pk",      "links/4782_10000_3/Qk" ]
    , [ "links/4782_4705_3/start",          "links/4782_4705_3/end",        "links/4782_4705_3/num",        "links/4782_4705_3/state",        "links/4782_4705_3/Pn",        "links/4782_4705_3/Qn",       "links/4782_4705_3/Pk",       "links/4782_4705_3/Qk" ]
    , [ "links/590_4705_3/start",           "links/590_4705_3/end",         "links/590_4705_3/num",         "links/590_4705_3/state",         "links/590_4705_3/Pn",         "links/590_4705_3/Qn",        "links/590_4705_3/Pk",        "links/590_4705_3/Qk" ]
    , [ "links/4703_4782_3/start",          "links/4703_4782_3/end",        "links/4703_4782_3/num",        "links/4703_4782_3/state",        "links/4703_4782_3/Pn",        "links/4703_4782_3/Qn",       "links/4703_4782_3/Pk",       "links/4703_4782_3/Qk" ]
    , [ "links/4703_4727_3/start",          "links/4703_4727_3/end",        "links/4703_4727_3/num",        "links/4703_4727_3/state",        "links/4703_4727_3/Pn",        "links/4703_4727_3/Qn",       "links/4703_4727_3/Pk",       "links/4703_4727_3/Qk" ]
    , [ "links/147_4727_3/start",           "links/147_4727_3/end",         "links/147_4727_3/num",         "links/147_4727_3/state",         "links/147_4727_3/Pn",         "links/147_4727_3/Qn",        "links/147_4727_3/Pk",        "links/147_4727_3/Qk" ]
    , [ "links/4727_1852_3/start",          "links/4727_1852_3/end",        "links/4727_1852_3/num",        "links/4727_1852_3/state",        "links/4727_1852_3/Pn",        "links/4727_1852_3/Qn",       "links/4727_1852_3/Pk",       "links/4727_1852_3/Qk" ]
    , [ "links/1852_1853_3/start",          "links/1852_1853_3/end",        "links/1852_1853_3/num",        "links/1852_1853_3/state",        "links/1852_1853_3/Pn",        "links/1852_1853_3/Qn",       "links/1852_1853_3/Pk",       "links/1852_1853_3/Qk" ]
    , [ "links/1853_1817_3/start",          "links/1853_1817_3/end",        "links/1853_1817_3/num",        "links/1853_1817_3/state",        "links/1853_1817_3/Pn",        "links/1853_1817_3/Qn",       "links/1853_1817_3/Pk",       "links/1853_1817_3/Qk" ]
    , [ "links/1853_9917_3/start",          "links/1853_9917_3/end",        "links/1853_9917_3/num",        "links/1853_9917_3/state",        "links/1853_9917_3/Pn",        "links/1853_9917_3/Qn",       "links/1853_9917_3/Pk",       "links/1853_9917_3/Qk" ]
    , [ "links/1631_9917_3/start",          "links/1631_9917_3/end",        "links/1631_9917_3/num",        "links/1631_9917_3/state",        "links/1631_9917_3/Pn",        "links/1631_9917_3/Qn",       "links/1631_9917_3/Pk",       "links/1631_9917_3/Qk" ]
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END LINKS
  ];


csv_template( "nodesReact.csv" )->
  [
    [ {const,"nr_n_number"},  {const,"nr_r_number"},  {const,"nr_conduct"}, {const,"nr_state"}, {const,"nr_weight"}   ]
    %[ "nr/_______/node_num",       "nr/_______/num",            "nr/_______/conduct",      "nr/_______/state",      "nr/_______/weight"        ]
	, [ "nr/26_1/node_num",          "nr/26_1/num",               "nr/26_1/conduct",            "nr/26_1/state",           "nr/26_1/weight"           ] 
    , [ "nr/9932_2/node_num",        "nr/9932_2/num",             "nr/9932_2/conduct",          "nr/9932_2/state",         "nr/9932_2/weight"         ] 
    , [ "nr/2919_2/node_num",        "nr/2919_2/num",             "nr/2919_2/conduct",          "nr/2919_2/state",         "nr/2919_2/weight"         ]
    , [ "nr/1621_2/node_num",        "nr/1621_2/num",             "nr/1621_2/conduct",          "nr/1621_2/state",         "nr/1621_2/weight"         ] 
    , [ "nr/180_3/node_num",         "nr/180_3/num",              "nr/180_3/conduct",           "nr/180_3/state",          "nr/180_3/weight"          ] 
    , [ "nr/1631_1/node_num",        "nr/1631_1/num",             "nr/1631_1/conduct",          "nr/1631_1/state",         "nr/1631_1/weight"         ]
    , [ "nr/1817_1/node_num",        "nr/1817_1/num",             "nr/1817_1/conduct",          "nr/1817_1/state",         "nr/1817_1/weight"         ] 
    , [ "nr/1850_1/node_num",        "nr/1850_1/num",             "nr/1850_1/conduct",          "nr/1850_1/state",         "nr/1850_1/weight"         ] 
    , [ "nr/1817_2/node_num",        "nr/1817_2/num",             "nr/1817_2/conduct",          "nr/1817_2/state",         "nr/1817_2/weight"         ]
	, [ "nr/938_2/node_num",         "nr/938_2/num",              "nr/938_2/conduct",           "nr/938_2/state",          "nr/938_2/weight"         ]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END NODES REACT

  ];

csv_template( "linksReact.csv" )->
  [
    [ {const,"lr_start"},   {const,"lr_end"},   {const,"lr_num"},   {const,"lr_r_num"},   {const,"lr_conduct"},   {const,"lr_state"},   {const,"lr_weight"}  ]
     , [ "lr/26_1660_3_4/start",    "lr/26_1660_3_4/end",    "lr/26_1660_3_4/num",    "lr/26_1660_3_4/r_num",    "lr/26_1660_3_4/conduct",    "lr/26_1660_3_4/state",    "lr/26_1660_3_4/weight"  ]
     , [ "lr/26_1660_3_5/start",    "lr/26_1660_3_5/end",    "lr/26_1660_3_5/num",    "lr/26_1660_3_5/r_num",    "lr/26_1660_3_5/conduct",    "lr/26_1660_3_5/state",    "lr/26_1660_3_5/weight"  ]
     , [ "lr/26_469_3_3/start",     "lr/26_469_3_3/end",     "lr/26_469_3_3/num",     "lr/26_469_3_3/r_num",     "lr/26_469_3_3/conduct",     "lr/26_469_3_3/state",     "lr/26_469_3_3/weight"  ]
     , [ "lr/26_980_3_2/start",     "lr/26_980_3_2/end",     "lr/26_980_3_2/num",     "lr/26_980_3_2/r_num",     "lr/26_980_3_2/conduct",     "lr/26_980_3_2/state",     "lr/26_980_3_2/weight"  ]
     , [ "lr/26_175_3_6/start",     "lr/26_175_3_6/end",     "lr/26_175_3_6/num",     "lr/26_175_3_6/r_num",     "lr/26_175_3_6/conduct",     "lr/26_175_3_6/state",     "lr/26_175_3_6/weight"  ]
     , [ "lr/25_129_3_1/start",     "lr/25_129_3_1/end",     "lr/25_129_3_1/num",     "lr/25_129_3_1/r_num",     "lr/25_129_3_1/conduct",     "lr/25_129_3_1/state",     "lr/25_129_3_1/weight"  ]
     , [ "lr/25_325_3_2/start",     "lr/25_325_3_2/end",     "lr/25_325_3_2/num",     "lr/25_325_3_2/r_num",     "lr/25_325_3_2/conduct",     "lr/25_325_3_2/state",     "lr/25_325_3_2/weight"  ]
     , [ "lr/25_1817_3_3/start",    "lr/25_1817_3_3/end",    "lr/25_1817_3_3/num",    "lr/25_1817_3_3/r_num",    "lr/25_1817_3_3/conduct",    "lr/25_1817_3_3/state",    "lr/25_1817_3_3/weight"  ]
     , [ "lr/325_25_3_1/start",     "lr/325_25_3_1/end",     "lr/325_25_3_1/num",     "lr/325_25_3_1/r_num",     "lr/325_25_3_1/conduct",     "lr/325_25_3_1/state",     "lr/325_25_3_1/weight"  ]
     , [ "lr/469_325_3_1/start",    "lr/469_325_3_1/end",    "lr/469_325_3_1/num",    "lr/469_325_3_1/r_num",    "lr/469_325_3_1/conduct",    "lr/469_325_3_1/state",    "lr/469_325_3_1/weight"  ]
     , [ "lr/469_900_1_5/start",    "lr/469_900_1_5/end",    "lr/469_900_1_5/num",    "lr/469_900_1_5/r_num",    "lr/469_900_1_5/conduct",    "lr/469_900_1_5/state",    "lr/469_900_1_5/weight"  ]
     , [ "lr/469_26_3_2/start",     "lr/469_26_3_2/end",     "lr/469_26_3_2/num",     "lr/469_26_3_2/r_num",     "lr/469_26_3_2/conduct",     "lr/469_26_3_2/state",     "lr/469_26_3_2/weight"  ]
     , [ "lr/469_26_3_4/start",     "lr/469_26_3_4/end",     "lr/469_26_3_4/num",     "lr/469_26_3_4/r_num",     "lr/469_26_3_4/conduct",     "lr/469_26_3_4/state",     "lr/469_26_3_4/weight"  ]
     , [ "lr/469_900_2_5/start",    "lr/469_900_2_5/end",    "lr/469_900_2_5/num",    "lr/469_900_2_5/r_num",    "lr/469_900_2_5/conduct",    "lr/469_900_2_5/state",    "lr/469_900_2_5/weight"  ]
     , [ "lr/900_469_1_1/start",    "lr/900_469_1_1/end",    "lr/900_469_1_1/num",    "lr/900_469_1_1/r_num",    "lr/900_469_1_1/conduct",    "lr/900_469_1_1/state",    "lr/900_469_1_1/weight"  ]
     , [ "lr/900_469_2_2/start",    "lr/900_469_2_2/end",    "lr/900_469_2_2/num",    "lr/900_469_2_2/r_num",    "lr/900_469_2_2/conduct",    "lr/900_469_2_2/state",    "lr/900_469_2_2/weight"  ]
     , [ "lr/900_902_3_3/start",    "lr/900_902_3_3/end",    "lr/900_902_3_3/num",    "lr/900_902_3_3/r_num",    "lr/900_902_3_3/conduct",    "lr/900_902_3_3/state",    "lr/900_902_3_3/weight"  ]
     , [ "lr/900_938_3_5/start",    "lr/900_938_3_5/end",    "lr/900_938_3_5/num",    "lr/900_938_3_5/r_num",    "lr/900_938_3_5/conduct",    "lr/900_938_3_5/state",    "lr/900_938_3_5/weight"  ]
     , [ "lr/902_900_3_1/start",    "lr/902_900_3_1/end",    "lr/902_900_3_1/num",    "lr/902_900_3_1/r_num",    "lr/902_900_3_1/conduct",    "lr/902_900_3_1/state",    "lr/902_900_3_1/weight"  ]
     , [ "lr/902_9932_3_2/start",   "lr/902_9932_3_2/end",   "lr/902_9932_3_2/num",   "lr/902_9932_3_2/r_num",   "lr/902_9932_3_2/conduct",   "lr/902_9932_3_2/state",   "lr/902_9932_3_2/weight"  ]
     , [ "lr/938_900_3_1/start",    "lr/938_900_3_1/end",    "lr/938_900_3_1/num",    "lr/938_900_3_1/r_num",    "lr/938_900_3_1/conduct",    "lr/938_900_3_1/state",    "lr/938_900_3_1/weight"  ]
     , [ "lr/9932_900_3_1/start",   "lr/9932_900_3_1/end",   "lr/9932_900_3_1/num",   "lr/9932_900_3_1/r_num",   "lr/9932_900_3_1/conduct",   "lr/9932_900_3_1/state",   "lr/9932_900_3_1/weight"  ]
     , [ "lr/2919_9932_3_1/start",  "lr/2919_9932_3_1/end",  "lr/2919_9932_3_1/num",  "lr/2919_9932_3_1/r_num",  "lr/2919_9932_3_1/conduct",  "lr/2919_9932_3_1/state",  "lr/2919_9932_3_1/weight"  ]
     , [ "lr/800_2919_3_1/start",   "lr/800_2919_3_1/end",   "lr/800_2919_3_1/num",   "lr/800_2919_3_1/r_num",   "lr/800_2919_3_1/conduct",   "lr/800_2919_3_1/state",   "lr/800_2919_3_1/weight"  ]
     , [ "lr/800_830_3_2/start",    "lr/800_830_3_2/end",    "lr/800_830_3_2/num",    "lr/800_830_3_2/r_num",    "lr/800_830_3_2/conduct",    "lr/800_830_3_2/state",    "lr/800_830_3_2/weight"  ]
     , [ "lr/980_26_3_1/start",     "lr/980_26_3_1/end",     "lr/980_26_3_1/num",     "lr/980_26_3_1/r_num",     "lr/980_26_3_1/conduct",     "lr/980_26_3_1/state",     "lr/980_26_3_1/weight"  ]
     , [ "lr/980_240_3_2/start",    "lr/980_240_3_2/end",    "lr/980_240_3_2/num",    "lr/980_240_3_2/r_num",    "lr/980_240_3_2/conduct",    "lr/980_240_3_2/state",    "lr/980_240_3_2/weight"  ]
     , [ "lr/986_980_3_1/start",    "lr/986_980_3_1/end",    "lr/986_980_3_1/num",     "lr/986_980_3_1/r_num",   "lr/986_980_3_1/conduct",    "lr/986_980_3_1/state",    "lr/986_980_3_1/weight"  ]
     , [ "lr/987_986_3_1/start",    "lr/987_986_3_1/end",    "lr/987_986_3_1/num",     "lr/987_986_3_1/r_num",   "lr/987_986_3_1/conduct",    "lr/987_986_3_1/state",    "lr/987_986_3_1/weight"  ]
     , [ "lr/129_180_3_1/start",    "lr/129_180_3_1/end",    "lr/129_180_3_1/num",     "lr/129_180_3_1/r_num",   "lr/129_180_3_1/conduct",    "lr/129_180_3_1/state",    "lr/129_180_3_1/weight"  ]
     , [ "lr/129_25_3_2/start",     "lr/129_25_3_2/end",     "lr/129_25_3_2/num",      "lr/129_25_3_2/r_num",    "lr/129_25_3_2/conduct",     "lr/129_25_3_2/state",     "lr/129_25_3_2/weight"  ]
     , [ "lr/31_1850_3_2/start",    "lr/31_1850_3_2/end",    "lr/31_1850_3_2/num",     "lr/31_1850_3_2/r_num",   "lr/31_1850_3_2/conduct",    "lr/31_1850_3_2/state",    "lr/31_1850_3_2/weight"  ]
     , [ "lr/1621_31_3_1/start",    "lr/1621_31_3_1/end",    "lr/1621_31_3_1/num",     "lr/1621_31_3_1/r_num",   "lr/1621_31_3_1/conduct",    "lr/1621_31_3_1/state",    "lr/1621_31_3_1/weight"  ]
     , [ "lr/1660_26_3_3/start",    "lr/1660_26_3_3/end",    "lr/1660_26_3_3/num",     "lr/1660_26_3_3/r_num",   "lr/1660_26_3_3/conduct",    "lr/1660_26_3_3/state",    "lr/1660_26_3_3/weight"  ]
     , [ "lr/1660_26_3_4/start",    "lr/1660_26_3_4/end",    "lr/1660_26_3_4/num",     "lr/1660_26_3_4/r_num",   "lr/1660_26_3_4/conduct",    "lr/1660_26_3_4/state",    "lr/1660_26_3_4/weight"  ]
     , [ "lr/1630_1660_1_2/start",  "lr/1630_1660_1_2/end",  "lr/1630_1660_1_2/num",   "lr/1630_1660_1_2/r_num", "lr/1630_1660_1_2/conduct",  "lr/1630_1660_1_2/state",  "lr/1630_1660_1_2/weight"  ]
     , [ "lr/1630_1660_1_3/start",  "lr/1630_1660_1_3/end",  "lr/1630_1660_1_3/num",   "lr/1630_1660_1_3/r_num", "lr/1630_1660_1_3/conduct",  "lr/1630_1660_1_3/state", "lr/1630_1660_1_3/weight"  ]
     , [ "lr/1630_1621_3_1/start",  "lr/1630_1621_3_1/end",  "lr/1630_1621_3_1/num",   "lr/1630_1621_3_1/r_num", "lr/1630_1621_3_1/conduct",  "lr/1630_1621_3_1/state", "lr/1630_1621_3_1/weight"  ]
     , [ "lr/175_26_3_1/start",     "lr/175_26_3_1/end",     "lr/175_26_3_1/num",      "lr/175_26_3_1/r_num",    "lr/175_26_3_1/conduct",     "lr/175_26_3_1/state",     "lr/175_26_3_1/weight"  ]
     , [ "lr/175_26_3_2/start",     "lr/175_26_3_2/end",     "lr/175_26_3_2/num",      "lr/175_26_3_2/r_num",    "lr/175_26_3_2/conduct",     "lr/175_26_3_2/state",     "lr/175_26_3_2/weight"  ]
     , [ "lr/175_590_3_3/start",    "lr/175_590_3_3/end",    "lr/175_590_3_3/num",     "lr/175_590_3_3/r_num",   "lr/175_590_3_3/conduct",    "lr/175_590_3_3/state",    "lr/175_590_3_3/weight"  ]
     , [ "lr/175_590_3_4/start",    "lr/175_590_3_4/end",    "lr/175_590_3_4/num",     "lr/175_590_3_4/r_num",   "lr/175_590_3_4/conduct",    "lr/175_590_3_4/state",    "lr/175_590_3_4/weight"  ]
     , [ "lr/175_590_3_5/start",    "lr/175_590_3_5/end",    "lr/175_590_3_5/num",     "lr/175_590_3_5/r_num",   "lr/175_590_3_5/conduct",    "lr/175_590_3_5/state",    "lr/175_590_3_5/weight"  ]
     , [ "lr/175_26_3_6/start",     "lr/175_26_3_6/end",     "lr/175_26_3_6/num",      "lr/175_26_3_6/r_num",    "lr/175_26_3_6/conduct",     "lr/175_26_3_6/state",     "lr/175_26_3_6/weight"  ]
     , [ "lr/590_175_3_2/start",    "lr/590_175_3_2/end",    "lr/590_175_3_2/num",     "lr/590_175_3_2/r_num",   "lr/590_175_3_2/conduct",    "lr/590_175_3_2/state",    "lr/590_175_3_2/weight"  ]
     , [ "lr/576_577_3_1/start",    "lr/576_577_3_1/end",    "lr/576_577_3_1/num",     "lr/576_577_3_1/r_num",   "lr/576_577_3_1/conduct",    "lr/576_577_3_1/state",    "lr/576_577_3_1/weight"  ]
     , [ "lr/576_180_3_2/start",    "lr/576_180_3_2/end",    "lr/576_180_3_2/num",     "lr/576_180_3_2/r_num",   "lr/576_180_3_2/conduct",    "lr/576_180_3_2/state",    "lr/576_180_3_2/weight"  ]
     , [ "lr/180_129_3_1/start",    "lr/180_129_3_1/end",    "lr/180_129_3_1/num",     "lr/180_129_3_1/r_num",   "lr/180_129_3_1/conduct",    "lr/180_129_3_1/state",    "lr/180_129_3_1/weight"  ]
     , [ "lr/180_576_3_2/start",    "lr/180_576_3_2/end",    "lr/180_576_3_2/num",     "lr/180_576_3_2/r_num",   "lr/180_576_3_2/conduct",    "lr/180_576_3_2/state",    "lr/180_576_3_2/weight"  ]
     , [ "lr/480_469_3_1/start",    "lr/480_469_3_1/end",    "lr/480_469_3_1/num",     "lr/480_469_3_1/r_num",   "lr/480_469_3_1/conduct",    "lr/480_469_3_1/state",    "lr/480_469_3_1/weight"  ]
     , [ "lr/2921_2919_3_1/start",  "lr/2921_2919_3_1/end",  "lr/2921_2919_3_1/num",   "lr/2921_2919_3_1/r_num", "lr/2921_2919_3_1/conduct",  "lr/2921_2919_3_1/state",  "lr/2921_2919_3_1/weight"  ]
     , [ "lr/147_175_3_2/start",    "lr/147_175_3_2/end",    "lr/147_175_3_2/num",     "lr/147_175_3_2/r_num",   "lr/147_175_3_2/conduct",    "lr/147_175_3_2/state",    "lr/147_175_3_2/weight"  ]
     , [ "lr/147_1817_3_1/start",   "lr/147_1817_3_1/end",   "lr/147_1817_3_1/num",    "lr/147_1817_3_1/r_num",  "lr/147_1817_3_1/conduct",   "lr/147_1817_3_1/state",   "lr/147_1817_3_1/weight"  ]
     , [ "lr/31_25_3_1/start",      "lr/31_25_3_1/end",      "lr/31_25_3_1/num",       "lr/31_25_3_1/r_num",     "lr/31_25_3_1/conduct",      "lr/31_25_3_1/state",      "lr/31_25_3_1/weight"  ]
	 , [ "lr/1660_1631_1_1/start",  "lr/1660_1631_1_1/end",  "lr/1660_1631_1_1/num",   "lr/1660_1631_1_1/r_num", "lr/1660_1631_1_1/conduct",  "lr/1660_1631_1_1/state",  "lr/1660_1631_1_1/weight"  ]
	 , [ "lr/1660_1631_2_2/start",  "lr/1660_1631_2_2/end",  "lr/1660_1631_2_2/num",   "lr/1660_1631_2_2/r_num", "lr/1660_1631_2_2/conduct",  "lr/1660_1631_2_2/state",  "lr/1660_1631_2_2/weight"  ]
	 , [ "lr/900_938_3_5/start",    "lr/900_938_3_5/end",    "lr/900_938_3_5/num",     "lr/900_938_3_5/r_num",   "lr/900_938_3_5/conduct",    "lr/900_938_3_5/state",    "lr/900_938_3_5/weight"  ]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END LINKS REACT

  ];



csv_template( "pred.csv" )->
  [  
     [ {const,"_pn"}, "ss1/p_pp_uz_s",     "ss1/p_pp_kz_s",     "ss1/p_pp_kn_s",      "ss1/kzu_norm_s",      "ss1/kzu_jakobian_s",      "ss1/kn_s",      "ss1/p_mdp_kzu_s",     "ss1/p_mdp_kz_s",      "ss1/p_mdp_kn_s" ,    {const,"_"}   ]
   , [ {const,"_pn"}, "ss2/p_pp_uz_s",     "ss2/p_pp_kz_s",     "ss2/p_pp_kn_s",      "ss2/kzu_norm_s",      "ss2/kzu_jakobian_s",      "ss2/kn_s",      "ss2/p_mdp_kzu_s",     "ss2/p_mdp_kz_s",      "ss2/p_mdp_kn_s" ,    {const,"_"}   ]
   , [ {const,"_pn"}, "ss3/p_pp_uz_s",     "ss3/p_pp_kz_s",     "ss3/p_pp_kn_s",      "ss3/kzu_norm_s",      "ss3/kzu_jakobian_s",      "ss3/kn_s",      "ss3/p_mdp_kzu_s",     "ss3/p_mdp_kz_s",      "ss3/p_mdp_kn_s" ,    {const,"_"}   ]
   , [ {const,"_pn"}, "VL5170/p_pp_uz_s",  "VL5170/p_pp_kz_s",  "VL5170/p_pp_kn_s",   "VL5170/kzu_norm_s",   "VL5170/kzu_jakobian_s",   "VL5170/kn_s",   "VL5170/p_mdp_kzu_s",  "VL5170/p_mdp_kz_s",   "VL5170/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL5120/p_pp_uz_s",  "VL5120/p_pp_kz_s",  "VL5120/p_pp_kn_s",   "VL5120/kzu_norm_s",   "VL5120/kzu_jakobian_s",   "VL5120/kn_s",   "VL5120/p_mdp_kzu_s",  "VL5120/p_mdp_kz_s",   "VL5120/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL5394/p_pp_uz_s",  "VL5394/p_pp_kz_s",  "VL5394/p_pp_kn_s",   "VL5394/kzu_norm_s",   "VL5394/kzu_jakobian_s",   "VL5394/kn_s",   "VL5394/p_mdp_kzu_s",  "VL5394/p_mdp_kz_s",   "VL5394/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL2258/p_pp_uz_s",  "VL2258/p_pp_kz_s",  "VL2258/p_pp_kn_s",   "VL2258/kzu_norm_s",   "VL2258/kzu_jakobian_s",   "VL2258/kn_s",   "VL2258/p_mdp_kzu_s",  "VL2258/p_mdp_kz_s",   "VL2258/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL2268/p_pp_uz_s",  "VL2268/p_pp_kz_s",  "VL2268/p_pp_kn_s",   "VL2268/kzu_norm_s",   "VL2268/kzu_jakobian_s",   "VL2268/kn_s",   "VL2268/p_mdp_kzu_s",  "VL2268/p_mdp_kz_s",   "VL2268/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL5300/p_pp_uz_s",  "VL5300/p_pp_kz_s",  "VL5300/p_pp_kn_s",   "VL5300/kzu_norm_s",   "VL5300/kzu_jakobian_s",   "VL5300/kn_s",   "VL5300/p_mdp_kzu_s",  "VL5300/p_mdp_kz_s",   "VL5300/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL5320/p_pp_uz_s",  "VL5320/p_pp_kz_s",  "VL5320/p_pp_kn_s",   "VL5320/kzu_norm_s",   "VL5320/kzu_jakobian_s",   "VL5320/kn_s",   "VL5320/p_mdp_kzu_s",  "VL5320/p_mdp_kz_s",   "VL5320/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL5394/p_pp_uz_s",  "VL5394/p_pp_kz_s",  "VL5394/p_pp_kn_s",   "VL5394/kzu_norm_s",   "VL5394/kzu_jakobian_s",   "VL5394/kn_s",   "VL5394/p_mdp_kzu_s",  "VL5394/p_mdp_kz_s",   "VL5394/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL2029/p_pp_uz_s",  "VL2029/p_pp_kz_s",  "VL2029/p_pp_kn_s",   "VL2029/kzu_norm_s",   "VL2029/kzu_jakobian_s",   "VL2029/kn_s",   "VL2029/p_mdp_kzu_s",  "VL2029/p_mdp_kz_s",   "VL2029/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL2303/p_pp_uz_s",  "VL2303/p_pp_kz_s",  "VL2303/p_pp_kn_s",   "VL2303/kzu_norm_s",   "VL2303/kzu_jakobian_s",   "VL2303/kn_s",   "VL2303/p_mdp_kzu_s",  "VL2303/p_mdp_kz_s",   "VL2303/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL5143/p_pp_uz_s",  "VL5143/p_pp_kz_s",  "VL5143/p_pp_kn_s",   "VL5143/kzu_norm_s",   "VL5143/kzu_jakobian_s",   "VL5143/kn_s",   "VL5143/p_mdp_kzu_s",  "VL5143/p_mdp_kz_s",   "VL5143/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL2183/p_pp_uz_s",  "VL2183/p_pp_kz_s",  "VL2183/p_pp_kn_s",   "VL2183/kzu_norm_s",   "VL2183/kzu_jakobian_s",   "VL2183/kn_s",   "VL2183/p_mdp_kzu_s",  "VL2183/p_mdp_kz_s",   "VL2183/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL2193/p_pp_uz_s",  "VL2193/p_pp_kz_s",  "VL2193/p_pp_kn_s",   "VL2193/kzu_norm_s",   "VL2193/kzu_jakobian_s",   "VL2193/kn_s",   "VL2193/p_mdp_kzu_s",  "VL2193/p_mdp_kz_s",   "VL2193/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "VL2283/p_pp_uz_s",  "VL2283/p_pp_kz_s",  "VL2283/p_pp_kn_s",   "VL2283/kzu_norm_s",   "VL2283/kzu_jakobian_s",   "VL2283/kn_s",   "VL2283/p_mdp_kzu_s",  "VL2283/p_mdp_kz_s",   "VL2283/p_mdp_kn_s" , {const,"_"}   ]
   , [ {const,"_pn"}, "Shu_AT3/p_pp_uz_s", "Shu_AT3/p_pp_kz_s", "Shu_AT3/p_pp_kn_s",  "Shu_AT3/kzu_norm_s",  "Shu_AT3/kzu_jakobian_s",  "Shu_AT3/kn_s",  "Shu_AT3/p_mdp_kzu_s", "Shu_AT3/p_mdp_kz_s",  "Shu_AT3/p_mdp_kn_s", {const,"_"}   ]

  ].

%%d081221


