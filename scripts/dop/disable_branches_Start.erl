%%% скрипты для консоли ERLANG для манипулации шаблонами - отключение/включение

%% BRANCHES - user_script disabled
fp_lib:login().
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/user_script')").
%%%% scPstart - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"scPstart">>)/=nomatch].
%%%% scPend - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"scPend">>)/=nomatch].
%%%% scQstart - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"scQstart">>)/=nomatch].
%%%% scQend - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"scQend">>)/=nomatch].
%%%% scStateOTC - disabled FALSE
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"scStateOTC">>)/=nomatch].


%% Reactors - user_script disabled
fp_lib:login().
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/user_script')").
%%%% scPstart - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Reactors">>) /= nomatch, string:find(Path, <<"scStateR">>)/=nomatch].


f().
%% scdPbalance - user_script enabled
fp_lib:login().
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/user_script')").
%%%% scdPbalance - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"scPload_sc">>)/=nomatch].

[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"scPgen_sc">>)/=nomatch].

[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"scQload_sc">>)/=nomatch].

[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"scQgen_sc">>)/=nomatch].



[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"scdPbalance">>)/=nomatch].


f(OidAndPath).
f(Oid).
f(Path).

f().
fp_lib:login().
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/user_script')").
%%%% scdPbalance - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"scdPbalance">>)/=nomatch].



%% NODES - archives disabled
fp_lib:login().
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/ARCHIVE')").
%%%% p_load_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"p_load_status">>)/=nomatch].
%%%% q_load_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"q_load_status">>)/=nomatch].
%%%% p_gen_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"p_gen_status">>)/=nomatch].
%%%% q_gen_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"q_gen_status">>)/=nomatch].
%%%% u_n_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"u_n_status">>)/=nomatch].
%%%% u_fi_n_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"u_fi_n_status">>)/=nomatch].
%%%% state_n_ras - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"state_n_ras">>)/=nomatch].

%%%% /archives/p - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"/archives/p">>)/=nomatch].
%%%% /archives/q - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"/archives/q">>)/=nomatch].
%%%% /archives/u - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"/archives/u">>)/=nomatch].
%%%% /archives/state - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"/archives/state">>)/=nomatch].
%%%% /archives/fiu - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"/archives/fiu">>)/=nomatch].



%%%% /archives/p - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"p_gen">>)/=nomatch].
%%%% /archives/q - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"q_gen">>)/=nomatch].
%%%% /archives/u - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"p_load">>)/=nomatch].
%%%% /archives/state - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"q_load">>)/=nomatch].
%%%% /archives/fiu - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"u_fi_n">>)/=nomatch].



%% BRANCHES - archives disabled
fp_lib:login().
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/ARCHIVE')").
%%%% p_start_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"p_start_status">>)/=nomatch].
%%%% p_end_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"p_end_status">>)/=nomatch].
%%%% q_start_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"q_start_status">>)/=nomatch].
%%%% q_end_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"q_end_status">>)/=nomatch].


[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"p_start">>)/=nomatch].
%%%% p_end_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"p_end">>)/=nomatch].
%%%% q_start_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"q_start">>)/=nomatch].
%%%% q_end_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"q_end">>)/=nomatch].




%% _RAS - archives disabled
f().
fp_lib:login().
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/ARCHIVE')").
%%%% archive branches disables
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"predict_state_b">>) /= nomatch].
%%%WAIT
%%%% archive branches disables
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"predict_state_b">>) /= nomatch].

%%%% p_start_status - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:find(Path, <<"_ras">>) /= nomatch].

%%%% _sc- disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:find(Path, <<"_sc">>) /= nomatch].

%%%% predict_- disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:find(Path, <<"predict_p_gen">>) /= nomatch].

%%%% predict_- disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:find(Path, <<"predict_q_gen">>) /= nomatch].

%%%% predict_- disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:find(Path, <<"predict_p_load">>) /= nomatch].

%%%% predict_- disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:find(Path, <<"predict_q_load">>) /= nomatch].

%%%% predict_- disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:find(Path, <<"predict_rstate">>) /= nomatch].

%%%% predict_- disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:find(Path, <<"predict_vstate">>) /= nomatch].

%%%% u_n- disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:find(Path, <<"u_n">>) /= nomatch].

%%%% predict_- disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:find(Path, <<"predict_vstate">>) /= nomatch].


%%%% u_n_ras - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes/">>) /= nomatch, string:find(Path, <<"u_n_ras">>) /= nomatch].

%%%% u_n - disabled
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => false} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes/">>) /= nomatch, string:find(Path, <<"u_n">>) /= nomatch].


f(OidAndPath).
fp_lib:login().

{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/user_script')").
OidAndPathFilter = [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"scdPbalance">>)/=nomatch].

[[Oid1,Path1]|_] = OidAndPathFilter.
FpDbOid1=fp_db:open( Oid1 ). %PathToTag -> Oid1

%get current script
{ok,ScrTextOid1}=fp_db:read_field(FpDbOid1, <<"script">>). % {ok, val}
%find string, where will be change
ScriptOid1 = string:find(ScrTextOid1, "],\"script\":\""). 
%get Head scritp
[ScriptOid1Head, _] = string:split(ScrTextOid1,"],\"script\":\"").

%get script new ->
OidNew = {55,1121}.
FpDbOidNew=fp_db:open( OidNew ).
{ok,ScrTextOidNew}=fp_db:read_field(FpDbOidNew, <<"script">>).
ScriptNew = string:find(ScrTextOidNew, "],\"script\":\""). 

%concat Head and New Tail (change script)
ScrTextOid1New = <<ScriptOid1Head/binary,ScriptNew/binary>>.

%change old script whith new script - END.
fp_db:edit_object(FpDbOid1, #{<<"script">> => ScrTextOid1New} 



f().
TagIDs = fp_db:query(<<"get .oid from root where andnot( .pattern=$oid('/root/.patterns/model_control'), disabled=true, isTriggered=false )">>).
[TagID|_] = TagIDs.
Tag = fp_db:open(TagID,none).
fp_db:edit_object( Tag, #{ <<"trigger">> => 0 } ).

%% Script must be an Erlang fun of arity 2.
%% - VARS argument is a map that contains
%%     values for the defined variables.
%%     Key - variable name, Value - variable value
%% - State is an argument that can be used as an accumulator.
%%     It keeps the value of the accumulator that was returned
%%     from the previous call
%% - Set Value in order to set a new value for a field, you need to return an tuple, 
%%     the first parameter is a map of variables and values, the second parameter is a state.
%%     for example: { #{'var_2' => Var_1}, _State }
%% The function must return a tuple:
%%      { <values_map>, <accumulator> }
%%
fun(VARS,_State)->
    fp:log(info,"DEBUG change user sripts: START"),
f().
fp_lib:login().
    {_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/user_script')").
    [
    begin 
        fp:log(info,"DEBUG change user sripts: Start Edit ~p", Path),
        FpDbOid=fp_db:open( Oid ),
        %get current script%
        {ok,ScrTextOid}=fp_db:read_field(FpDbOid, <<"script">>),
        %find string, where will be change%
        %ScriptOid = string:find(ScrTextOid, "],\"script\":\""),%
        %get Head scritp%
        [ScriptOidHead, _] = string:split(ScrTextOid,"],\"script\":\""),
        %get script new -> ALMA-u220-script-scdPbalance%
        OidNew = {55,1121},
        FpDbOidNew=fp_db:open( OidNew ),
        {ok,ScrTextOidNew}=fp_db:read_field(FpDbOidNew, <<"script">>),
        ScriptNew = string:find(ScrTextOidNew, "],\"script\":\""),
        %concat Head and New Tail (change script)%
        ScrTextOidNew = <<ScriptOidHead/binary,ScriptNew/binary>>,
        %change old script whith new script - END.%
        ok = fp_db:edit_object(FpDbOid, #{<<"script">> => ScrTextOidNew}),
        fp:log(info,"DEBUG change user sripts: END Edit ~p", Path),
        f(FpDbOid,ScrTextOid),
        f(ScrTextOid),        
        f(ScriptOidHead),
        f(OidNew),
        f(FpDbOidNew),
        f(ScrTextOidNew)
     end || [Oid, Path] <-
        OidAndPath, 
        string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, 
        string:find(Path, <<"scdPbalance">>)/=nomatch 
    ].

    fp:log(info,"DEBUG change user sripts: DONE"),
    { none, none }
end.
    




OidNew = {55,1121}. 
FpDbOidNew=fp_db:open( OidNew ).
{ok,ScrTextOidNew}=fp_db:read_field(FpDbOidNew, <<"script">>).
ScriptNew = string:find(ScrTextOidNew, "],\"script\":\""). 
ScrTextOidNew_new = 
ScriptNew = <<"">>.
[ScriptNewHead, ScriptNewTail] = string:split(ScriptNew,"],\"script\":\"").

string:replace(ScrTextOidNew, "..", "*", all).

% %%fp_db:read_field(object, [«"value"», «"lock_timer"»])
% ],"script":"
%% "],\"script\":\" "
%string:next_codepoint(ScrTextOid1)
%string:slice(<<"He̊llö Wörld"/utf8>>, 4).
%{_1,_2,_3,_4,InRes1,_6,_7} = Res1.

%%%% scdPbalance - disabled
%[fp_db:edit_object(fp_db:open( Oid), #{<<"script">> => false} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Nodes">>) /= nomatch, string:find(Path, <<"scdPbalance">>)/=nomatch].
