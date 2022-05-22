fp_lib:login().
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/user_script')").
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"scPstart">>)/=nomatch].


fp_lib:login().
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/user_script')").
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"scStateOTC">>)/=nomatch].


fp_lib:login().
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/user_script')").
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Reactors">>) /= nomatch, string:find(Path, <<"scStateR">>)/=nomatch].
