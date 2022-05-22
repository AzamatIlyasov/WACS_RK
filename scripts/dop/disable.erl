fp_lib:login().
OidList = fp_db:query("get .oid from root where .pattern=$oid('/root/.patterns/user_script')").
[fp_db:edit_object(fp_db:open( Oid), #{<<"disabled">> => true} ) || Oid<- OidList].
