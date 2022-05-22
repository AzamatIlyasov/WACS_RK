fp_lib:login().
{_, PathAndTrigger} = fp_db:query("get .path, tag_trigger from root where .pattern=$oid('/root/.patterns/user_script')").
TagPath = <<"/root/PROJECT/TAGS/branches/vl1101/vtag">>,
[Path|| [Path, Trigger] <- OidAndPath, case fp_lib:from_json(Trigger) of #{<<"settings">> := #{ <<"tag">> := TagPath}} -> true; _ -> false end].
