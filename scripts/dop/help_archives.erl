fp_lib:login().
ArchiveOid = fp_db:query("get .oid from root where .pattern=$oid('/root/.patterns/ARCHIVE')").
%Это удалит все архивы в вашем проекте
[ fp_db:delete_object(fp_db:open(Oid)) || Oid <-  ArchiveOid].

ArchiveLog = fp_db:query("get .oid from * where .pattern=$oid('/root/.patterns/archive_log')").
%%А это удалит все архив логи
[fp_db:delete_object(fp_db:open(Oid))||Oid<- ArchiveLog].
%%Теперь попробуйте заново импортнуть архивы

%% очистка не нужных архивов - в папках wams - state
fp_lib:login().
f(OidAndPath).
f(Oid).
f(Path).
f(ArchiveLog).
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/ARCHIVE')").
[fp_db:delete_object(fp_db:open( Oid)) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"wams">>) /= nomatch, string:find(Path, <<"state">>)/=nomatch].
ArchiveLog = fp_db:query("get .oid from * where .pattern=$oid('/root/.patterns/archive_log')").
[fp_db:delete_object(fp_db:open( Oid)) || [Oid, Path] <- ArchiveLog, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"wams">>) /= nomatch, string:find(Path, <<"state">>)/=nomatch].


%% очистка не нужных архивов - в папках wams - _ras
fp_lib:login().
f(OidAndPath).
f(Oid).
f(Path).
f(ArchiveLog).
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/ARCHIVE')").
[fp_db:delete_object(fp_db:open( Oid)) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"wams">>) /= nomatch, string:find(Path, <<"_ras">>)/=nomatch].
ArchiveLog = fp_db:query("get .oid from * where .pattern=$oid('/root/.patterns/archive_log')").
[fp_db:delete_object(fp_db:open( Oid)) || [Oid, Path] <- ArchiveLog, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"wams">>) /= nomatch, string:find(Path, <<"_ras">>)/=nomatch].



%% очистка не нужных архивов - в папках wams - archivesP _status
fp_lib:login().
f(OidAndPath).
f(Oid).
f(Path).
f(ArchiveLog).
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/ARCHIVE')").
[fp_db:delete_object(fp_db:open( Oid)) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"wams">>) /= nomatch, string:find(Path, <<"archivesP">>) /= nomatch, string:find(Path, <<"_status">>)/=nomatch].
ArchiveLog = fp_db:query("get .oid from * where .pattern=$oid('/root/.patterns/archive_log')").
[fp_db:delete_object(fp_db:open( Oid)) || [Oid, Path] <- ArchiveLog, string:prefix(Path, <<"/root/PROJECT/TAGS/Branches">>) /= nomatch, string:find(Path, <<"wams">>) /= nomatch, string:find(Path, <<"archivesP">>) /= nomatch, string:find(Path, <<"_status">>)/=nomatch].


%% очистка не нужных архивов - везде - archivesP _status
fp_lib:login().
f(OidAndPath).
f(Oid).
f(Path).
f(ArchiveLog).
{_, OidAndPath} = fp_db:query("get .oid, .path from root where .pattern=$oid('/root/.patterns/ARCHIVE')").
[fp_db:delete_object(fp_db:open( Oid)) || [Oid, Path] <- OidAndPath, string:prefix(Path, <<"/root/PROJECT/TAGS/">>) /= nomatch, string:find(Path, <<"archivesP">>) /= nomatch, string:find(Path, <<"_status">>)/=nomatch].
ArchiveLog = fp_db:query("get .oid from * where .pattern=$oid('/root/.patterns/archive_log')").
[fp_db:delete_object(fp_db:open( Oid)) || [Oid, Path] <- ArchiveLog, string:prefix(Path, <<"/root/PROJECT/TAGS/">>) /= nomatch, string:find(Path, <<"archivesP">>) /= nomatch, string:find(Path, <<"_status">>)/=nomatch].
