fp_lib:login().
ArchiveOid = fp_db:query("get .oid from root where .pattern=$oid('/root/.patterns/ARCHIVE')").
%Это удалит все архивы в вашем проекте
[ fp_db:delete_object(fp_db:open(Oid)) || Oid <-  ArchiveOid].

ArchiveLog = fp_db:query("get .oid from * where .pattern=$oid('/root/.patterns/archive_log')").
%%А это удалит все архив логи
[fp_db:delete_object(fp_db:open(Oid))||Oid<- ArchiveLog].

%%Теперь попробуйте заново импортнуть архивы
