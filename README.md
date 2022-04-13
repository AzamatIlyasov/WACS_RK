# WACS_RK
 
Шпаргалка по работе с FP - WACS

## работа в консоли в браузере
* F12
* Ctrt+F5 - очистка кэши
* Ctrl+Shit+Delete

## работа в консоли в erlang
### для tmux
    tmux ls #список сессий 
    tmux new-session -s fp #создание новой сессии для fp
    tmux a -t fp #подлючение к существующей сессии

### запуск faceplate
    cd ~/faceplate/bin/ 
    ulimit -n 200000 
    sudo ./faceplate console 
    
    #sudo для принудительного запуска в качестве мастера
    sudo FORCE=true ./faceplate console 

### дополнительно
    ./faceplate start #для фонового запуска

    tar -xvf имя_архива.tar.gz #разархивируем файл
    tar -zcvf имя_архива.tar.gz имя_папки/ #архивируем папку
    load purge ( ) #загрузка обновленного файла типа beam
    code:load_file(fp_iot_iec104). 

    ./faceplate remote_console

* CTRL-Z  
    * затем bg  
    * сделать нужные дела  
    * fg


## ZFS
    sudo zfs list #смотрим датасеты ZFS
    sudo zfs list -t snapshot #проверка снапшотов

    sudo zfs rollback yourpoolname/dataset1@snapshotname #возврат к снапшоту


## Заметки - обновление
при обновлении и установке FP:
* обновить файлы в папке release:
 	- vm.args - указать названмие сервера, указать папку где лежит база
 в папке app 
 	- dlss.cfg - as node true - Для резервного сервера, segment limit - индивидуальная настройка - для арчм 32
	- ecomet.cfg - process_memory_limit - увеличить до 2048 - чтобы не было ограничений - только для норм серверов
	- lager.cfg -  увеличить кол-во файлов минимум до 100 - т.к. ошибки могут быть замечены поздно!
* запуск:

        ulimit -n 200000     
        sudo FORCE=true ./faceplate console


* не забудь настроить ZFS и AUTOSNAP

* проверка синхронизации: 

        length(dlss:get_local_segments).
        [  ||  ]
        dlss:get_storaget().
        dlss:get_segments().



# Работа с fbprophet
## start

надо в директорию operator перейти где лежит docker-compose.yaml

    tmux ls
    tmux a -t docker_operator
    tmux a -t ...
    docker-compose up -d


docker ps
docker ps -a 
ls

docker stop #addNameContainer

service/operator-service
service/prophet-service
