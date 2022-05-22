#!/bin/bash

echo "The current directory is:"
pwd


# move result files from ulitit to archive
cd archive/
mv !(_old) _old



# for file in *; do
#   if [ -d "$file" ]; then
#     case $file in
#       неменя|нет|прикольная)
#         echo "пропускаем $file"
#         ;;
#       *)
#         echo "перемещаем $file"
#         mv $file кудато
#         ;;
#     esac;
#   else
#     echo "$file не папка";
#   fi
# done
