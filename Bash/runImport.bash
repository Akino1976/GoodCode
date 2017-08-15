#!/usr/bin/env bash

_MAILFROM="akin.bash@gmail.com"
_MAILTO="serdar@protectme.se"

if [ "$(uname)" == "Darwin" ];
then
#-- if mac --#
    _filepath="/Users/akinoosx/Sites/hackad/rsrc/"
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ];
then
    _filepath="/var/www/api.hackad.se/rsrc/"
fi
##############################################################################
# Need to stop execution here it there is no values
##############################################################################
query="select count(*) as nr from prod_Storage where status = 'open'"
user=""
psw=""
database="api_lead"
exec 3>&2
exec 2> /dev/null
while read line
do
    if test $(echo ${line}|grep -c -i "nr:") -gt 0
    then
         clean=$(echo ${line}|sed 's/^nr:\s//g')
         countArray+=("${clean}")
    fi
done < <(mysql --compress -u${user} -p${psw} ${database} -e "${query}")
## check to see if there is any nr that is lt 0 if not exit script
isZero=0
for i in "${countArray[@]}"
do
    if [[ $i -gt 0 ]]
    then
        isZero=$((isZero+1))
    fi
done
unset countArray
exec 2>&3
[[ ${isZero} -eq 0 ]] && exit 0
##############################################################################



runImporting(){
    cd "${_filepath}"
    rfile=$(find . -iname '*insertDB.R*' -type f)
    printf "********%s\n" "${rfile}"
    _rfile1="./"$(basename "${rfile}")
     printf "********%s\n" "$(pwd)"
    printf "********%s\n" "${_rfile1}"
    if [[ -e "${_rfile1}" ]]
    then
        Output=$(exec "${_rfile1}"  2>&1)
        [ $(echo ${Output}|grep -c -i "nothing") -gt 1 ] && exit 0
        if test $(echo ${Output}|grep -c -i "error") -gt 0
        then
                _message="error"
                printf "%s\n" "${Output}"
            else
                _message="updated"
        fi

        echo  "Import status: [${_message}]" | mail -s "Import status" "${_MAILFROM}" "${_MAILTO}"

    else
        echo  "Import status: file dont exists" | mail -s "Import status" "${_MAILFROM}" "${_MAILTO}"
    fi
}

runImporting
