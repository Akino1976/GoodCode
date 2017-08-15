#!/usr/bin/env bash
#  Will use what ever bash is installed
#  checkData
#  USAGE: ./swData.bash -m -f "<script>" -d "output" -c "score|bolag"
#   -m) Will email the person in php file
#   -d) Which directory to search for the data
#   -f) Which Rscript to run
#   -c) Conditions run into R|php script
# 
#  Created by Serdar Akin on 2016-10-20.
#USAGE ./swData.bash -m -f 'sessionCheck.R' -c 'session'
#



#------------------------------------------------------------
# Function needed to enable the analysis
#------------------------------------------------------------
PATH=$PATH:/usr/bin
GREEN="\033[0;32m"
NO_COLOUR="\033[0m"
RED="\033[1;31m"
BLUE="\033[0;34m"
LIGHT_BLUE="\033[1;34m"
PURPLE="\033[1;35m"
BLACK="\033[1;30m"
Rpath=$(which Rscript);
RR="${Rpath} --verbose"


#------------------------------------------------------------
# Parse command line options/arguments
#------------------------------------------------------------
if (! getopts "hmf:d:c:" name); then
    echo "Usage: ${0##*/} script,-h for help"
    exit $E_OPTERROR
fi

while getopts hmf:d:c: option
do
case ${option} in
    h) usage;
    exit;;
    v) version;
    exit ;;
    f) Rfile="$OPTARG"
    ;;
    c) cond="$OPTARG"
    ;;
    d) DIR="$OPTARG" ## Returns the argumen
    ;;
    m) _MAIL="yes"
    ;;
    \?) Syntax;
    alert "${scriptname}: usage: [-m ], [-d <dataDir>], [-f <Rfile> ] [-c <condition>]"
    exit 2
    ;;
    esac
done
shift "$(( OPTIND - 1 ))"

argum=${DIR:="Data"}
Rfile=${Rfile:="sessionCheck.R"}
logs="./logs"
#------Made for regexp purpose------------#

_cond="${cond}"


_RFile=$(find . -iname "*${Rfile}*" -type f)
_RFile1="./"$(basename "${_RFile}")





currentDay="$(date +'%Y-%m-%d')"
if [ "$(uname -s)" == "Darwin" ];
then
    #----One day back---#
    HOMEDIR="${PWD}"
    _HOME="${HOMEDIR}"
    _DATA="$HOMEDIR/${argum}"
    yesterDay="$(date -j -v-1d -f '%Y-%m-%d' ${currentDay} '+%Y-%m-%d')"
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ];
then
    HOMEDIR="/home/serdara"
    _HOME="$HOMEDIR/SweetPay"
    _DATA="$HOMEDIR/SweetPay/${argum}"
    yesterDay="$(date -d '1 day ago' '+%Y-%m-%d')"
fi

#------Made for ./src/insertPerson.R when updating collection data------------#
#------./swData.bash -m -f 'insertPerson.R' -c 'insert'------------#
if [ "${_cond}" == "insert" ] || [ "${_cond}" == "survival" ]  ;
then

[[ "${_cond}" == "insert" ]] &&   cd "${_HOME}/src" ||  cd "${_HOME}"
echo ${PWD}
echo "################################################################\n"
printf "# R script %s is about to be executed updated \n path " "${_RFile1}" "${PWD}"
echo "################################################################\n"
    Output=$(exec "${_RFile1}"  "${_cond}" 2>&1)

    if test $(echo ${Output}|grep -c -i "error script") -gt 0
    then
        _message="error"
    else
        _message="updated"
    fi
     cd "${_HOME}"
## Here message will be if the database has been updated or not, and --what insert
    php Project/email.php --catch="${_message}" --what="${_cond}" 2>&1
exit 1
fi


#---------------------------------------------------------------------------#


cd "${_HOME}"
echo "${_DATA} ${_MAIL} ${yesterDay} ${_Rfile1} ${_cond}"
ls
echo "################################################################\n"
printf "# R script %s is about to be executed updated " "${_RFile1}"
echo "################################################################\n"

printf "## Run %s with condition %s ##\n"  "${_RFile1}" "${_cond}"
Output=$(exec "${_RFile1}" "${_cond}" 2>&1)

if test $(echo ${Output}|grep -c -i "succes") -gt 0
then
    _xlsx=$(find "${_DATA}" -iname "*${_cond}*${currentDay}*" -type f)
    _xlsx1=$(basename "${_xlsx}")

    echo "What ${_xlsx1}"
    [[ -z "${_xlsx1}" ]] && { echo "Parameter xlsx is empty" ; exit 1; }
    echo "################################################################\n"
    printf "# R script were sucessufully updated for %s \n" "${_RFile1}"
    echo "################################################################\n"
    ## If -m not given at command then no email
    if test -n "${_MAIL}"
    then
        php Project/email.php --catch="${_xlsx1}" --what="${_cond}" 2>&1
    fi
else
    echo "################################################################\n"
    printf "# R  faild to updated for %s \n" "${_FileName}"
    echo "${Output}"
    echo "################################################################\n"

    exit 0
fi







