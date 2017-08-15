#!/usr/bin/env bash
#  Will use what ever bash is installed
#  payLevo.sh
#  USAGE: payLevo.bash -s "2014-01-01" -e "2014-11-21" -d "all"
# to update the whole database
# for single directories then
#   payLevo.bash -m -s "2014-01-01" -e "2014-11-21" -d "Invoice"
#  
#  Created by Serdar Akin on 2014-11-21.
# 



#------------------------------------------------------------
# Function needed to enable the analysis
#------------------------------------------------------------
GREEN="\033[0;32m"
NO_COLOUR="\033[0m"
RED="\033[1;31m"
BLUE="\033[0;34m"
LIGHT_BLUE="\033[1;34m"
PURPLE="\033[1;35m"
BLACK="\033[1;30m"
RR="/usr/bin/Rscript --verbose"
## Dirs
RSRC="./Rsrc"
RClass="./RClass"
LATEX="./LatexSrc"
_HOME="$PWD"
## All the R files in each directory
RFiles=$(find $RSRC -iname '*.R')
RClass=$(find $RClass -iname '*.R')


#------------------------------------------------------------
# Parse command line options/arguments
#------------------------------------------------------------
if (! getopts "vhs:e:d:c:" name); then
    echo "Usage: ${0##*/} script,-h for help"
exit $E_OPTERROR
fi

while getopts vh:d:c: option
do
case ${option} in
    v) version;
    exit ;;
    d) DIR="$OPTARG" ## Returns the argumen
    ;;
    c) company="$OPTARG"
    ;;
    \?) Syntax;
    alert "${scriptname}: usage: [-m ], [-s startData], [-e endDate] [-d path]"
    exit 2
    ;;
esac
done
shift "$(( OPTIND - 1 ))"
company=${company:-"all"}

## Se if endDate time is empty, if empty than put todays date
endDate=${endDate:="$(date +'%Y-%m-%d')"}

## Get the lower case from input
_FILE=$(echo "${DIR}" | tr '[:upper:]' '[:lower:]')
[ ! -d "$_FILE" ] && mkdir "$_FILE" || echo "$_FILE exists"
if [[ "${_FILE}" =~ ^[Ii]nvestor* ]]
    then
    tmpFile="invoice"
    else
    tmpFile="${_FILE}"
fi


printf "Going to update database ${RED}(%s)${BLACK}, for file ${GREEN}(%s)${BLACK} with PHP\n" "${_FILE}" "${tmpFile}"
sleep 1s


_RCompile=$(find ${RFiles} -iname "*$tmpFile*" -type f)
## First thest if the nr of files that meet the above statement
## if it meets than run the R source file else exit and try again
_FileHome=$(dirname $_RCompile)
_FileName=$(basename $_RCompile)
if test $(echo "$_RCompile"|wc -l) -eq 1
    then
## Test if dir exist, if it does move to that
        test -d ${_FileHome} && cd ${_FileHome} || exit 0
        printf "Running source file ${RED}[%s]${BLACK}\n" "$_FileName"
        time $RR "${_FileName}" "${company}"
        Status=1
        cd ../
    else
    Status=0
    printf "Cannot execute the the R-script for (%s) please check input again\n" "${_RCompile}"
    exit 0
fi
###########################################################################
# Find all LaTeX file that match the input
###########################################################################
_LatexCompile=$(find -E ${LATEX} -regex ".*${_FILE}.*(rnw|Rnw)$" -type f)
## Run LaTeX file
## If status is eq to 1 than run the Latex file
if test $(echo "$Status") -eq 1
    then
    _FileHome=$(dirname $_LatexCompile)
    _FileName=$(basename $_LatexCompile)
    echo $_FileHome
    cp "${_LatexCompile}" "${_FILE}"
    cd "${_FILE}"
    compile.sh $_FileName
    cd ../
    else
    printf "Didn't run the R-script $RED[%s]$BLACK sucessfully, please check error and try again\ns" "$_FileName"
fi



