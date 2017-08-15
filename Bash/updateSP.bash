#!/usr/bin/env bash

IP=
PW=
updateSP(){
    _mpath=`which mysql`
    MYSQL="${_mpath} --compress -userdara -p${PW} -h${IP} -P3306 --default-character-set=utf8 kriita_db1"

    ${MYSQL} -e "call SetprofitTable()";

}

updateSP
