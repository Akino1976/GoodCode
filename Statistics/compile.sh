################################################################
## USAGE: compile.sh <FIL.Rnw>
#! /bin/bash

DOK="Dokument"
TRASH="logFiles"
dirtFiles="*.aux *.fls *.log *.out *.toc *.*_*"

[ ! -d $DOK ] && mkdir $DOK $TRASH || echo "$DOK exists"


for i in "$@"; do

OutPut=$i


## Find the extension
Extension=${OutPut##*.}
## And whitout extension
fileNoExt=${OutPut%%.$Extension}


## Check if there is any R-code in the file
Codes=$(grep '<<.*>>=' $OutPut)

if [ ${#Codes} -eq 0 ]
then
    echo "-------------No need to  compile with Rnw---------------"
    sleep 1
    latexmk -g -f ${fileNoExt}.Rnw
else
    echo "------------Going to compile the file ---------------"
    echo "-----------------------------------------------------"
    sleep 1
    R CMD Sweave --encoding=utf8 ${OutPut}
    latexmk -g -f ${fileNoExt}.tex
fi

done
    mv ${fileNoExt}.pdf $DOK
    open $DOK/${fileNoExt}.pdf
    rm -f $dirtFiles
