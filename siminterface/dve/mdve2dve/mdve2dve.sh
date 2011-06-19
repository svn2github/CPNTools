#####
#
#  File: mdve2dve.sh
#
#  Created:
#     Nov. 27, 2007
#
#  Description:
#     Generate a dve file from an mdve file containing m4 macros.
#
#  Usage:
#     mdve2dve.sh {PARAM=VALUE}* MODEL-NAME
#
#  Example:
#     mdve2dve.sh N=4 ERROR=1 peterson
#     > generates dve file peterson.N=4.ERROR=1.dve from mdve file
#       peterson.mdve (which must be in the current directory).  parameters N
#       and ERROR are set to 4 and 1.
#
#####


m4Defs="m4_defs"
define=""
outFile=""
i=1
model=""
blank="/tmp/blank"
tmp="/tmp/model.mdve"

if ! test -e $m4Defs; then
    echo "Missing definition file $m4Defs!!!"
    exit
fi

if test $# -eq 0; then
    echo "Expecting model name!!!"
    echo "Usage: mdve2dve.sh {PARAM=VALUE}* MODEL-NAME"
    exit
fi

for arg in "$@"; do
    if test $i -eq $#; then
	model="$arg"
    else
	define="$define --define=$arg"
	if test $i -eq 1; then
	    outFile=".$arg"
	else
	    outFile="$outFile.$arg"
	fi
    fi
    i=$(($i + 1))
done

if ! test -e $inFile; then
    echo "Missing model file $inFile!!!"
    exit
fi

inFile="$model.mdve"
outFile="$model$outFile.dve"

echo "" >> $blank
echo "" >> $blank

cat $m4Defs $blank $inFile > $tmp

m4 $define $tmp > $outFile

echo "Successfully created output file $outFile"

rm $blank
rm $tmp