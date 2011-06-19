in=$1
out=$2
make dve.lex.sml
make dve.grm.sml
echo "DveCompiler.compileDir (\""$in"\", \""$out"\", "\
    "false, SOME TextIO.stdOut);"\
    | sml -Ccontrol.poly-eq-warn=false -m dve.cm
