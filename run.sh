#!/bin/sh
set -e
# to run this script: 
#   - make sure you have the executable
#   - run with "./run.sh TEST_CASE_NAME"
#   - e.g. "./run.sh test0"
INPUTDIR=./testcases/cases
OUTPUTDIR=./asm_output
MARS=./mars/Mars4_5.jar
NAME=TigerSubsetCompiler

echo
echo "Testing with $1.tog:"
echo "-------------------------------------------------------------"
echo " - Generating MIPS translation: $1.s"
echo "-------------------------------------------------------------"
./$NAME <$INPUTDIR/$1.tog >$OUTPUTDIR/$1.s

echo "-------------------------------------------------------------"
echo " - Running MIPS code generated ($OUTPUTDIR/$1.s): "
echo "-------------------------------------------------------------"
java -jar $MARS $OUTPUTDIR/$1.s 
echo

