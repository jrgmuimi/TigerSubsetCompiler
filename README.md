# TigerSubsetCompiler
This "compiler" is not a compiler in the traditional sense and makes a lot of assumptions that will be listed down below. The compiler performs lexical and syntax analysis using a lexer/parser as required for generating code, however semantic analysis and code optimization are not done in this program. When making the "compiler," the main focus was generating runnable and faithful MIPS code.

Assumptions:

1. All input programs are free of semantic errors.

2. All functions have four or fewer parameters, and all parameters of the integer type.

3. If there is a return value for a function, it must be of the integer type.

4. The only variables we have are integer variables (No string variables or other types).

5. Strings are only used as constants with the "printstring" system call.

6. The condition supplied in a for-loop is static and doesn't change value throughout execution of the for-loop. See G5_hardFor2 in the testcases/cases folder (this test infinitely loops).

Language Specifications:

This subset of the Tiger programming language was introduced in the undergraduate Compilers course I took at GMU and is dubbed "Toyger". To see the lexical/syntax specifications for Toyger, please refer to the "language" folder and the example Toyger programs in the "testcases" folder. Also included in the "language" folder are the semantic rules for Toyger.

To run a testcase and compile/run an example Toyger program, simply use the provided shell script: ./run.sh [TEST-CASE-NAME]

For example: ./run.sh G5_hardFor2. The expected outputs for each Toyger program are provided in the testcases/expected_output folder. You may notice that some programs have different output variations such as G4_basicInput1@1.out and G4_basicInput1@2.out. This is because G4_basicInput1 takes user input using getint(). You can supply the correct input argument for the testcase by using the appropriate input file: ./run.sh G4_basicInput1 < testcases/cases/inputs/G4_basicInput1@1.in and ./run.sh G4_basicInput1 < testcases/cases/inputs/G4_basicInput1@2.in

The input files for several interactable programs are provided in the "inputs" folder. Whenever you run the shell script on a target program, the MIPS assembly output or .S file will be stored in the asm_output folder. So, if you want to view the generated code, please refer to the asm_output folder. Beware that this folder gets cleared when calling "make clean".
