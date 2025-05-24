# TigerSubsetCompiler
This "compiler" is not a compiler in the traditional sense and makes a lot of assumptions that will be listed down below. The compiler performs lexical and syntax analysis using a lexer/parser as required for generating code, however semantic analysis and code optimization are not done in this program. When making the "compiler," the main focus was generating runnable and faithful MIPS code.

Assumptions:

1. All input programs are free of semantic errors.

2. All functions have four or fewer parameters, and all parameters of the integer type.

3. If there is a return value for a function, it must be of the integer type.

4. The only variables we have are integer variables (No string variables or other types).

5. Strings are only used as constants with the "printstring" system call.

6. The condition supplied in a for-loop is static and doesn't change value throughout execution of the for-loop. See G5_hardFor2 in the testcases/cases folder (this test infinitely loops).
