%{
#include <stdio.h> // For printing
#include <stdlib.h> // For calloc
#include <string.h> // For strcmp
#include <assert.h> // For ssertions

typedef struct scope 
{
	char* name;
	int offset;
	struct scope* next; // This only stores IDs of variables not functions, because functions we must call
} Scope; // Originally I was gonna call this struct ID and have a linked list of IDs
// But the name felt really off to me cause I wanted a global scope and local_scope(s).
// So I just decided to name it Scope and then pretend we're adding a new ID to the scope/
// list everytime we're using it. Idk, it just seems more natural to call it scope.

Scope if_scope = {0}; // To track the ifs and their associated label
Scope for_scope = {0}; // To track the fors and their associated label
Scope local_scope = {0}; // Everytime we encounter a new local scope (function scope) we separate the head from the body
int in_global = 1; // Are we in the global scope. Always start with global scope
int arg_reg_ct = 0; // We use when we're storing arguments into the stack (start at a0, and continue until we have more args and push th em)
int call_arg_reg_ct = 0; // We use this when we're storing data into the argument registers before a call statement (need to know which arg to push data in)
int str_count = 0; // Same as below, use this count to prevent duplicate string .asciiz labels
int if_ct = 0; // Same as below, use this count to prevent duplicate labels and force unique counts
int loop_ct = 0; // Number of loops used for naming purposes and prevent duplicate labels
int offset = 0; // Total offset (number of variables we pushed on the stack) of stack pointer from where it was originally

extern int yylineno;
int yyerror();
extern int yylex();
extern int lex_err_flag;

const char* pass_msg = "Input Accepted\n";
const char* syn_err_msg = "Syntax Error: Line %d\n";

int registers[8] = {0,1,2,3,4,5,6,7}; // Used to see which registers are free and keep track of them (only temporaries t0-t7)

const char* toyger_start = ".text\ntoyger_start:\n"; // The entry point: MAIN
const char* toyger_end = ".text\ntoyger_end:\n\tli $v0, 10\n\tsyscall\n"; // 10 for EXIT

const char* glob_var = ".data\n%s: .word 0\n"; // Print a MIPS global variable, 0 initialize
const char* loc_var_arg = ".text\n\tmove $t0, $a%d\n\taddi $sp, $sp, -4\n\tsw $t0, 0($sp)\n"; // Move a local parameter variable/argument in a function to the stack
const char* loc_var_new = ".text\n\tli $t0, 0\n\taddi $sp, $sp, -4\n\tsw $t0, 0($sp)\n"; // Move a brand new local variable (declared in function) to stack

const char* func_label = ".text\n%s:\n"; // Label for function, IDs SHOULD BE UNIQUE

// These labels/functions are always included in every MIPS asm generated even if the toyger program does not use them
const char* p_i = ".text\nprintint:\n\tli $v0, 1\n\tsyscall\n\tjr $ra\n"; // 1 for printint, $v0, $ra is caller-save
const char* p_str = ".text\nprintstring:\n\tli $v0, 4\n\tsyscall\n\tjr $ra\n"; // 4 for printstring
const char* g_i = ".text\ngetint:\n\tli $v0, 5\n\tsyscall\n\tjr $ra\n"; // 5 for getint

const char* arith_reg = ".text\n\t%s %s, %s, %s\n"; // Perform arithemtic and comparison operations (add, mul, sub, div, slt, ...)

const char* save_before_call = 
	".text\n\t"
	"addi $sp, $sp, -52\n\t" // Save everything except for the return register $v0
	"sw $t0, 0($sp)\n\t" // We will only call this when we jal a function not of the above
	"sw $t1, 4($sp)\n\t" // The reason why I don't see a use in saving the $v0 register is that
	"sw $t2, 8($sp)\n\t" // Later in the code below, if we really need to save it like with getint,
	"sw $t3, 12($sp)\n\t" // Then we will allocate a register to put that value in, else it doesnt matter
	"sw $t4, 16($sp)\n\t" 
	"sw $t5, 20($sp)\n\t"
	"sw $t6, 24($sp)\n\t"
	"sw $t7, 28($sp)\n\t"
	"sw $a0, 32($sp)\n\t"
	"sw $a1, 36($sp)\n\t"
	"sw $a2, 40($sp)\n\t"
	"sw $a3, 44($sp)\n\t"
	"sw $ra, 48($sp)\n"; // PROLOG END

const char* call_and_reload = 
	".text\n\t"
	"jal %s\n\t"			// Need to supply a function here
	"lw $t0, 0($sp)\n\t"
	"lw $t1, 4($sp)\n\t"
	"lw $t2, 8($sp)\n\t"
	"lw $t3, 12($sp)\n\t"
	"lw $t4, 16($sp)\n\t"
	"lw $t5, 20($sp)\n\t"
	"lw $t6, 24($sp)\n\t"
	"lw $t7, 28($sp)\n\t"
	"lw $a0, 32($sp)\n\t"
	"lw $a1, 36($sp)\n\t"
	"lw $a2, 40($sp)\n\t"
	"lw $a3, 44($sp)\n\t"
	"lw $ra, 48($sp)\n\t"
	"addi $sp, $sp, 52\n"; // EPILOG

const char* load_val = ".text\n\tl%s $t%d, %s\n"; // Used for loading a word or an immediate into a temporary register t. 
const char* store_word = ".text\n\tsw %s, %s\n"; // Store a word from a register into memory

void push(Scope* scope)
{
	Scope* walker = scope; // Get the if scope
	while((walker->next) != NULL) { walker = walker->next; } // We have to push to top of stack

	Scope* new_if = calloc(1, sizeof(Scope)); // Make new if for stack
	assert(new_if);
	new_if->offset = if_ct; // Set it to the if_ct
	walker->next = new_if; // Set new top to new_if
	if_ct+=1; // Increase if_ct
}

void pop(Scope* scope) // This must only be called if there's an inserted if
{
	Scope* walker = scope; // Get if scope
	while((walker->next->next) != NULL) { walker = walker->next; }
	walker->next = NULL; // Pop top
}

int peek(Scope* scope) // Get the offset of current top
{
	Scope* walker = scope; 
	while((walker->next) != NULL) { walker = walker->next; }
	return walker->offset; // Get top's offset
}

void append_loc_scope(char* id) // Append some loc VARIABLE ID to local scope (global funcs and vars have their own definitions)
{
	Scope* walker = &local_scope; // Only care about local because when we search for a var, we will first look through the stack for loc, then def
	Scope* new_member = calloc(1, sizeof(Scope)); // New member of scope
	assert(new_member);
	new_member->name = id; // Store the ID 
	offset+=4; new_member->offset = offset; // Specify an offset for this variable so we can put it on the stack

	while((walker->next) != NULL) { walker = walker->next; }
	walker->next = new_member; // Append to scope
}

char* make_reg_or_label(char* name, int arg) // Appends a number to a passed in a string, used for registers and labels
{
	char* label = calloc(50, sizeof(char));
	assert(label);
	snprintf(label, 50, name, arg); // Arg is for the register number, name is for the register itself $t%d
	return label;
}

char* make_str_label(char* str) // Makes a label for a passed in string literal
{
	printf(".data\nstring_mips_label%d: .asciiz %s\n", str_count, str);
	char* label = make_reg_or_label("string_mips_label%d", str_count); str_count+=1;
	return label;
}

int get_offset_loc_scope(char* ID) // This is only used for local scopes
{
	Scope* walker = (&local_scope)->next; // Start from scope->next because head is sentinel node
	while(walker != NULL) { // Iterate through the linked list of IDs in current SCOPE
		if((walker->name) && strcmp(walker->name, ID) == 0) { // We check the first condition
			return walker->offset; // Because I'm scared of NULLs and segfaults
		} // For local scopes we absolutely need a sentinel node 
		// because we break off the head's next everytime we encounter a new local scope
		walker = walker->next;
	}
	return -1; // If the target ID isn't found in current scope return -1
}

char* get_ID_addr(char* val) // Get the LOCATION (by location we mean the memory address either on stack or declared statically) of some VARIABLE
{
	//printf("Ami in global? %d\n", in_global);
	if(in_global) { return val; } // If we are in global scope we don't have to go searching through no local scopes
	else {
		int my_offset = get_offset_loc_scope(val); // If we are not global, then we need to check if the variable is on the stack (local) or global
		if (my_offset == -1) { return val; } // If we get an offset -1 that means it's definitely not on the local scope (on global)
		int true_offset = offset-my_offset; // We need the true offset (Everytime we add a var to the stack we subtract 4. So in order to get the first var we need a offset-4
		return(make_reg_or_label("%d($sp)", true_offset)); // Not really making a label but somewhere that points to where our data is (global or on stack)
	}

	return NULL; // THIS SHOULD NOT FAIL BECAUSE WE ASSUME NO ERRORS
}

char* assign_register(char* val_reg_id, int imm_flag, int call_flag) // One of the most important pieces of our program: Allocating registers to provided data
{ // Immediates, variables, even call statements. If Imm flag, we load immediate. If call_flag, we load $v0 result register into temp register
	char* reg_allocated = NULL;
	for(int i=0; i<8; i++) {
		if(registers[i]!=-1) {
			if(imm_flag) { printf(load_val, "i", i, val_reg_id); } // Load an immediate into a register
			else if(call_flag) { printf(".text\n\tmove $t%d, %s\n", i, val_reg_id); } // Load a rresult from a call ($v0) into a register
			else {
				char* id_location = get_ID_addr(val_reg_id); // Get the data stored in memory
				printf(load_val, "w", i, id_location); // And put that data into a register for later use
			}
			registers[i] = -1; // We are using this register, so use -1 to indicate
			return(reg_allocated = make_reg_or_label("$t%d", i)); // Make a temporary register
		}
	}
	assert(reg_allocated != NULL); // Shouldn't fail, if so we have a problem
	return NULL;
}

void exit_func()
{ printf(".text\n\taddi $sp, $sp, %d\n\tjr $ra\n", offset); } // Make sure we clear our stack to its original state. We keep a global offset to make sure

void clear_registers() // THIS WILL BE CALLED AT THE END OF EVERY FUNCTION to reset the important fields 
{
	exit_func();
	// We also need to add the jr statement to make sure we jump back to our caller
	for(int i=0; i<8; i++)
	{
		if(registers[i]==-1) { // A register is in use if it's -1
			registers[i]=i; // Make sure there are no -1s in the registers (free them)
		}
	}
	offset = 0; // After reseting stack, reset the offset for next function
	arg_reg_ct = 0; // We use when we're storing arguments into the stack (start at a0, and continue until we have more args and push them)
	in_global = 1; // When we exit a function we return to the global scope
	local_scope.next = NULL; // Break the head to handle the next local function
}

int chartoi(char chr) { return((chr-'0')); } // Converts a char to an int

void free_reg(char* reg) // Frees a reg from use
{
	int reg_num = chartoi(reg[2]); // $ is first char, t second, num third. This is the only place we will segfault in our program
	registers[reg_num] = reg_num; // Free this register. Recall -1 means its in use
}

char* arith_compute(char* reg1, char* reg2, char* op) // Perform arithmetic operations add,mult,div etc.. and store results in reg1 provided
{ 
	printf(arith_reg, op, reg1, reg1, reg2); // Do arithmetic
	free_reg(reg2);
	return reg1;
}

void make_space_for_loc_var(char* id, int is_arg) // Used for adding local variables/parameters to the stack and printing them
{
	append_loc_scope(id); 
	if(is_arg) { printf(loc_var_arg, arg_reg_ct); } // We use this to tell which arg register to get the arg from (a0, a1, etc..)
	else { printf(loc_var_new); } // If it's not an arg, just load a 0 into the next 4 bytes of the stack
	arg_reg_ct+=1;
}

void func_entry(char* id) // Whenever we enter the function
{
	in_global = 0; // No longer in global scope
	printf(func_label, id); // Print label with ID
}

void put_else_label() // This is the end of the normal if statement. We always have ELSEs for simplicity, but if the IF
{ // does have an ELSE, then there's statements after the ELSE
	int top = peek(&if_scope); // Since we increase the if count (in case of nests), we also have to decrease it to get the original label
	printf(".text\n\tj IF_END%d\nELSE%d:\n", top, top); // If we don't go to ELSE, go to IF_END
}

void put_end_label() // For a normal if statement, END will immediately follow ELSE. But for an if-else then we need to include this later
{
	int top = peek(&if_scope); 
	printf(".text\nIF_END%d:\n", top);
	pop(&if_scope); // We do this because of nests. If there are multiple IF_ENDs, in order to get to the previous IF_END we must subtract
}

void call_loop(char* id, char* orig_reg, char* cond_reg)
{ // Makes the actual loop. Before this we just assigned the expr to the ID
	push(&for_scope); int top = peek(&for_scope);
	printf(".text\nLOOP%d:\n\t", top); // This is the start of the loop and we will continously loop to
	char* id_location = get_ID_addr(id); // We will increment and store the value back into the ID, so we need to get the changed value back
	printf("lw %s, %s\n\t", orig_reg, id_location); // We will keep reusing the orig_reg to save variables, and we don't set it to -1
	printf("sle %s, %s, %s\n\t", orig_reg, orig_reg, cond_reg); // The for loop in this language uses the <= comparison always. We also reuse orig_reg
	printf("beqz %s, LOOP_END%d\n", orig_reg, top); // If condition doesn't hold we go to LOOP_END
	free_reg(orig_reg);
}

void jump_back_to_loop(char* id, char* orig_reg, char* cond_reg)
{
	char* id_location = get_ID_addr(id); // Recall that we use the ID variable as the counter, so we have to get it an increment it
	int top = peek(&for_scope);
	printf(".text\n\tlw %s, %s\n\t", orig_reg, id_location); // We keep reusing the orig_reg and use it to hold the var
	printf("addi %s, %s, %d\n\t", orig_reg, orig_reg, 1); // Increment orig_reg by 1 for counter
	printf("sw %s, %s\n\t", orig_reg, id_location); // Store the incremented reg back to the var
	printf("j LOOP%d\n", top); // Go back to beginning of LOOP
	printf("LOOP_END%d:\n", top); // If we end up here then the loop is over
	free_reg(cond_reg);
	pop(&for_scope);
}

void insert_into_arg_reg(char* reg) // Used for call statements and inserting values from registers into the proper arg reg
{
	printf(".text\n\tmove $a%d, %s\n", call_arg_reg_ct, reg); // Move into arg reg
	free_reg(reg); // Moved into a0 and no longer need
	call_arg_reg_ct+=1;
}

void save_before_call_and_incr() // We have to make sure we adjust the offset when retreiving variables
{
	printf(save_before_call);
	offset+=52; // Because we push 52 bytes on the stack to save variables
}

void call_and_reload_and_decr(char* call) // We have to make sure we adjust the offset when retreiving variables
{
	printf(call_and_reload, call);
	offset-=52; // Because we push 52 bytes on the stack to save variables
}

void call_pint_or_pstr(char* reg, char* print_call) // Used for calling printstring or printint
{ // This function only needs 1 arg
	save_before_call_and_incr(); // Sav regs
	if(strcmp(print_call, "printint")==0) {
		printf(".text\n\tmove $a0, %s\n", reg); // We have to move the int to a0
	} else { printf(".text\n\tla $a0, %s\n", reg); } // We have to move str address to a0
	call_and_reload_and_decr(print_call); // Call and reload
	free_reg(reg);
}

void call_gint(char* id_location) // Call getint
{
	save_before_call_and_incr(); // Sav regs
	call_and_reload_and_decr("getint"); // Call and reload
	printf(".text\n\tsw $v0, %s\n", id_location); // Save into some register
}

%}

%union {
	char* val;
};

%token <val> ID NUMBER STRING LP RP
%type <val> var_dec expr term factor call_stmt rel_expr
%token ADD MINUS MUL DIV EQ LT LE GT GE ASSIGN NE
%token LET IN END VAR FUNCTION IF THEN ELSE FOR TO DO PRINTINT GETINT PRINTSTRING RETURN INT_TYPE STRING_TYPE VOID
%token COLON COMMA SEQ SEMICOLON

%left ADD MINUS
%left MUL DIV
/* MUL DIV have higher left-associative priority */
%%


program		: LET {printf(".text\n\tj toyger_start\n%s%s%s",p_i,p_str,g_i);} decs IN {printf(toyger_start);}
            statements END {printf(toyger_end);}

decs  : dec  decs
      | %empty

dec : var_dec { printf(glob_var, $1); } | function_dec 

var_dec: VAR ID COLON type {$$ = $2;}

type: INT_TYPE | STRING_TYPE | VOID

function_dec	: FUNCTION ID LP { func_entry($2); } parameters RP COLON type SEQ local_dec statements END { clear_registers(); /* Registers no longer in use */ }
  | FUNCTION ID LP RP { func_entry($2); } COLON type SEQ local_dec statements END  { clear_registers(); }

local_dec : LET var_decs IN 
  | %empty

var_decs: var_decs var_dec { make_space_for_loc_var($2, 0); }
  | %empty

parameters	: parameters COMMA parameter
    | parameter

parameter : ID COLON type { make_space_for_loc_var($1, 1); /* This is an arg, so use 1 to specify we need to get the arg from the arg register */ } 

statements	: statements SEMICOLON statement 
		| statement

statement	: assignment_stmt 
		| print_stmt 
		| input_stmt 
		| if_stmt 
		| for_stmt 
		| call_stmt 
		| return_stmt

assignment_stmt	: ID ASSIGN expr { printf(store_word, $3, get_ID_addr($1)); free_reg($3); }

return_stmt	: RETURN expr { printf(".text\n\tmove $v0, %s\n", $2);  exit_func(); free_reg($2); }
    | RETURN { exit_func(); }

rel_expr		: expr EQ expr { $$ = arith_compute($1, $3, "seq"); }
		| expr NE expr { $$ = arith_compute($1, $3, "sne"); }
		| expr LT expr { $$ = arith_compute($1, $3, "slt"); }
		| expr LE expr { $$ = arith_compute($1, $3, "sle"); }
		| expr GT expr { $$ = arith_compute($1, $3, "sgt"); }
		| expr GE expr { $$ = arith_compute($1, $3, "sge"); }

expr		: expr ADD term { $$ = arith_compute($1, $3, "add"); }
		| expr MINUS term { $$ = arith_compute($1, $3, "sub"); }
		| term { $$ = $1; }

term		: term MUL factor { $$ = arith_compute($1, $3, "mul"); }
		| term DIV factor { $$ = arith_compute($1, $3, "div"); }
		| factor { $$ = $1; }

factor		: LP expr RP { $$ = $2; }
		| NUMBER { $$ = assign_register($1, 1, 0); }
		| STRING { $$ = make_str_label($1); }
		| ID { $$ = assign_register($1, 0, 0); }
		| call_stmt { $$ = assign_register("$v0", 0, 1); }

print_stmt	: PRINTINT LP expr RP { call_pint_or_pstr($3, "printint"); }
  | PRINTSTRING LP expr RP { call_pint_or_pstr($3, "printstring"); }

input_stmt	: ID ASSIGN GETINT LP RP { call_gint(get_ID_addr($1)); }

call_stmt	: ID LP { save_before_call_and_incr(); } RP { call_and_reload_and_decr($1); /* call and reload/restore regs */ }
		    | ID LP { save_before_call_and_incr(); } expr_list { call_arg_reg_ct = 0; } RP { call_and_reload_and_decr($1); }

if_stmt	: IF LP rel_expr { push(&if_scope); int top = peek(&if_scope); printf(".text\nIF%d:\n\tbeqz %s, ELSE%d\n", top, $3, top); free_reg($3); 
		/* It's better to always use ELSE or else we would have to do some gross hack*/ } RP THEN if_stmt1

if_stmt1 : statements END { put_else_label(); put_end_label(); }
         | statements ELSE { put_else_label(); } statements END { put_end_label(); }

for_stmt	: FOR ID ASSIGN expr TO expr DO { printf(store_word, $4, get_ID_addr($2)); /* We first assign the expr to the ID */
			call_loop($2, $4, $6); /* we need ID for increments */ } statements END  { jump_back_to_loop($2, $4, $6); }

expr_list	: expr_list COMMA expr { insert_into_arg_reg($3); /* 3 is our reg that holds the value to be inserted into the arg reg */ }
		| expr { insert_into_arg_reg($1); }

%%

int main(){
    yyparse();
    fprintf(stderr, pass_msg); // If we successfully parse all tokens then PASS
    return 0;
}

int yyerror(){ // If we encounter an error that is NOT a LEX error NOR one of the above
   if(!lex_err_flag) { fprintf(stderr, syn_err_msg, yylineno); } // Then SYNTAX ERR
   exit(-1);
}

