CFLAGS=-g -Og -Wall -Wno-unused-variable -Wno-unused-function -std=c99 -ly -lfl
CC=gcc
NAME=TigerSubsetCompiler

$(NAME):	$(NAME).lex.yy.c $(NAME).tab.c 
	$(CC) -o $@ $(NAME).lex.yy.c $(NAME).tab.c  $(CFLAGS)

$(NAME).lex.yy.c:	$(NAME).l
	flex -o $@ $(NAME).l

$(NAME).tab.c:	$(NAME).y
	bison -o $(NAME).tab.c -dv $(NAME).y

clean:	
	rm -f $(NAME) $(NAME).lex.yy.c $(NAME).tab.* $(NAME).output asm_output/*

