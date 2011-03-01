OBJS=c.o c-parse.tab.o c-lex.o 

c : $(OBJS)
	cc -o $@ $(OBJS)

c-parse.tab.h c-parse.tab.c : c-parse.y
	bison -d c-parse.y

c-lex.o : c-lex.l
	flex c-lex.l
	cc -c -o c-lex.o lex.yy.c
	rm lex.yy.c

c-parse.y : c-parse.in
	@echo "Generating c-parse.y"
	sh c.sh

clean :
	rm -f c	c-parse.tab.* c-parse.y c-lex.o c.o c *.tmp *.out \
                c-parse.scm-for-edit
run :
	sed '/^#include/d' c.c | ./c

