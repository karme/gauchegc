all: c-parse.yy.scm c-gram.yy.scm

c-parse.yy.scm : c-parse.scm
	@echo "	Generating c-parse.yy.scm will take while,"
	@echo "	I just toch c-parse.yy.scm."
	@echo ""
	touch c-parse.yy.scm
	@echo ""
	@echo "	If you made actual change in c-parse.scm, type,"
	@echo ""
	@echo "gosh -I. -ulalr c-parse.scm"
	@echo ""
	@echo "	And be patient."

c-gram.yy.scm : c-gram.scm
	gosh -I. -ulalr c-gram.scm


run-scan: c-gram.yy.scm
	sed '/^#include/d' c/hello.c | cpp -P | gosh -I. c-scan.scm

run-lex:
	sed '/^#include/d' c/hello.c | cpp -P | gosh -I. c-lex.scm

run-decl:
	gcc -E /usr/local/include/aquaterm/aquaterm.h | gosh -I. c-decl.scm

clean:
	rm -f c-gram.yy.scm *.out *.log
