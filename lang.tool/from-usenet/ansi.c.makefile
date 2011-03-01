all:
	zcat ansi.c.grammar.Z | sed -n '/^#!/,$$p' | sh
	zcat ansi.c.grammar.Z | sed '/^#!/,/exit/d' > Article
clean:
	rm -f Makefile README gram.y main.c scan.l y.tab.h Article



