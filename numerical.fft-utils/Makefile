SCMFILES=fft-utils.scm
COMMANDS=

DESTDIR=`gauche-config --sitelibdir`/ggc/numerical

install :
	gauche-install -T $(DESTDIR) -m 444 $(SCMFILES)

test:
	@rm -f test.log
	gosh -I. -l fft-utils.scm test.scm  > test.log
clean:
	rm -f fo.* *.scm.*~ Makefile.*~ *.log *.png
