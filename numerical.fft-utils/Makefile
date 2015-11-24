SCMFILES=fft-utils.scm
COMMANDS=

DESTDIR=`gauche-config --sitelibdir`/ggc/numerical

install :
	gauche-install -T $(DESTDIR) -m 444 $(SCMFILES)

test:

clean:
	rm -f fo.* *.scm.*~ Makefile.*~ *.log *.png
