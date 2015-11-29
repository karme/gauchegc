SITELIBDIR=`gauche-config --sitelibdir`

SCMFILES= lwp.scm

install:
	gauche-install -T $(SITELIBDIR)/ggc -m 444 $(SCMFILES)


check:
	gosh -l./lwp.scm example.scm

clean:
	rm -f *~
