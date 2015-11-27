SITELIBDIR=`gauche-config --sitelibdir`

SCMFILES1= util.scm
SCMFILES2= circular-list.scm

install :
	gauche-install -T $(SITELIBDIR)/ggc      -m 444 $(SCMFILES1)
	gauche-install -T $(SITELIBDIR)/ggc/util -m 444 $(SCMFILES2)

clean:
	rm -f *~
