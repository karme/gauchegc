GGC_ROOT=`gauche-config --sitelibdir`/ggc

SCMFILES1= util.scm
SCMFILES2= circular-list.scm

install :
	gauche-install -T $(GGC_ROOT) -m 444 $(SCMFILES1)
	gauche-install -T $(GGC_ROOT)/util -m 444 $(SCMFILES2)

clean:
	rm -f *~
