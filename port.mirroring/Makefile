SCMFILES= mirroring.scm
COMMANDS= gosh-pseudo-session

DESTDIR=`gauche-config --sitelibdir`/ggc/port

all : manual.txt example.txt

manual.txt  : manual.scm
example.txt : example.scm

check :
	gosh -l./mirroring.scm test.scm > test.log

install :
	gauche-install -T $(DESTDIR) -m 444 $(SCMFILES)

clean :
	rm -f example.txt manual.txt

.SUFFIXES: .scm .txt
.scm.txt :
	gosh -l./mirroring.scm  gosh-pseudo-session $<  > $@

