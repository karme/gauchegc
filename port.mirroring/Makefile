SCM_INSTALL_DIR=`gauche-config --sitelibdir`/ggc/port

SCMFILES= mirroring.scm
COMMANDS= gosh-pseudo-session

INSTALL=../install-sh -c

all : manual.txt example.txt

manual.txt  : manual.scm
example.txt : example.scm

install :
	for f in $(COMMANDS) _end; do \
	  if test $$f != _end; then   \
	     chmod +x $$f;            \
	  fi;                         \
	done
	mkdir -p $(SCM_INSTALL_DIR)
	for f in $(SCMFILES) _end; do \
	  if test $$f != _end; then \
	    $(INSTALL) -m 444 $$f $(SCM_INSTALL_DIR); \
	  fi; \
	done

clean :
	rm -f example*.txt manual.txt

.SUFFIXES: .scm .txt
.scm.txt :
	gosh -I../.. gosh-pseudo-session $<  > $@

