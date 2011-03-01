#  $Id: Makefile,v 1.16 2004/08/20 00:19:29 skimu Exp $
#
SCM_INSTALL_DIR=`gauche-config --sitelibdir`/ggc/file
SCMFILES=util.scm qstat.scm spice.scm

COMMANDS=pun2list pun2digit pundat

INSTALL=../install-sh -c

install :
	for f in $(COMMANDS) _end; do \
	  if test $$f != _end; then   \
	     chmod +x $$f;            \
	  fi;                         \
	done
	mkdir -p $(SCM_INSTALL_DIR)
	for f in $(SCMFILES) _end; do                 \
	  if test $$f != _end; then                   \
	    $(INSTALL) -m 444 $$f $(SCM_INSTALL_DIR); \
	  fi;                                         \
	done

clean:
	@echo "Nothing to do"


