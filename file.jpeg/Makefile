#  $Id: Makefile,v 1.2 2004/08/20 00:20:46 skimu Exp $
#
SCM_INSTALL_DIR=`gauche-config --sitelibdir`/ggc/file
SCMFILES=jpeg.scm

COMMANDS=jpeg-markers

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


