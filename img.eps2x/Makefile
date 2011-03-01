#  $Id: Makefile,v 1.3 2002/10/22 04:22:00 skimu Exp $
#
SCM_INSTALL_DIR=`gauche-config --sitelibdir`/ggc/img
SCMFILES=eps2x.scm
COMMANDS=eps2x eps2tifflzw eps2tiff24nc eps2png eps2png256

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

install-cmds:
	for f in $(COMMANDS) _end; do                  \
	  if test $$f != _end; then                    \
	     $(INSTALL) -m 755 $$f $$HOME/bin/scripts; \
	  fi;                                          \
	done

clean:
	rm -f *.png *.tiff


