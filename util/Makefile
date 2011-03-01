SCM_INSTALL_DIR=`gauche-config --sitelibdir`/ggc

SCMFILES= util.scm
SCMFILES1= circular-list.scm

INSTALL=../install-sh -c

install :
	mkdir -p $(SCM_INSTALL_DIR)
	for f in $(SCMFILES) _end; do \
	  if test $$f != _end; then \
	    $(INSTALL) -m 444 $$f $(SCM_INSTALL_DIR); \
	  fi; \
	done
	mkdir -p $(SCM_INSTALL_DIR)/util
	for f in $(SCMFILES1) _end; do \
	  if test $$f != _end; then \
	    $(INSTALL) -m 444 $$f $(SCM_INSTALL_DIR)/util; \
	  fi; \
	done
