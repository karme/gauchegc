SCM_INSTALL_DIR=`gauche-config --sitelibdir`/ggc/term

SCMFILES= with-raw-mode.scm without-echo.scm

INSTALL=../install-sh -c

install :
	chmod +x somepoint
	mkdir -p $(SCM_INSTALL_DIR)
	for f in $(SCMFILES) _end; do \
	  if test $$f != _end; then \
	    $(INSTALL) -m 444 $$f $(SCM_INSTALL_DIR); \
	  fi; \
	done
