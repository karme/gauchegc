INSTALL_DIR=`gauche-config --sitelibdir`/ggc/term

SCMFILES= with-raw-mode.scm without-echo.scm vt100.scm

install :
	gauche-install -T $(INSTALL_DIR) -m 444 $(SCMFILES)
