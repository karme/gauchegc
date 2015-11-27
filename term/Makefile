SITELIBDIR=`gauche-config --sitelibdir`/ggc/term

SCMFILES= with-raw-mode.scm without-echo.scm vt100.scm

install :
	gauche-install -T $(SITELIBDIR) -m 444 $(SCMFILES)
