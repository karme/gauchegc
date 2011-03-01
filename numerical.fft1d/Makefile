SCM_INSTALL_DIR=`gauche-config --sitelibdir`/ggc/numerical

SCMFILES=fft1d.scm

COMMANDS=

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

test:
	for i in 16 32 64 128 256 512 1024; do  \
		echo -n $$i " ";       \
		gosh test.scm $$i; \
	done

clean:
	rm -f fo.* *.scm.*~ Makefile.*~ *.log *.png
