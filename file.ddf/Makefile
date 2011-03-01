SCM_INSTALL_DIR=`gauche-config --sitelibdir`/ggc/file

SCMFILES= ddf.scm ddf-fread.scm ddf-sdts-dlg.scm

COMMANDS=ddfdump dlg2png ddfprint

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
	gosh test-ddf-sdts.scm palo_alto > test-dir.log
	gosh test-ddf-sdts.scm s/1554105.RD.sdts.tar.gz > test-tgz1.log
	gosh test-ddf-sdts.scm s/1554107.RR.sdts.tar.gz > test-tgz2.log
	gosh test-ddf-sdts.scm s/1542155.HP.sdts.tar.gz > test-tgz3.log
	gosh test-ddf-sdts.scm s/1542153.HP.sdts.tar.gz > test-tgz4.log

clean:
	rm -f fo.* *.scm.*~ Makefile.*~ *.log *.png
