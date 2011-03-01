#  $Id: Makefile,v 1.4 2002/09/04 01:55:16 skimu Exp $
#
SCM_INSTALL_DIR=`gauche-config --sitelibdir`/ggc/skimu
SCMFILES= frame-buffer.scm plot.scm


INSTALL=../install-sh -c

install :
	chmod +x gpsmap.scm
	chmod +x gradimage.scm
	chmod +x makegauss.scm
	mkdir -p $(SCM_INSTALL_DIR)
	for f in $(SCMFILES) _end; do \
	  if test $$f != _end; then \
	    $(INSTALL) -m 444 $$f $(SCM_INSTALL_DIR); \
	  fi; \
	done

## post install test
test:
	./makegauss.scm 200 200 gauss.jpg
	./gradimage.scm gauss.jpg gaussd.jpg
clean:
	rm -f *.png *.jpg

