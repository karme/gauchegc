# $Id: Makefile,v 1.9 2002/10/15 03:05:54 skimu Exp $

SCM_INSTALL_DIR=`gauche-config --sitelibdir`/ggc/skimu
SCMFILES= gnuplot.scm
CFLAGS=-O
all : plot_glut

INSTALL=../install-sh -c

plot_glut : plot_glut.o
	$(CC) $(CFLAGS) -o plot_glut -framework GLUT -flat_namespace plot_glut.c

clean :
	rm -f *.o *.gp plot_glut Gplt.app/Contents/MacOS/Gplt

app: Gplt.app/Contents/MacOS/Gplt

Gplt.app/Contents/MacOS/Gplt: plot_glut
	mkdir -p Gplt.app/Contents/MacOS
	cp plot_glut Gplt.app/Contents/MacOS/Gplt

install-mac : install install-helper install-app
install-all : install install-helper

install :
	mkdir -p $(SCM_INSTALL_DIR)
	for f in $(SCMFILES) _end; do \
	  if test $$f != _end; then \
	    $(INSTALL) -m 444 $$f $(SCM_INSTALL_DIR); \
	  fi; \
	done


install-app: app
	cp -r Gplt.app $(HOME)/Applications

install-helper: plot_glut comm_plot_glut
	chmod +x comm_plot_glut
	cp plot_glut comm_plot_glut /usr/local/bin
test:
	gosh -I. example.scm

