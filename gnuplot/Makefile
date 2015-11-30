SITELIBDIR=`gauche-config --sitelibdir`

SCMFILES= gnuplot.scm

CFLAGS=-O

all : plot_glut app

plot_glut : plot_glut.o
	$(CC) $(CFLAGS) -o plot_glut -framework GLUT -framework OpenGL plot_glut.c

clean :
	rm -f *.o *.gp plot_glut Gplt.app/Contents/MacOS/Gplt

app : Gplt.app/Contents/MacOS/Gplt

Gplt.app/Contents/MacOS/Gplt: plot_glut
	mkdir -p Gplt.app/Contents/MacOS
	cp plot_glut Gplt.app/Contents/MacOS/Gplt

install :
	gauche-install -T $(SITELIBDIR)/ggc/skimu -m 444 $(SCMFILES)

install-mac : install install-helper install-app
install-all : install install-helper

install-app : app
	cp -r Gplt.app $(HOME)/Applications

install-helper : plot_glut comm_plot_glut
	chmod +x comm_plot_glut
	cp plot_glut comm_plot_glut /usr/local/bin

test-mac : app
	open Gplt.app
	gosh -I. -lgnuplot.scm -l../lwp/lwp.scm example.scm

