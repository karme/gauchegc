SCMFILES= eps2x.scm
COMMANDS= eps2x eps2tifflzw eps2tiff24nc eps2png eps2png256

DESTDIR=`gauche-config --sitelibdir`/ggc/img
EXECDIR=$(HOME)/bin/ggc

install :
	gauche-install -T $(DESTDIR) -m 444 $(SCMFILES)
	gauche-install -T $(EXECDIR) -m 755 $(COMMANDS)

clean:
	rm -f *.png *.tiff
