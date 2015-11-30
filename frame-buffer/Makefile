SITELIBDIR=`gauche-config --sitelibdir`
SCMFILES= frame-buffer.scm plot.scm

install :
	gauche-install -T $(SITELIBDIR)/ggc/skimu -m 444 $(SCMFILES)

sicp :
	gosh -umath.const -l../gnuplot/pict-core.scm fractal.scm
	gosh -umath.const -l../gnuplot/pict-core.scm parallel.scm

grad :
	gosh makegauss.scm 200 200 gauss.jpg
	gosh gradimage.scm gauss.jpg gaussd.jpg

clean :
	rm -f *.png *.jpg

