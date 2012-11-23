srcdir=.
#CFLAGS=-pg -ggdb -O0
CFLAGS=-ggdb -O0

# gprof.out
all: profile-summary.out

profile.log: test.scm
	./test.scm -vv  >/dev/null 2> $@

profile-summary.out: profile.log
	< $< $(srcdir)/profile-summary.scm|tee $@

%.txt : %.ge
	graph-easy --as boxart $< > $@

%.dot : %.ge
	graph-easy --as dot $< > $@

%.svg : %.dot
	dot $< -Tsvg -o $@

clean:
	rm -vf profile-summary.out ctest

gmon.out: ctest
	./ctest

gprof.out: gmon.out
	gprof ./ctest > $@
	cat $@

callgrind.out: ctest
	valgrind --tool=callgrind --callgrind-out-file=$@ ./ctest
	callgrind_annotate --inclusive=yes --tree=both $@
#	callgrind_annotate --inclusive=yes --tree=caller callgrind.out
#	kcachegrind ./ctest $@
