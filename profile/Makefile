srcdir=.

all: profile-summary.svg profile-summary.txt
	see profile-summary.svg
	cat profile-summary.txt

profile-data.scm: test.scm
	$(srcdir)/$<  -vv 2> $@

profile-summary.ge: profile-data.scm
	$(srcdir)/profile-summary.scm < $< $@

%.txt : %.ge
	graph-easy --as boxart $< > $@

%.dot : %.ge
	graph-easy --as dot $< > $@

%.svg : %.dot
	dot $< -Tsvg -o $@

clean:
	rm -vf profile-data.scm
