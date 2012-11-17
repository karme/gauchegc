all: profile-summary

srcdir=.

profile-data.scm: test.scm
	$(srcdir)/$<  -vv 2> $@

profile-summary: profile-data.scm
	$(srcdir)/profile-summary.scm < $< > $@
	cat $@

clean:
	rm -vf profile-data.scm
