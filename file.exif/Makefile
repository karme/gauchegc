#  $Id: Makefile,v 1.3 2004/08/20 04:22:02 skimu Exp $
#
SCM_INSTALL_DIR=`gauche-config --sitelibdir`/ggc/file

SCMFILES=exif.scm exif-data.scm \
	 exif-fujifilm.scm exif-nikon.scm

COMMANDS=extract-thumbnail exif-simple-dump exif-index mkexifindex

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

clean:
	rm -f *.JPG *.jpg fo.log

clean-clean:
	@if (cvs update | grep '^M') > /dev/null 2>&1;\
        then                                          \
	   echo "You should cvs commit first";        \
	else                                          \
	   make clean;                                \
	   rm -f *~;                                  \
	   echo "clean clean clean";                  \
	fi

test:
	for jpg in d/*.JPG d/*.jpg;              \
	do                                       \
	  echo === $$jpg === 1>&2;               \
	  echo --- exif-test.scm --- 1>&2;       \
	  gosh exif-test.scm $$jpg;              \
	  echo --- exif-test.scm dump --- 1>&2;  \
	  gosh exif-test.scm $$jpg dump;         \
	  echo --- exif-simple-dump --- 1>&2;    \
	  ./exif-simple-dump $$jpg;              \
	  echo --- extract-thumbnail -- 1>&2;    \
	  if ./extract-thumbnail $$jpg > fo.jpg; \
	  then                                   \
	     mv fo.jpg `basename $$jpg`;         \
	  else                                   \
	     rm fo.jpg;                          \
	  fi;                                    \
	done


