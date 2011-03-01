INSTALL_TYPE = site
GOSH = gosh
INSTALL = install -c
GAUCHE_CONFIG=gauche-config

CFLAGS = `$(GAUCHE_CONFIG) --so-cflags` `$(GAUCHE_CONFIG) -I` -DCLOCK_DEBUG
LDFLAGS = `$(GAUCHE_CONFIG) --so-ldflags`
LIBDIR= `$(GAUCHE_CONFIG) -L` 
LIBS=`$(GAUCHE_CONFIG) --so-libs`
HEADER_INSTALL_DIR  = `$(GAUCHE_CONFIG) --$(INSTALL_TYPE)incdir`
SCM_INSTALL_DIR     = `$(GAUCHE_CONFIG) --$(INSTALL_TYPE)libdir`/ggc
ARCH_INSTALL_DIR    = `$(GAUCHE_CONFIG) --$(INSTALL_TYPE)archdir`

ARCHFILES = ggcclock.so
SCMFILES = clock.scm
HEADERS = 

TARGET = ggcclock.so
OBJS =  clock.o

all : $(TARGET)

$(TARGET) : $(OBJS)
	$(CC) $(LDFLAGS) $(TARGET) $(OBJS) $(LIBDIR) $(LIBS)

test : all
	@rm -f test.log
	$(GOSH) test.scm > test.log

install : all
	mkdir -p $(HEADER_INSTALL_DIR)
	@for f in $(HEADERS) _end; do \
	  if test $$f != _end; then \
	    $(INSTALL) -m 444 $$f $(HEADER_INSTALL_DIR); \
	  fi; \
	done
	mkdir -p $(SCM_INSTALL_DIR)
	@for f in $(SCMFILES) _end; do \
	  if test $$f != _end; then \
	    $(INSTALL) -m 444 $$f $(SCM_INSTALL_DIR); \
	  fi; \
	done
	mkdir -p $(ARCH_INSTALL_DIR)
	@for f in $(ARCHFILES) _end; do \
	  if test $$f != _end; then \
	    $(INSTALL) -m 555 $$f $(ARCH_INSTALL_DIR); \
	  fi; \
	done

clean :
	rm -rf core $(TARGET) $(OBJS) *~ test.log so_locations

distclean : clean
	rm -rf $(CONFIG_GENERATED)

realclean : clean
	rm -rf $(CONFIG_GENERATED) configure




