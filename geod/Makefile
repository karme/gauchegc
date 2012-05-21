CPPFLAGS+=-Igeographiclib-1.6/include -fPIC -DPIC -O2 -g -Wall
# -pedantic

all: libgeodwrapper.so
	./test.scm

libgeodwrapper.so: geodwrapper.o
	$(CXX) -shared -g -o $@ $< -Lgeographiclib-1.6/src/.libs -lGeographic

clean:
	rm -v *.so *.o *~

