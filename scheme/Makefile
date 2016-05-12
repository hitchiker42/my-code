libguile-openssl.so: openssl_guile.c
	gcc `pkg-config --libs --cflags openssl guile-2.0`\
	  -shared -O2 -g -fPIC -o $@ $^
libguile-openssl: libguile-openssl.so

openssl_test: openssl_test.c
	gcc `pkg-config --libs --cflags openssl` -O2 -g -o $@ $^
