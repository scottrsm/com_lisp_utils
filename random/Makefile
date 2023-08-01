# FILE IDENTIFICATION
#
#  Name:         Makefile
#  Purpose:      Makefile for creating library to load with UFFI
#  Author:       Kevin M. Rosenberg
#  Date Started: Aug 2003
#
#  CVS Id:   $Id: Makefile,v 1.3 2003/09/10 22:19:25 scottrsm Exp $
#

base=random
source=$(base).c
object=$(base).o
shared_lib=lib$(base).so

.PHONY: all
all: 
	@echo select linux, mac, or solaris


linux: $(source) Makefile
	gcc -fPIC -DPIC -c $(source) -o $(object)
	gcc -shared $(object) -o $(shared_lib)
	#gcc -shared -Wl,-soname,uffi-c-test-lib $(object) -o $(shared_lib)
	rm $(object)

mac:
	cc -dynamic -c $(source) -o $(object)
	ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress -o $(base).dylib $(object)
	ld -bundle /usr/lib/bundle1.o -flat_namespace -undefined suppress /usr/lib/libz.dylib -o z.dylib

solaris:
	cc -KPIC -c $(source) -o $(object)
	cc -G $(object) -o $(shared_lib)

aix-acl:
	gcc -c -D_BSD -D_NO_PROTO -D_NONSTD_TYPES -D_MBI=void $(source)
	make_shared -o $(shared_lib) $(object)

clean:
	rm -f $(object)

.PHONY: distclean
distclean: clean
	rm -f $(shared_lib)



