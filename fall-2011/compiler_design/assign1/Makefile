
ASSN = 1
CLASS= cs780
CLASSDIR= /nfs/ecsnas1/users/cs780/cs780pub
COOLDIR= /usr/local/lib/cool
SRC= type.cl README 
LSRC= Makefile type_eg.s type.test

CC=gcc
CFLAGS=-g

.c.o:
	${CC} ${CFLAGS} -c $<

source :	lsource
	${COOLDIR}/etc/copy-skel ${ASSN} ${SRC}

lsource:
	${COOLDIR}/etc/link-shared ${ASSN} ${LSRC}

turnin: ${SRC}
	${CLASSDIR}/turnin/turnin-pa${ASSN} ${SRC}

compile: type.s

type.s: type.cl
	${COOLDIR}/bin/coolc type.cl 

test:	compile
	@echo type.test
	${COOLDIR}/bin/spim -file type.s < type.test

clean:
	rm -f *.s core *~
