
ASSN = 2
CLASS= cs780
CLASSDIR= /nfs/ecsnas1/users/cs780/cs780pub
COOLDIR= /usr/local/lib/cool
LIB= -L/usr/sww/lib -lfl

SRC= cool.flex test.cl README 
CSRC= lextest.cc utilities.cc stringtab.cc handle_flags.cc
TSRC= mycoolc
HSRC= 
CGEN= cool-lex.cc
HGEN=
LIBS= parser semant cgen
CFIL= ${CSRC} ${CGEN}
LSRC= Makefile
OBJS= ${CFIL:.cc=.o}
OUTPUT= test.output

CPPINCLUDE= -I. -I${COOLDIR}/include/PA${ASSN} -I${COOLDIR}/src/PA${ASSN}


FFLAGS= -d -ocool-lex.cc

CC=g++ -fpermissive
CFLAGS= -g -Wall -Wno-unused -Wno-deprecated ${CPPINCLUDE}
FLEX=flex ${FFLAGS}
DEPEND = ${CC} -MM ${CPPINCLUDE}


source : ${SRC} ${TSRC} ${LSRC} ${LIBS} lsource

lsource: ${LSRC}

#------------------------------------------------------------------------
# You may execute turnin as often as you like, files of the same name will 
# be overwritten with the latest submission, submission time will be 
# determined by the latest submission.
#
turnin : ${SRC} ${OUTPUT} turnin-clean
	${CLASSDIR}/turnin/turnin-pa${ASSN} ${SRC} ${OUTPUT}

turnin-clean:
	@rm -f lexer ${OBJS}
#------------------------------------------------------------------------

${OUTPUT}:	lexer test.cl
	@rm -f test.output
	-./lexer test.cl >test.output 2>&1 

lexer: ${OBJS}
	${CC} ${CFLAGS} ${OBJS} ${LIB} -o lexer

.cc.o:
	${CC} ${CFLAGS} -c $<

cool-lex.cc: cool.flex 
	${FLEX} cool.flex

dotest:	lexer test.cl
	./lexer test.cl

${LIBS}:
	${COOLDIR}/etc/link-object ${ASSN} $@

# These dependencies allow you to get the starting files for
# the assignment.  They will not overwrite a file you already have.

${SRC} :								
	${COOLDIR}/etc/copy-skel ${ASSN} ${SRC}

${LSRC} :
	${COOLDIR}/etc/link-shared ${ASSN} ${LSRC}

${TSRC} ${CSRC}:
	-ln -s ${COOLDIR}/src/PA${ASSN}/$@ $@

${HSRC}:
	-ln -s ${COOLDIR}/include/PA${ASSN}/$@ $@

submit-clean: ${OUTPUT}
	-rm -f *.s core ${OBJS} lexer cool-lex.cc *~ parser cgen semant

clean :
	-rm -f ${OUTPUT} *.s core ${OBJS} lexer cool-lex.cc *~ 
#	-rm -f ${OUTPUT} *.s core ${OBJS} lexer cool-lex.cc *~ parser cgen semant

clean-compile:
	@-rm -f core ${OBJS} cool-lex.cc ${LSRC}

%.d: %.cc ${SRC} ${LSRC}
	${SHELL} -ec '${DEPEND} $< | sed '\''s/\($*\.o\)[ :]*/\1 $@ : /g'\'' > $@'

-include ${CFIL:.cc=.d}


