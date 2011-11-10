
ASSN = 3
CLASS= cs780
CLASSDIR= /nfs/ecsnas1/users/cs780/cs780pub
COOLDIR= /usr/local/lib/cool
LIB= -L/usr/sww/lib -lfl
AR= gar
ARCHIVE_NEW= -cr
RANLIB= gar -qs

SRC= cool.y cool-tree.handcode.h good.cl bad.cl README
CSRC= parser-phase.cc utilities.cc stringtab.cc dumptype.cc \
      tree.cc cool-tree.cc tokens-lex.cc  handle_flags.cc 
TSRC= myparser mycoolc cool-tree.aps
CGEN= cool-parse.cc
HGEN= cool-parse.h
LIBS= lexer semant cgen
CFIL= ${CSRC} ${CGEN}
HFIL= cool-tree.h cool-tree.handcode.h 
LSRC= Makefile
OBJS= ${CFIL:.cc=.o}
OUTPUT= good.output bad.output


CPPINCLUDE= -I. -I${COOLDIR}/include/PA${ASSN} -I${COOLDIR}/src/PA${ASSN}

BFLAGS = -d -v -y -b cool --debug -p cool_yy

CC=g++ -Wno-deprecated
CFLAGS=-g -Wall -Wno-unused -Wno-deprecated -fpermissive -DDEBUG ${CPPINCLUDE}
FLEX=flex ${FFLAGS}
BISON= bison128 ${BFLAGS}
DEPEND = ${CC} -MM ${CPPINCLUDE}

source: ${SRC} ${TSRC} ${LIBS} lsource

lsource: ${LSRC}

#------------------------------------------------------------------------
# You may execute turnin as often as you like, files of the same name will 
# be overwritten with the latest submission, submission time will be 
# determined by the latest submission.
#
turnin : ${SRC} ${OUTPUT} turnin-clean
	${CLASSDIR}/turnin/turnin-pa${ASSN} ${SRC} ${OUTPUT}

turnin-clean:
	@rm -f ${OBJS}

#turnin-clean:
#	@rm -f lexer ${OBJS}
#------------------------------------------------------------------------

# for bash
${OUTPUT}:	parser good.cl bad.cl
#	@rm -f ${OUTPUT}
	./myparser good.cl >good.output 2>&1 
	./myparser bad.cl >bad.output 2>&1 

# for tcsh
${OUTPUT}:	parser good.cl bad.cl
#	@rm -f ${OUTPUT}
#	./myparser good.cl >& good.output
#	./myparser bad.cl >& bad.output 

parser: ${OBJS}
	${CC} ${CFLAGS} ${OBJS} ${LIB} -o parser

.cc.o:
	${CC} ${CFLAGS} -c $<

cool-parse.cc cool-parse.h: cool.y
	${BISON} ${BFLAGS} cool.y
	mv -f cool.tab.c cool-parse.cc

dotest:	parser good.cl bad.cl
	@echo "\nRunning parser on good.cl\n"
	-./myparser good.cl 
	@echo "\nRunning parser on bad.cl\n"
	-./myparser bad.cl

${LIBS}:
	${COOLDIR}/etc/link-object ${ASSN} $@

# These dependencies allow you to get the starting files for
# the assignment.  They will not overwrite a file you already have.

${SRC} :								
	-${COOLDIR}/etc/copy-skel ${ASSN} ${SRC}

${LSRC} :
	-${COOLDIR}/etc/link-shared ${ASSN} ${LSRC}

${TSRC} ${CSRC}:
	-ln -s ${COOLDIR}/src/PA${ASSN}/$@ $@

${HSRC}:
	-ln -s ${COOLDIR}/include/PA${ASSN}/$@ $@

submit-clean: ${OUTPUT}
	-rm -f *.s core ${OBJS} ${CGEN} ${HGEN}  *~  

clean :
	-rm -f ${OUTPUT} *.s core ${OBJS} ${CGEN} ${HGEN} *~ *.a *.o 

#clean :
#	-rm -f ${OUTPUT} *.s core ${OBJS} ${CGEN} ${HGEN} lexer parser cgen semant *~ *.a *.o 

clean-compile:
	@-rm -f core ${OBJS} ${CGEN} ${HGEN} ${LSRC}

%.d: %.cc ${LSRC}
	${SHELL} -ec '${DEPEND} $< | sed '\''s/\($*\.o\)[ :]*/\1 $@ : /g'\'' > $@'

-include ${CFIL:.cc=.d}
