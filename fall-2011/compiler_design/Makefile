all: 

# mid term program
midterm : midterm.c
	gcc $< -lfl -o $@ 

midterm.c : mid-term-2010.l
	flex -o $@ $<

# program 1. 
prog1 : prog1.c
	gcc $< -lfl -o $@ 

prog1.c : fb1-1.l 
	flex -o $@ $<

# program 2.
prog2 : prog2.c fb1-3.tab.c 
	gcc $^ -o $@ -lfl

prog2.c : fb1-3.l
	flex -o $@ $<

fb1-3.tab.c : fb1-3.y
	bison -d $< 

clean: 
	rm -rf *~ $(EXE) lex.yy.c fb1-3.tab.c fb1-3.tab.h