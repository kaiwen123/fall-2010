CPP = /usr/bin/g++
CPPFLAGS = -O2 -fPIC -ggdb
LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXE1 = proj2
EXE2 = proj2.vector
DOX = /usr/bin/doxygen
DOXCONF = Dox.txt

SRC1 = LinkedSortedList.cpp Employee.cpp main.cpp 
OBJ1 = $(SRC1:.cpp=.o)

SRC2 = Employee.cpp SortedVector.cpp main.vector.cpp
OBJ2 = $(SRC2:.cpp=.o)

OBJS = $(OBJ1) $(OBJ2)
EXES = $(EXE1) $(EXE2)

all: $(EXE1) $(EXE2)

.SUFFIX: .o .cpp

$(EXE1): $(OBJ1)
	$(CPP) $(LDFLAGS) $^ -o $@

$(EXE2): $(OBJ2)
	$(CPP) $(LDFLAGS) $^ -o $@

.o: .cpp
	@$(CPP) -c $(CPPFLAGS) $< -o $@

doc: $(DOXCONF) $(SRC)
	@$(DOX) $<

clean:
	@$(RM) $(OBJS) $(EXES)
