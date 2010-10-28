CPP = /usr/bin/g++
#MACROS += -DDEBUG_LIST_INSERT
#MACROS += -DDEBUG_INDEX_SEARCH
#MACROS += -DDEBUG_LIST_DELETE
#MACROS += -DDEBUG_DELETE_TREE
#MACROS += -DDEBUG_DELETE_BYEID
CPPFLAGS = $(MACROS)

LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXE = proj3
DOX = /usr/bin/doxygen
DOXCONF = Dox.txt

SRC = Database.cpp bst.cpp LinkedSortedList.cpp Employee.cpp lab3.cpp 
OBJ = $(SRC:.cpp=.o)

all: $(EXE)

.SUFFIX: .o .cpp

$(EXE): $(OBJ)
	$(CPP) $(LDFLAGS) $^ -o $@

.o: .cpp
	@$(CPP) -c $(CPPFLAGS) $< -o $@

doc: $(DOXCONF) $(SRC)
	@$(DOX) $<

clean:
	@$(RM) $(OBJ) $(EXE)
