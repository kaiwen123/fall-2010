CPP = /usr/bin/g++
MACROS += -DDEBUG_LIST_INSERT
CPPFLAGS = -O2 -fPIC -ggdb $(MACROS)
LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXE = proj3
DOX = /usr/bin/doxygen
DOXCONF = Dox.txt

SRC = Database.cpp BSTree.cpp LinkedSortedList.cpp Employee.cpp lab3.cpp 
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
