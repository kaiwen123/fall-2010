CPP = /usr/bin/g++
CPPFLAGS = -O2 -fPIC -ggdb
LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXECUTABLE = proj1
DOX = /usr/bin/doxygen
DOXCONF = Dox.txt

SRC = LinkedSortedList.cpp main.cpp
HDRS = LinkedSortedList.h
OBJ = $(SRC:.cpp=.o)

all: $(EXECUTABLE)

.SUFFIX: .o .cpp

$(EXECUTABLE): $(OBJ)
	@$(CPP) $(LDFLAGS) $^ -o $@
#$(OBJ): $(HDRS)
.o: .cpp
	@$(CPP) -c $(CPPFLAGS) $< -o $@

doc: $(DOXCONF) $(SRC)
	@$(DOX) $<

clean:
	@$(RM) $(OBJ) $(EXECUTABLE)
