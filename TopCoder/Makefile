RM = /bin/rm -v
CPP = /usr/bin/g++
CPPFLAGS = -O2 $(MACROS) -Wall
LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXECUTABLE = sort_test

SRC = select_sort.cpp sort_test.cpp insert_sort.cpp 

OBJ = $(SRC:.cpp=.o)

all: $(EXECUTABLE)

.SUFFIX: .o .cpp

$(EXECUTABLE): $(OBJ)
	$(CPP) $(LDFLAGS) $^ -o $@

.o: .cpp
	$(CPP) -c $(CPPFLAGS) $< -o $@

clean:
	@$(RM) $(OBJ) $(EXECUTABLE) *~
