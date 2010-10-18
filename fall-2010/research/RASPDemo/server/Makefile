# Version 1.0.1
# Author Shumin Guo(gsmsteve@gmail.com).
CPP = /usr/bin/g++
#MACROS += -DDEBUG_ALL
INCLUDES = -I/usr/local/include/spatialindex
CPPFLAGS = -O2 -ggdb3 $(MACROS) $(INCLUDES) #-Wall
LDFLAGS = -L/usr/lib -L/lib -lspatialindex
RM = /bin/rm -rf
EXECUTABLE = RASPServer
DOX = /usr/bin/doxygen
DOXCONF = Dox.txt

SRC = DataServer.cpp DataStore.cpp main.cpp RequestMsg.cpp ResponseMsg.cpp
OBJ = $(SRC:.cpp=.o)

all: $(EXECUTABLE)

.SUFFIX: .o .cpp

$(EXECUTABLE): $(OBJ)
	$(CPP) $(LDFLAGS) $^ -o $@

.o: .cpp
	$(CPP) -c $(CPPFLAGS) $< -o $@

doc: $(DOXCONF) $(SRC)
	@$(DOX) $<

clean:
	@$(RM) $(OBJ) $(EXECUTABLE)
