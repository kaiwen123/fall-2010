##################################################
# Version 1.0.1
# Author Shumin Guo(gsmsteve@gmail.com).
##################################################

# Programs to use. 
CPP = /usr/bin/g++
RM = /bin/rm -rf
LD = /usr/bin/ld
DOX = /usr/bin/doxygen

# Macros and variables. 
#MACROS += -DDEBUG_ALL
INCLUDES = -I/usr/local/include/spatialindex
CPPFLAGS = -O2 -fPIC $(MACROS) $(INCLUDES) #-Wall
LDFLAGS = -L/usr/lib -L/lib -lspatialindex

# Executables and object files. 
EXECUTABLE = RASPServer
WRAPPERLIB = _rasp_demo.so
OBJ = $(SRC:.cpp=.o)
WRAPPEROBJ = wrapper.o wrapper_wrap.o

# Source files. 
SRC = DataServer.cpp DataStore.cpp main.cpp RequestMsg.cpp ResponseMsg.cpp
DOXCONF = Dox.txt

all: $(EXECUTABLE) $(WRAPPERLIB)

.SUFFIX: .o .cpp .cxx .i

$(EXECUTABLE): $(OBJ)
	$(CPP) $(LDFLAGS) $^ -o $@

.o: .cpp
	$(CPP) -c $(CPPFLAGS) $< -o $@

# Rules to build the wrapper library.
$(WRAPPERLIB):$(WRAPPEROBJ)
	$(CPP) -shared -lpython2.6 $< -o $@

wrapper_wrap.cxx:wrapper.i wrapper.h
	swig -c++ -python $<

wrapper_wrap.o: wrapper_wrap.cxx
	$(CPP) -I/usr/include/python2.6 -c $(CPPFLAGS) $< -o $@

doc: $(DOXCONF) $(SRC)
	@$(DOX) $<

clean:
	@$(RM) $(OBJ) $(EXECUTABLE) $(WRAPPERLIB) $(WRAPPEROBJ) wrapper_wrap.cxx
