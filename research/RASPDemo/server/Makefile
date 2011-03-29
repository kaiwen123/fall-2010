##################################################
# Version 1.5.1
# Time : Sun Oct 24 13:23:10 EDT 2010
# Author Shumin Guo(gsmsteve@gmail.com).
##################################################

# Programs to use. 
CPP = /usr/bin/g++
RM = /bin/rm -rf
LD = /usr/bin/ld
DOX = /usr/bin/doxygen

# Macros and variables. 
#MACROS += -DDEBUG_ALL
#MACROS += -DDEBUG_PARSING_DATA
#MACROS += -DDEBUG_INSERT_DATA
MACROS += -DDEBUG_QUERY_DATA
INCLUDES = -I/usr/local/include/spatialindex
CPPFLAGS = -O2 -fPIC -ggdb3 $(MACROS) $(INCLUDES) #-Wall
LDFLAGS = -L/usr/lib -L/lib -lspatialindex

# Source files. 
SRC = DataServer.cpp DataStore.cpp
DOXCONF = Dox.txt

# Executables and object files. 
EXECUTABLE = RASPServer
WRAPPERLIB = _rasp_demo.so
OBJ = $(SRC:.cpp=.o)
WRAPPEROBJ = wrapper.o wrapper_wrap.o $(OBJ)

all:  $(WRAPPERLIB)

.SUFFIX: .o .cpp .cxx .i

$(EXECUTABLE): $(OBJ)
	$(CPP) $(LDFLAGS) $^ -o $@

.o: .cpp
	$(CPP) -c $(CPPFLAGS) $< -o $@

# Rules to build the wrapper library.
$(WRAPPERLIB):$(WRAPPEROBJ)
	$(CPP) -shared -lpython2.7 $(LDFLAGS) $^ -o $@

wrapper_wrap.cxx:wrapper.i
	swig -c++ -python $<

wrapper_wrap.o: wrapper_wrap.cxx
	$(CPP) -I/usr/include/python2.7 -c $(CPPFLAGS) $< -o $@

doc: $(DOXCONF) $(SRC)
	@$(DOX) $<

clean:
	@$(RM) $(OBJ) $(EXECUTABLE) $(WRAPPERLIB) $(WRAPPEROBJ) wrapper_wrap.cxx
