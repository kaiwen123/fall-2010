CPP = /usr/bin/g++

# Debugging options. 
# MACROS += -DOUT_VERBOSE
#MACROS += -DDEBUG_CATCH
#MACROS += -DDEBUG_ALL
#MACROS += -DDEBUG_CFTREE
#MACROS += -DDEBUG_CFNODE
#MACROS += -DDEBUG_CFNODE_SPLIT
#MACROS += -DDEBUG_ENTRY
#MACROS += -DDEBUG_WCD
#MACROS += -DDEBUG_WCD_PHASE1
#MACROS += -DDEBUG_WCD_PHASE2
#MACROS += -DDEBUG_WCD_TEST
#MACROS += -DDEBUG_WCD_INSERT

# Flags and compilation variables. 
CPPFLAGS = -g $(MACROS)
LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXECUTABLE = ewcd
DOX = /usr/bin/doxygen
DOXCONF = Doxygen

HEADERS = cfnode.h cftree.h config.h entry.h libs.h wcd.h
SRC = main.cpp wcd.cpp cftree.cpp cfnode.cpp entry.cpp libs.cpp

OBJ = $(SRC:.cpp=.o)

# Dependencies and operations. 
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
