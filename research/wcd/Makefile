CPP = /usr/bin/g++

# Debugging options. 
MACROS += -DDEBUG_ALL
MACROS += -DDEBUG_CFTREE
MACROS += -DDEBUG_ENTRY
MACROS += -DDEBUG_WCD

MACROS += -DDEBUG_WCD_TEST
MACROS += -DDEBUG_WCD_INSERT

# Flags and compilation variables. 
CPPFLAGS = -O2 # $(MACROS)
LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXECUTABLE = ewcd
DOX = /usr/bin/doxygen
DOXCONF = Doxygen

SRC = main.cpp wcd.cpp cftree.cpp entry.cpp 

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
