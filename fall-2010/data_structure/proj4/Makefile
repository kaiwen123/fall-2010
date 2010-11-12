CPP = /usr/bin/g++
CPPFLAGS =

LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXE = proj4.double
DOX = /usr/bin/doxygen
DOXCONF = Dox.txt

SRC = main.double.cpp ClosedHash.cpp
OBJ = $(SRC:.cpp=.o)

EXE1 = proj4.rand
SRC1 = main.rand.cpp RandHash.cpp
OBJ1 = $(SRC1:.cpp=.o)

all: $(EXE) $(EXE1)

.SUFFIX: .o .cpp

$(EXE): $(OBJ)
	$(CPP) $(LDFLAGS) $^ -o $@

$(EXE1): $(OBJ1)
	$(CPP) $(LDFLAGS) $^ -o $@

.o: .cpp
	@$(CPP) -c $(CPPFLAGS) $< -o $@

doc: $(DOXCONF) $(SRC)
	@$(DOX) $<

clean:
	@$(RM) $(OBJ) $(OBJ1) $(EXE) $(EXE1)