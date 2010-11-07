CPP = /usr/bin/g++
CPPFLAGS =

LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXE = proj4
DOX = /usr/bin/doxygen
DOXCONF = Dox.txt

SRC = main.cpp ClosedHash.cpp
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
