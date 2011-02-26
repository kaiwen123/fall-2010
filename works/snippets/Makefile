CPP = /usr/bin/g++
CPPFLAGS = -O -Wno-write-strings
LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib -lc
RM = /bin/rm -rf

SRC = file.cpp
OBJ = $(SRC:.cpp=.o)

all: file
.SUFFIX: .o .cpp
file: $(OBJ)
	@$(CPP) $(LDFLAGS) -o $@ $<

.o: .cpp
	@$(CPP) -c $(CPPFLAGS) -o $@ $<

clean:
	$(RM) $(OBJ) file