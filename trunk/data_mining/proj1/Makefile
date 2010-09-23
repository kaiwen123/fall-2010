CPP = /usr/bin/g++
CPPFLAGS = -O2 -fPIC -ggdb
LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXECUTABLE = proj1
DOX = /usr/bin/doxygen
DOXCONF = Dox.txt

SRC = GeneDataSet.cpp GeneFeatureData.cpp main.cpp \
	GeneFeatureBins.cpp GeneFeatureItem.cpp
OBJ = $(SRC:.cpp=.o)

all: $(EXECUTABLE)

.SUFFIX: .o .cpp

$(EXECUTABLE): $(OBJ)
	@$(CPP) $(LDFLAGS) $^ -o $@

.o: .cpp
	@$(CPP) -c $(CPPFLAGS) $< -o $@

doc: $(DOXCONF) $(SRC)
	@$(DOX) $<

clean:
	@$(RM) $(OBJ) $(EXECUTABLE)
