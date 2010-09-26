CPP = /usr/bin/g++
CPPFLAGS = -fPIC -ggdb -O1 
LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXECUTABLE = binningexe 
DOX = /usr/bin/doxygen
DOXCONF = Dox.txt

SRC = GeneDataSet.cpp GeneFeatureData.cpp main.cpp \
	GeneFeatureBins.cpp GeneFeatureItem.cpp
OBJ = $(SRC:.cpp=.o)

all: $(EXECUTABLE)

.SUFFIX: .o .cpp

$(EXECUTABLE): $(OBJ)
	$(CPP) $(LDFLAGS) $^ -o $@

.o: .cpp
	@$(CPP) -c $(CPPFLAGS) $< -o $@

doc: $(DOXCONF) $(SRC)
	@$(DOX) $<

clean:
	@$(RM) $(OBJ) $(EXECUTABLE)
