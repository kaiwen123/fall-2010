CPP = /usr/bin/g++
#MACROS += -DPRINT_GAIN_INFO
#MACROS += -DDEBUG_DATA
#MACROS += -DDEBUG_DATA_ENTROPY
#MACROS += -DDEBUG_INFO_GAIN
#MACROS += -DDEBUG_K_TOP
#MACROS += -DDEBUG_DATA_SET
#MACROS += -DDEBUG_BINS
#MACROS += -DDEBUG_ITEM
#MACROS += -DDEBUG_BINS_ENTROPY
#MACROS += -DSORT_ENTROPY
CPPFLAGS = -O2 -ggdb3 $(MACROS)
LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXECUTABLE = binningexe 
DOX = /usr/bin/doxygen
DOXCONF = Doxygen

SRC = GeneDataSet.cpp GeneFeatureData.cpp main.cpp \
	GeneFeatureBins.cpp GeneFeatureItem.cpp
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
