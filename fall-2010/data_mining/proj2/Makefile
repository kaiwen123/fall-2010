CPP = /usr/bin/g++
#MACROS += -DDEBUG_DATA_LOAD
#MACROS += -DDEBUG_ITEM_MAP
#MACROS += -DDEBUG_LEVEL_TWO
#MACROS += -DDEBUG_APRIORI
#MACROS += -DDEBUG_APRIORI_JOIN
#MACROS += -DDEBUG_APRIORI_PRUNE
#MACROS += -DDEBUG_HASH_INSERT
MACROS += -DDEBUG_HASH_INSERTSET
MACROS += -DDEBUG_ITEM
MACROS += -DDEBUG_ITEMSET
MACROS += -DDEBUG_HASHTREE
MACROS += -DDEBUG_GENEDATA
MACROS += -DDEBUG_DATASET

CPPFLAGS = -fPIC -O2 -ggdb3 $(MACROS)
LD = /usr/bin/ld
LDFLAGS = -L/usr/lib -L/lib
RM = /bin/rm -rf
EXECUTABLE = AssoRuleMiner
DOX = /usr/bin/doxygen
DOXCONF = Doxygen

SRC = main.cpp DataSet.cpp Itemset.cpp HashTree.cpp

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
