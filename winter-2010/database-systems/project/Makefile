# makefile for the database project. 
RM = `which rm`
CPP = `which g++`
CPPFLAGS = -Wall 

EXE = order
LIBS = -lmysqlcppconn

SRCS = order.cpp funcs.cpp
OBJS = $(SRCS:.cpp=.o)

all: $(EXE)

.SUFFIX: .o .cpp

$(EXE):$(OBJS)
	@$(CPP) $(LIBS) $^ -o $@

.o: .cpp
	@$(CPP) -c $(CPPFLAGS) $< -o $@

clean: 
	@$(RM) -rf $(OBJS) $(EXE) 