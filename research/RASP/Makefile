CXX = /usr/bin/g++
RM = /bin/rm -v -rf
CXXFLAGS = -O2 -fPIC -I/usr/local/include/spatialindex
LIBS = -L/home/ada/softwares/lib -lspatialindex -lgsl -lgslcblas -lm
PROG = RTreeQuery
OBJS = RTreeQuery.o SimonVisitor.o
SRCS = ($(OBJS):.o=.cc)
QUERYFILES = query.txt query.txt*

all:$(PROG)

.SUFFIX: .o .cc

.o: .cc
	$(CXX) -c $(CXXFLAGS) -o $@ $<

$(PROG):$(OBJS)
	$(CXX) $(LIBS) -o $@ $<

adult:
	@./scheduler-adult.sh

norm:
	@./scheduler-norm.sh

simu:
	@./scheduler-simu.sh
clean:
	@$(RM) $(OBJS) $(QUERYFILES)
