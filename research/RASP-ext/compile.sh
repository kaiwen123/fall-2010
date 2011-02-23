g++ -I/usr/local/include/spatialindex -lspatialindex -lgsl -lgslcblas -lm RTreeQuery.cc -o RTreeQuery

g++ -L/home/ada/softwares/lib -I/home/ada/softwares/include/spatialindex -lspatialindex -lgsl -lgslcblas -lm MyRTreeLoad.cc -o MyRTreeLoad

# # Compiling for the c matrix implementation
swig -python c_ext.i
gcc -fpic -c c_ext.c c_ext_wrap.c -I/usr/include/python2.6
ld -shared c_ext.o c_ext_wrap.o -lpython2.6 -o _c_ext.so
