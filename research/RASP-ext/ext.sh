swig -python c_ext.i
gcc -fPIC -c c_ext.c c_ext_wrap.c -I/usr/include/python2.6
ld -shared *.o -lpython2.6 -o _c_ext.so
