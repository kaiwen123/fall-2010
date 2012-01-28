
/*
 * Deregister RPC program numbers
 * pmateti@wright.edu  Fri Jul 16 1993
 * usage: deregall <prgnum> <howmany>
*/

#include "wb.h"

int main(int argc, char *argv[])
{
  long prognum = 0x40000000, m = 10, i, d, v;

  if (argc > 1 && argv[1] != NULL) {
    prognum = atol(argv[1]);
  }
  if (argc > 2 && argv[2] != NULL) {
    m = atol(argv[2]);
  }
  printf("%s %ld %ld\n", argv[0], prognum, m);
  for (d = 0, i = prognum; i < prognum + m; i++) {
    for (v = 1; v < 4; v++) {
      int x = pmap_unset(i, v);
      printf("pmap_unset(%ld, %ld) == %d\n", i, v, x);
      d += x;
    }
  }
  printf("%s: deregistered %ld port+version pairs.\n", argv[0], d);
  return 0;
}

