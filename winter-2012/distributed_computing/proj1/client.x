
/* client.x */

#include "oneln.h"

program WhiteBoardClient {
  version WhiteBoardClientVersion {
    void callbackfromwbs(OneLn) = 1;
  } = 1;
} = 0x20007766;			/* replaced by gettransient() value */

/* -eof- */
