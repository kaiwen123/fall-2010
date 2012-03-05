
/* client.x */

#include "oneln.h"

struct XferWBArg {
	char boardnm[50];
	char machinenm[50];
	int nprogram;
	int nversion;
};

program WhiteBoardClient {
  version WhiteBoardClientVersion {
    void callbackfromwbs(OneLn) = 1;
    int clienttransfer(XferWBArg) = 2;
  } = 1;
} = 0x20007766;			/* replaced by gettransient() value */

/* -eof- */
