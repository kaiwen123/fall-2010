
/* server.x == WhiteBoard Server RPC Interface */

#define NMSZ	50

#include "oneln.h"

typedef struct ALine * Linep;
typedef struct BBoard * BBoardp; 

struct ALine {
  struct ALine *next;		/* generic singly-linked list */
  OneLn ln;			/* see oneln.h */
};

struct ClientData {
  char boardnm[NMSZ];		/* board name */
  char xdisplaynm[NMSZ];	/* X11 display name */
  char machinenm[NMSZ];		/* name of machine running this client */
  int nprogram;			/* rpc prog num for callbacks */
  int nversion;			/* registered version number */
  long color;
};

struct AddLineArg {
  OneLn ln;
  ClientData clientdata;
};

struct BBoard {			/* cf with ABoard */
  struct BBoard *next;
  struct BClient *clients;
  ALine *lines;
};

/* The declarations for BClient, BBoard, XferWBArg, query, newserver,
 * and transferwhiteboard should go into the new server.x */
struct BClient {		/* cf with AClient */
  struct BClient *next;
  ClientData clientdata;
				/* CLIENT *callback; removed */
};

struct XferWBArg {
  char boardnm[NMSZ];		/* board name to be transferred */
  char machinenm[NMSZ];		/* name of the receiving machine */
  int nprogram;			/* rpc prog num of the server above */
  int nversion;			/* registered version number */
};

program WhiteBoardServer {
  version WhiteBoardServerVersion {
    int addclient(ClientData) = 1;
    int delclient(ClientData) = 2;
    int addline(AddLineArg) = 3;
    Linep sendallmylines(ClientData) = 4;
    BBoardp query(int) = 5;
    int newserver(string) = 6;
    int transferwhiteboard(XferWBArg) = 7;
    int sendwbtonewserver(BBoard) = 8; 
  } = 1;
} = 0x20007161;			/* change to your own last 4 digits */

/* -eof- */
