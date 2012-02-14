/**
 * @file server.x == WhiteBoard Server RPC Interface 
 *
 */

#define NMSZ	50

#include "oneln.h"

typedef struct ALine *Linep;

/**
 * \class ALine 
 * \brief Linked list of lines. 
 */
struct ALine {
  struct ALine *next;		/* generic singly-linked list */
  OneLn ln;			/* see oneln.h */
};

/**
 * \class ClientData 
 * \brief RPC request data given by clients. 
 * \see client.x
 */ 
struct ClientData {
  char boardnm[NMSZ];		/* board name */
  char xdisplaynm[NMSZ];	/* X11 display name */
  char machinenm[NMSZ];		/* name of machine running this client */
  int nprogram;			/* rpc prog num for callbacks */
  int nversion;			/* registered version number */
  long color;
};

/* structures used by the white board admin. */
struct BClient {		/* cf with AClient */
  struct BClient *next;
  ClientData * clientdata;
				/* CLIENT *callback; removed */
};

/**
 * \class BBoard. 
 */
struct BBoard {			/* cf with ABoard */
  struct BBoard *next;
  struct BClient *clients;
  ALine *lines;
};

/**
 * \class AddLineArg
 */
struct AddLineArg {
  OneLn ln;
  ClientData clientdata;
};

/**
 * \brief <span style="color:red">Interface of RPC server. It
 * specifies all the services that the server provides. </span>
 */ 
program WhiteBoardServer {
  version WhiteBoardServerVersion {
    int addclient(ClientData) = 1; /* when new client registered. */
    int delclient(ClientData) = 2; /* on right click. */
    int addline(AddLineArg) = 3;   /* when left clicked. */
    Linep sendallmylines(ClientData) = 4; /* when new client registered. */
    BBoard query(int) = 5;	  /* when webadmin queries server. */
  } = 1;
} = 0x20000099;			/* change to your own last 4 digits */

/* -eof- */
