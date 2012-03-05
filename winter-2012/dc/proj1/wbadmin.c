/*
 * An example 730-C-RPC White Board Administration program
 *
 * compile with:  gcc wbadmin.c -std=c99  -o wbadmin
 * link 
 */

#include "wb.h"
#define NMSZ 50
// int newserver_1(char * machineName, CLIENT * clp) { return 0; }
// int transferwhiteboard_1(struct XferWBArg a, CLIENT * clp) { return 0; }

void usage()
{
  fprintf
    (stderr,
     "\nNote: Linked with dummy query, newserver, transferwhiteboard functions.\n\n"
     "Usage: -q for query, -n for new server create, -t for transfer board\n"
     "wbadmin -q <server-machine-nm> <prognum-in-hex>\n"
     "wbadmin -n <existing-server-machine-nm> <prognum-in-hex> <new-server-machine-nm>\n"
     "wbadmin -t <from-server-machine-nm> <prognum-in-hex> <wb-nm> <to-server-machine-nm> <prognum>\n");
}

/**
 * @brief server querying function, which querys the server and get a
 * list of boards from the server. Then we iterate over the list and
 * print out the clients and lines attached to the boards.
 * @param snm server name to query. 
 * @param prgn program number for the server. 
 * @param clp client pointer, which indicates we need to create a new
 * client to the server before doing the querying thing. 
 * @return number of boards on the given server with progn.
 */
int queryServer(char * snm, int prgn, CLIENT * clp)
{
  int dummy = 0;
  int ccnt = 0, bcnt = 0 ;
  BBoardp b, * bp = query_1(&dummy, clp);
  if (!bp) {
    printf("Server has no boards.\n");
    return 0; 
  }
  // no boards returned from the server.
  fprintf(stdout, "\nQuery Results: \n"); 
  for (b = *bp; b; b = b->next) {
    printf("Board %s on server %s prognum %x has\n",
    	   b->clients->clientdata.boardnm, snm, prgn);

    // print clients information for the current board.
    struct BClient * clnt = b->clients;
    while(clnt) {
      printf("\tclient on server %s displayed at %s with prognum %x\n",
	     clnt->clientdata.machinenm, clnt->clientdata.xdisplaynm,
	     clnt->clientdata.nprogram);

      ccnt++; 
      clnt = clnt -> next; 
    }

    // print the lines in this board.
    struct ALine * lnp = b -> lines; 
    while(lnp) {
      fprintf(stdout, "Line: (%d, %d) to (%d, %d), color 0x%x.\n", 
	      lnp->ln.x1, lnp->ln.y1, lnp->ln.x2, lnp->ln.y2, lnp->ln.color); 
      lnp = lnp -> next;
    }
    bcnt++;
  } // for, loop around the board list. 

  // print results.
  fprintf(stdout, "\nTotal Boards: %d, total clients %d.\n\n", bcnt, ccnt);
  return bcnt; 
}

// main function to start the white board admin. 
int main(int argc, char * argv[])
{
  CLIENT * clp = 0;
  int  result = -1;

  if (argc < 4)
    goto error;
  char * cmd = argv[1];
  if (cmd[0] != '-')
    goto error;

  char * host = argv[2];
  int prognum = strtol(argv[3], 0, 16);
  printf("program number: %x\n", prognum);
  clp = clnt_create(host, prognum, WhiteBoardServerVersion, "tcp");
  if (!clp) {
    fprintf(stderr, "%s %s : %d. \n", "Can't create client. ", 
	    __FILE__, __LINE__); 
    printf("Maybe your server with given program number was not started.\n"); 
    goto done;
  }

  switch (cmd[1]) {
  case 'q':
    result = queryServer(host, prognum, clp);
    break;
  case 'n':
    if (argc < 5)
      goto error;
    /* result == prog num of new server */
    result = *(newserver_1(&argv[4], clp));
    break;
  case 't':
    if (argc < 6)
      goto error;
    struct XferWBArg xa;
    strcpy(xa.boardnm, argv[4]);
    strcpy(xa.machinenm, argv[5]);
    xa.nprogram = strtol(argv[6], 0, 16);
    xa.nversion = WhiteBoardServerVersion;
    result = *(transferwhiteboard_1(&xa, clp));
    break;
  default:
    goto error;
  }
 done:
  if (clp)
    clnt_destroy(clp);
  // printf("result %d\n", result);
  return result;

 error:				/* more elaborate error reporting is better */
  usage();
  goto done;
}

/* -eof- */
