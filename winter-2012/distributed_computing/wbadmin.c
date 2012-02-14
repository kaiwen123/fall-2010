/*
 * An example 730-C-RPC White Board Administration program
 *
 * compile with:  gcc wbadmin.c -std=c99  -o wbadmin
 * link 
 */

#include "wb.h"
#define NMSZ 50
//int newserver_1(char * machineName, CLIENT * clp) { return 0; }
int transferwhiteboard_1(struct XferWBArg a, CLIENT * clp) { return 0; }

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

int queryServer(char * snm, int prgn, CLIENT * clp)
{
  int dummy = 0;
  int count =0 ;
  //	printf("Calling query\n");
  struct BBoard * b = query_1(&dummy, clp);
  //	printf("After query: %x\n", (void*)b);
  if (b == 0 || (b->next == NULL && b->clients == NULL))
    printf("Server has no boards.\n");
  else
    for (; b; b = b->next) {
      printf("Board %s on server %s prognum %x has\n",
	     b->clients->clientdata->boardnm, snm, prgn);
      if (b->clients == 0) 	/* for robustness and better functionality ... */
	printf("\tno clients\n");
      else
	for (struct BClient * c = b->clients; c; c = c->next)
	  {
	    printf("\tclient on server %s displayed at %s with prognum %x\n",
		   c->clientdata->machinenm,
		   c->clientdata->xdisplaynm,
		   c->clientdata->nprogram);
	    count = count +1;
	  }
    }
  return count;
}

int main(int argc, char * argv[])
{
  CLIENT * clp = 0;
  int  result = -1;

  /* uncomment this when done. */
  /* if (argc < 4) */
  /*   goto error; */
  char * cmd = argv[1];
  /* if (cmd[0] != '-') */
  /*   goto error; */

  char * host; 
  // added by simon. 
  host = (char *)malloc(strlen("localhost") + 1);
  strcpy(host, "localhost");  // argv[2];

  int prognum = 0x20007161; // strtol(argv[3], 0, 16);
  printf("program number: %x\n", prognum);
  clp = clnt_create(host, prognum, WhiteBoardServerVersion, "tcp");
  if (clp == 0)
    goto error;

  switch (cmd[1]) {
  case 'q':
    result = queryServer(host, prognum, clp);
    break;
  case 'n':
    if (argc < 5)
      goto error;
    result = *(newserver_1(&argv[4], clp)); /* result == prog num of new server */
    break;
  case 't':
    if (argc < 6)
      goto error;
    struct XferWBArg xa;
    strcpy(xa.boardnm, argv[4]);
    strcpy(xa.machinenm, argv[5]);
    xa.nprogram = strtol(argv[6], 0, 16);
    xa.nversion = WhiteBoardServerVersion;
    result = transferwhiteboard_1(xa, clp);
    break;
  default:
    goto error;
  }
 done:
  if (clp)
    clnt_destroy(clp);
  printf("result %d\n", result);
  return result;

 error:				/* more elaborate error reporting is better */
  usage();
  goto done;
}

/* -eof- */
