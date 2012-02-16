// Author Shumin Guo. 
// UID : U00617724
/* server.c */

#include "wb.h"
static AddLineArg wblines;
/*
 * Generic node in a singly-linked list
 */
typedef struct ListNode {
  struct ListNode *next;
} ListNode;

/**
 * @brief Insert given node from the given node lists. This is a
 * static function, and thus can only be called from within the
 * current file. 
 * @pre The given node to be inserted is not NULL.
 * @post The given node will be inserted into the given list,
 * identified by the list header hdr. 
 * @param hdr header of the list to be inserted into. 
 * @param d the node to be inserted.
 * @return void. 
 */
static void insert(ListNode ** hdr, ListNode * p)
{
  // assert(p); // node p should not be NULL.
  if (hdr == NULL)
    return;
  p->next = *hdr;
  *hdr = p;
}

/**
 * @brief Delete given node from the given node lists. This is a
 * static function, and thus can only be called from within the
 * current file. 
 * @pre The given node is not NULL and the node list is not empty. 
 * @post If node exists in the node, then it will be deleted, the
 * length of the node will be shorter than one. And if the node does
 * not exists in the node list, then the node list will not be
 * changed. 
 * @param hdr header of the list to be deleted from. 
 * @param d the node to be deleted. 
 * @return void. 
 */
static void delete(ListNode ** hdr, ListNode * d) {
  ListNode *p, *q;

  if (hdr == NULL || *hdr == NULL)
    return;
  for (p = (ListNode *) hdr, q = p->next; q; p = q, q = q->next) {
    if (q == d) {
      p->next = q->next;
      free(q);
      return;
    }
  }
}

typedef struct AClient {
  struct AClient *next;
  ClientData clientdata;
  CLIENT *callback;		/* rpc.h */
} AClient;

typedef struct ABoard {
  struct ABoard *next;
  /* name of the board is in clients->clientdata.boardnm */
  AClient *clients;		/* list of clients on one board */
  ALine *lines;			/* list of LINEs that it has */
} ABoard;

static ABoard *boards = NULL;	/* list of boards that server has */

/* Find the white board with name nm[].  */
static ABoard *find_wbp(char *nm) {
  ABoard *p;

  for (p = boards; p; p = p->next) {
    if (strcmp(nm, p->clients->clientdata.boardnm) == 0)
      break;
  }
  return p;
}

/*
 * Add a client.  May start a new white board.  We clnt_create once
 * for each client.
 */
int *addclient_1_svc(ClientData * cd, struct svc_req *srq)
{
  static int result;		/* note: static */
  ABoard *ab = find_wbp(cd->boardnm);
  AClient *q = (AClient *) malloc(sizeof(AClient));

#ifdef __DEBUG__ 
  fprintf(stdout, "addclient: Client from machine %s, to board %s %s : %d.\n",
	  cd->machinenm, cd->boardnm, __FILE__, __LINE__); 
#endif 

  if (q == NULL)
    goto error;
  q->clientdata = *cd;
  q->callback =
    clnt_create(cd->machinenm, cd->nprogram, cd->nversion, "tcp");
  if (q->callback == NULL) {
    free(q);
    goto error;
  }

  if (ab == NULL) {
    /* new white board */
    ab = (ABoard *) malloc(sizeof(ABoard));
    if (ab == NULL)
      goto error;
    ab->lines = NULL;
    ab->clients = NULL;
    insert((ListNode **) & boards, (ListNode *) ab);
  }
  insert((ListNode **) & ab->clients, (ListNode *) q);
  result = 0;

#ifdef __DEBUG__
  fprintf(stdout, "Client from %s ...successfully created. %s -- %d.\n", 
	  cd->machinenm, __FILE__, __LINE__); 
#endif

  return &result;
 error:
  result = -1;
  return &result;
}

/**
 * @brief Commit suicide!  Unregister yourself. Invoked as SIGALRM handler.
 */
static void die(int dummy)
{
  int x = pmap_unset(WhiteBoardServer, WhiteBoardServerVersion);
  exit(x != 1);
}

/**
 * @brief Delete the boards. 
 * @param ab ABoard object pointer. 
 * @return void. 
 */
static void delboard(ABoard * ab)
{
  ALine *lp, *lq;

  for (lp = ab->lines; lp; lp = lq) {
    lq = lp->next;
    free(lp);
  }

  // clients with given board was deleted. 
  delete((ListNode **) & boards, (ListNode *) ab);
}

/**
 * @brief Delete a client.  If this is the last client on a
 * whiteboard, delete the board too.  If no more boards left, kill
 * yourself. 
 */
int *delclient_1_svc(ClientData * cd, struct svc_req *srq)
{
  static int result;		/* note: static */
  AClient *p;
  ABoard *ab = find_wbp(cd->boardnm);

  if (ab == NULL)
    goto error;

  /* delete the client; search on nprogram and machinenm */
  for (p = ab->clients; p; p = p->next) {
    if (p->clientdata.nprogram == cd->nprogram
	&& strcmp(p->clientdata.machinenm, cd->machinenm) == 0) {
#ifdef __DEBUG__ 
      fprintf(stdout, "Client from %s on board %s was deleted %s : %d. \n", 
	      cd->machinenm, cd->boardnm, __FILE__, __LINE__); 
#endif
      clnt_destroy(p->callback);
      delete((ListNode **) & ab->clients, (ListNode *) p);
      if (ab->clients == NULL)
	delboard(ab);
      break;
    }
  }
  if (boards == NULL) {
    /* server has no clients; so die *after* doing a return &i. */
    struct sigaction asigalrm;
    asigalrm.sa_flags = 0;
    asigalrm.sa_handler = die;
    sigemptyset(&asigalrm.sa_mask);
    sigaction(SIGALRM, &asigalrm, 0);	/* install the signal handler */
    alarm(1);			/* invoke die() after 1 second */
  }

  result = 0;
  return &result;
 error:
  result = -1;
  return &result;
}

/**
 * @brief A clients gives us a new line.  Get the coordinates of the line and
 * distribute among the clients.
 * @param ap add line argument.
 * @param srq service request struct pointer. 
 * @return -1 on failure, else >= 0; 
 */
int *addline_1_svc(AddLineArg * ap, struct svc_req *srq)
{
  static int result;		/* note: static */
  AClient *p;
  ALine *lp = (ALine *) malloc(sizeof(ALine));
  ABoard *ab = find_wbp(ap->clientdata.boardnm);

  if (ab == NULL || lp == NULL)
    goto error;

  /* add the line to the list of lines on this board */
  lp->ln = ap->ln;
  insert((ListNode **) & ab->lines, (ListNode *) lp);
#ifdef __DEBUG__
  fprintf(stdout, "Line (%d,%d)->(%d,%d) was added to board %s --"	\
	  "%s : %d. \n", ap->ln.x1, ap->ln.y1, ap->ln.x2, ap->ln.y2, 
	  ap->clientdata.boardnm, __FILE__, __LINE__); 
#endif

  /* tell all clients on this board of this addition */
  for (p = ab->clients; p; p = p->next) {
    callbackfromwbs_1(&ap->ln, p->callback);
  }
  result = 0;
  return &result;
 error:
  result = -1;
  return &result;
}

/*
 * A client wants to know all the lines present on his white board.
 */
Linep *sendallmylines_1_svc(ClientData * cd, struct svc_req * srq)
{
  static ALine *lp = NULL;	/* note: static */
  ABoard *ab = find_wbp(cd->boardnm);
  return (ab ? &ab->lines : &lp);
}

/**
 * @brief Query service. Return all information about all clients
 * @param unused a dummy integer. 
 * @param srq the service request struct.
 * @return A pointer to the list of boards. 
 */
BBoardp * query_1_svc (int * unused, struct svc_req *srq)
{
  static BBoard *bboards ;
  bboards = NULL; 
  BBoard *bbp = NULL ; // pointer for insert operations.
  ABoard *abp = boards; // ABoard pointer.
  
  if (!boards) {
    fprintf(stderr, "No boards are created on the server.\n");
    return &bboards; 
  }
  
#ifdef __DEBUG__ 
  // if boards are no empty, then print out the boards names and
  // client names; 
  fprintf(stdout, "%s\n", "Existing boards and clients.");
  while (abp) {
    fprintf(stdout, "board: %s\t client: %s\n", 
	    abp->clients->clientdata.boardnm, 
	    abp->clients->clientdata.machinenm); 
    abp = abp->next;
  }
#endif
  abp = boards; 
  // Copy all the ABoards boards to the BBoards bboards. 
  while(abp) {
    bbp = (BBoard *)malloc(sizeof(BBoard)); 
    if(!bbp) {
	fprintf(stderr, "%s %s:%d.\n", "can't create BBoard object",
		__FILE__, __LINE__);
	goto error; 
    }

    bbp -> next = NULL; 
    bbp -> clients = NULL; 
    bbp -> lines = NULL; 

    /* copy info from abp to bbp. */
    // copy clients data. 
    AClient *aclnt = abp->clients; 
    BClient *bclnt = NULL; 

    //////////////////////
    // copy clients linked list. 
    //////////////////////
    while(aclnt) {
      bclnt = (BClient *)malloc(sizeof(BClient)); 

      // deep copy of client object data from abp to bbp.
      if(!bclnt) {
	fprintf(stderr, "%s %s:%d.\n", "can't create BClient object",
		__FILE__, __LINE__); 
	goto error; 
      }

      // assign the clients pointer of the client list.
      // if(!bbp->clients) bbp->clients = bclnt; 
      // copy client data from aclnt to bclnt.
      bclnt -> clientdata = aclnt -> clientdata;
      bclnt -> next = NULL; 

      // insert into list. 
      insert((ListNode **) & bbp->clients, (ListNode *) bclnt); 

      aclnt = aclnt -> next; 
    } // while, copy client list. 

    //////////////////////
    // copy lines data. 
    //////////////////////
    ALine * aln = abp -> lines; 
    ALine * bln = NULL;
    while(aln) {
      bln = (ALine *)malloc(sizeof(ALine)); 
      if(!bln) {
	fprintf(stderr, "%s %s : %d.\n", 
		"Error creating new ALine object", 
		__FILE__, __LINE__); 
	goto error; 
      }

      bln -> next = NULL; 
      bln -> ln = aln -> ln; 
      insert((ListNode **) & bbp -> lines, (ListNode *) bln) ;

      aln = aln -> next; 
    } // while, copying a list of lines for board abp to board bbp. 

    // insert the board node into the list. 
    insert((ListNode **) & bboards, (ListNode *) bbp);

    abp = abp -> next;
  } // while, copy ABoard abp to BBoard bbp. 

  /* if (bboards) { */
  /*   bbboards = bboards;  */
  return &bboards; 
    //  }
  
 error: 
  bboards = NULL; 
  return &bboards; 
}

/**
 * @brief function to create a new server, what this function does is
 * actually to login to the machine specified and they run the
 * server730 command. 
 * @param newservername location of the new server. 
 * @param srq service request struct. 
 * @return -1 on failure and positive number on success. 
 */
int *newserver_1_svc(char **newservername,  struct svc_req *srq)
{
  static int retval = 0;

  /* get location of executable from environment. */
  char * execloc = getenv("SERVEREXE"); 
  if (!execloc) {
    fprintf(stderr, "can't find location of server executable. %s:%d",
	    __FILE__, __LINE__); 
    goto error; 
  }
  // strcat(execloc, " &"); // ensure work in the background. 

#ifdef __DEBUG__ 
  printf ("Starting a new server, on %s --- %s : %d\n",
	  *newservername, __FILE__, __LINE__);
#endif 

  char sshstr[255] = "ssh -f ";

  /* if ((strcmp(*newservername, "localhost") == 0)) { */
  /*   strcpy(sshstr, execloc); */
  /* } else { */
  strcat(sshstr, *newservername);
  strcat(sshstr, " "); // add space.
  strcat(sshstr, execloc);
    //  }
  
#ifdef __DEBUG__ 
  fprintf(stdout, "ssh command: %s, %s : %d.\n", 
	  sshstr, __FILE__, __LINE__);  
#endif 

  // at last run the command. 	
  retval = system(sshstr);
  return &retval; 

 error: 
  retval = -1; 
  return &retval;
}

/**
 * @brief Transfer white board from one server to another server. 
 * @param bname the name of board to be transfered. 
 * @param srq service request struct. 
 * @return -1 on failure, and number of clients transfered on success. 
 */
int * transferwhiteboard_1_svc(XferWBArg *xfarg, struct svc_req *srq) {
  static int retval = 0 ;

  // find board with the given board name. 
  char bname[255]; 
  strcpy(bname, xfarg -> boardnm); 
  ABoard * wbp = find_wbp(bname); 

  if(!wbp) {
    fprintf(stderr, "%s %s.\n", "No board named", bname); 
    goto error; 
  }

  // if white board exists, then print the name of clients connected
  // to this board. 
  AClient *acp = wbp -> clients; 
  while(acp) {
    fprintf(stdout, "Client: %s - %x - %d.\n", 
	    acp->clientdata.machinenm, 
	    acp->clientdata.nprogram, 
	    acp->clientdata.nversion); 

    // call the client to transfer to the new board. 
    retval = clienttransfer_1(xfarg, acp -> callback); 

    acp = acp -> next; 
  }

  // transfer all lines on this board to the new server. 
  // create connection to the new server. 
  clp = clnt_create(xfarg->machinenm, xfarg->nprogram,
		    xfarg->nversion, "tcp"); 
  if(!clp) {
    fprintf(stderr, "Error creating connection to the new server.\n"); 
    goto error; 
  }
  sendallmylines_1(&wblines.clientdata, clp); 

  // after transfering the clients to the new server, the old server
  // should delete the existing named board. 
  delboard(wbp); 

  return &retval; 

 error:
  retval = -1; 
  return &retval ;
} // transfer white boards. 

/* -eof- */
