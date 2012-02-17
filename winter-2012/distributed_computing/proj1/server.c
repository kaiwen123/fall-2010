/**
 * @file server.c The implementation of server services including adding and
 * deleting clients, boards and lines on the whiteboard. 
 * @version 1.0 
 * @author Shumin Guo. 
 */

#include "wb.h"
static AddLineArg wblines;
/* when transfering a board from old server to the new server, keep track of the
   client pointer to the old server. */
static CLIENT *sclp; 		

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

/**
 * @class AClient The client node that form the client list in the
 * white board structure.
 * @property next pointer to the next AClient node.
 * @property clientdata The client data for RPC operations.
 * @property callback The call back function used by the server
 * callback.
 * @pre none.
 * @post none.
 */
typedef struct AClient {
  struct AClient *next;
  ClientData clientdata;
  CLIENT *callback;		/* rpc.h */
} AClient;

/**
 * @class ABoard The white board node that forms the client list in the
 * white board structure.
 * @property next pointer to the next ABoard node.
 * @property clients all the clients connected to this board.
 * @property lines all the lines on the current board.
 * @pre none.
 * @post none.
 */
typedef struct ABoard {
  struct ABoard *next;
  /* name of the board is in clients->clientdata.boardnm */
  AClient *clients;		/* list of clients on one board */
  ALine *lines;			/* list of LINEs that it has */
} ABoard;

// The list of boards kept by the server. 
static ABoard *boards = NULL;	/* list of boards that server has */

/**
 * @brief Find the white board with name nm[]. 
 * @pre given nm should not be empty string.
 * @post The board was either found and returned or NULL was returned
 * and the existing board lists structure will not be changed. 
 * @param nm A string that contains the name of board. 
 * @return NULL if no board was found having given board name nm; and
 * a pointer to the found ABoard structure if found. 
 */
static ABoard *find_wbp(char *nm) {
  ABoard *p;

  for (p = boards; p; p = p->next) {
    if (strcmp(nm, p->clients->clientdata.boardnm) == 0)
      break;
  }
  return p;
}

/**
 * @brief Add a client. May start a new white board. We clnt_create once
 * for each client. If the board the client is trying to connect to
 * does not yet exist, it will be created. 
 * @pre the clientdata object pointer cd contains sufficient and good
 * data about the client, such as the client machine name, the program
 * number and etc.
 * @post client and/or board was successfully created/registered with the
 * server and the function evaluates to a non-negative number. 
 * @param cd the clientdata structure that contains info for the
 * client.
 * @param srq service request structure. 
 * @return pointer to integer, the value will be -1 on failure and
 * non-negative otherwise. 
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
 * @pre ab should point to a valid ABorad object with all the fields properly
 * set. 
 * @post board is deleted. 
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
 * @brief Delete a client. If this is the last client on a
 * whiteboard, delete the board too. If no more boards left, kill
 * yourself. This function will first search the board with the given
 * board name in the clientdata struct, and then delete the client
 * from the result structure. If no board was found, return -1.
 * @pre pointer cd should point to a good clientdata object, with
 * sufficient information about the client. 
 * @post the client be deleted from the clients list of the board, or
 * return -1 on error. 
 * @param cd the clientdata structure that contains the info about
 * client. 
 * @param srq the service request structure. 
 * @return pointer to integer, -1 on failure and 0 on success.
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
 * @brief Query service. Return all information about all clients and
 * boards. This function will copy current board list to a new list,
 * and the copy will be a deep copy containing all the clients list
 * and lines list and then return the copied list. 
 * @pre unused be given a valid integer number and the srq be the
 * proper service request client.
 * @post A list of BBoards returned if there are any and NULL returned
 * if no board exists on *this* server. 
 * @param unused a dummy integer. 
 * @param srq the service request struct.
 * @return A pointer to the list of boards if there are any and NULL
 * otherwise. 
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

  return &bboards; 
  
 error: 
  bboards = NULL; 
  return &bboards; 
}

/**
 * @brief function to create a new server, what this function does is
 * actually to login to the machine specified and they run the
 * server730 command. 
 * @pre newsservername contains a valid name of the new server; and
 * srq points to a valid service request struct. 
 * @post a new server was created and 0 returned or -1 returned on
 * error. 
 * @param newservername location of the new server. 
 * @param srq service request struct. 
 * @return -1 on failure and a positive number on success. 
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
  fprintf (stdout, "Starting a new server, on %s --- %s : %d\n",
	  *newservername, __FILE__, __LINE__);
#endif 

  char sshstr[255] = "ssh -f ";

  strcat(sshstr, *newservername);
  strcat(sshstr, " "); // add space between hostname and cmd. 
  strcat(sshstr, execloc);
  
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
 * @brief Transfer white board from one server to another
 * server. Specifically, the function first finds the board with given
 * board name and tells all the clients connected to the current board
 * to transfer itself to the new server. And then send all the lines
 * on this board to the new server. And at last will delete the board
 * from the current server. 
 * @pre xfarg should point to a valid XferWBArg object with sufficient
 * and correct information. And srq should point to a valid service
 * request struct. 
 * @post The designated board was transfered to the new server with
 * all the clients and lines properly set. And return -1 in case of
 * error. 
 * @param xfarg The XferWBArg struct used to transfer the board to new
 * server. 
 * @param srq service request struct. 
 * @return -1 on failure, and number of clients transfered on success. 
 */
int * transferwhiteboard_1_svc(XferWBArg *xfarg, struct svc_req *srq) {
  static int retval = 0 ;

#ifdef __DEBUG__
  fprintf(stdout, "Transfering %s To: %s . %x . %d.\n",
	  xfarg->boardnm, xfarg->machinenm,
	  xfarg->nprogram, xfarg->nversion); 
#endif 

  // find board with the given board name. 
  char bname[255]; 
  strcpy(bname, xfarg -> boardnm); 
  ABoard * wbp = find_wbp(bname); 
  //BBoard *b1;
  if(!wbp) {
    fprintf(stderr, "%s %s.\n", "No board named", bname); 
    goto error; 
  }

  // if white board exists, copy the board to a new object including
  // all the clients and lines and then print the name of clients
  // connected to this board.
  struct BBoard * bbd = (BBoard *) malloc(sizeof(BBoard)); 
  if(!bbd) goto error; 
  bbd->next = NULL; 
  bbd->lines = NULL; 
  bbd->clients = NULL; 
  //insert((ListNode **) &b1, (ListNode *) bbd);

  AClient *acp = wbp -> clients; // clients. 
  BClient *bcp;
  while(acp) {
    fprintf(stdout, "Client: %s - %x - %d.\n", 
	    acp->clientdata.machinenm, 
	    acp->clientdata.nprogram, 
	    acp->clientdata.nversion); 

    // tell the client to transfer to the new board. 
    retval = clienttransfer_1(xfarg, acp->callback); 

    // copy the client info to new struct. 
    bcp = (BClient *)malloc(sizeof(BClient)); 
    bcp->next = NULL; 
    if (!bcp) goto error;
    bcp->clientdata = acp->clientdata; 
    
    insert((ListNode **) & bbd->clients, (ListNode *) bcp);

    acp = acp->next; 
  }

  // copy the lines. 
  ALine * alp = wbp -> lines; // lines. 
  ALine * blp = NULL; 
  while(alp) {
    blp = (ALine *) malloc (sizeof(ALine)); 
    if (!blp) goto error; 
    //blp->next = NULL;
    blp->ln = alp->ln; 
    insert((ListNode **) & bbd->lines, (ListNode *) blp); 
    alp = alp->next; 
  }

  // After copying the to be transfered board to a new struct, then
  // send it to the new server. 
  sclp = clnt_create(xfarg->machinenm, xfarg->nprogram,
		     xfarg->nversion, "tcp"); 

  if(!sclp) {
    fprintf(stderr, "Can't create connection to new server on %s : %d.\n",
	    xfarg->machinenm, xfarg->nprogram); 
    goto error; 
  }
  retval = *(sendwbtonewserver_1(bbd, sclp));
  // delete board from current server. 
  delboard(wbp); 

  return &retval; 

 error:
  retval = -1; 
  return &retval ;
} // transfer white boards. 


/**
 * @brief Send the board to new server. 
 * @pre bdfromold should be properly set with valid value, and srq be
 * a pointer point to a valid service request struct. 
 * @post The board received from the old server be properly copied and
 * inserted into the board list. 
 * @param bdfromold Pointer to the BBoard structure that is from the
 * old server. 
 * @param srq the service request struct.
 * @return pointer to integer value, which is -1 on failure and
 * non-negative on success. 
 */
int * sendwbtonewserver_1_svc(BBoard *bdfromold, struct svc_req *srq) {
  static int retval = 0; 
  // after receiving the BBoard structure, copy it to new ABoard
  // truct and put it into the boards lists.
  struct ABoard * abd = (ABoard *) malloc (sizeof(ABoard)); 
  if(!abd) goto error; 
  //abd -> next = NULL;
  abd -> clients = NULL; 
  abd -> lines = NULL; 
  // at last, insert the new board in to the board list. 
  insert((ListNode **) & boards, (ListNode *) abd); 
  // copy the clients linked to this board.  
  BClient * bcp = bdfromold -> clients; 
  AClient * acp = NULL; 
  while(bcp) {
    acp = (AClient *) malloc (sizeof(AClient));
    if(!acp) goto error;
    acp -> next = NULL; 
    //acp -> callback = NULL;

    acp->clientdata = bcp->clientdata; 

    // re-create the callback function for the client struct. 
    acp->callback = clnt_create(bcp->clientdata.machinenm,
				bcp->clientdata.nprogram, 
				bcp->clientdata.nversion, "tcp"); 
#ifdef __DEBUG__ 
  fprintf(stdout, "Client on new server: %s . %s . %x . %d\n",
	  acp->clientdata.machinenm, acp->clientdata.boardnm, 
	  acp->clientdata.nprogram, acp->clientdata.nversion);
#endif 

    if(!acp->callback) {
      fprintf(stderr, "Error creating callback from the new server %s.\n",
	      bcp->clientdata.machinenm); 
      goto error; 
    }

    // insert the copied client into the client list in the board. 
    insert((ListNode **) &abd->clients, (ListNode *) acp); 

    bcp = bcp -> next; 
  }

  // copy the lines linked to the board. 
  ALine * blp = bdfromold -> lines; 
  ALine * alp = NULL;
  while(blp) {
    alp = (ALine *) malloc (sizeof(ALine)); 
    if(!alp) goto error; 
    alp -> next = NULL; 
    alp->ln = blp->ln; 

    insert((ListNode **) &abd->lines, (ListNode *) alp); 
    blp = blp->next; 
  }

  return &retval;

 error: 
  retval = -1; 
  return &retval; 
}

/* -eof- */
