/**
 * @file client.c The client program, which is responsible for
 * managing all the lines on the board, if there are any changes from
 * the server, it will responsed accordingly, such as the transfer of
 * itself to the new server (actually to reregister itself). 
 * @version 1.0 
 * @author Shumin Guo. 
 */

#include "wb.h"

/**
 * @brief A "white board client" is a pair of processes.  The forked child
 * receives server call-backs to callbackfromwbs() via its svc_run().
 * The parent process will handle the xwindow I/O.  These two
 * write/read via the pipe xwinio.
 */
static int transfer(int unused); /* transfer client to new server. */
static int parentid, childid;	/* process ids */
static int xwinio[2];		/* pipe: child writes, parent reads */
static AddLineArg me;		/* all the info there is about this client */
static CLIENT *clp;		/* librpc clnt_create()-ed */

/**
 * @brief Terminate the client.  Remove all traces of the parent+child
 * @pre client struct object me should be properly set with correct
 * information about the client to terminate. 
 * @post client is ended and the rpc map reset.
 * @param unused dummy integer. 
 * @return void.
 */
static void endtheclient(int unused)
{
  delclient_1(&me.clientdata, clp); /* ask server to delete me */
  clnt_destroy(clp);		/* CLIENT structure */
  pmap_unset(me.clientdata.nprogram, me.clientdata.nversion);
  closexwindow();
  kill(childid, SIGTERM);
  exit(0);
}

/**
 * @brief Get the call back from the server who is sending the
 * coordinates of a line to draw.  We simply write these four integers
 * into the xwinio pipe, and raise the signal so the parent can read.
 * @pre pointer to OneLn struct should be properly set with valid
 * data.
 * @post the client got the line info from the server and was notified
 * by a USR signal. and integer pointer to integer with 0 was
 * returned. 
 * @param p Pointer to the OneLn structure. 
 * @return void. 
 */
void *callbackfromwbs_1_svc(OneLn * p)
{
  static int i = 0;		/* note: static */

  write(xwinio[1], p, sizeof(OneLn));
  kill(parentid, SIGUSR1);	/* kill == "raise" */
  return (void *) &i;
}

/**
 * @brief Invoked via callbackfromwbs/SIGUSR1 when a new line comes
 * from the server to the svc_run()-ning process.
 */
static void readndraw(int unused)
{
  OneLn lc;

  (void) read(xwinio[0], &lc, sizeof(lc));
  drawline(&lc);
}

/**
 * @brief Client window got exposed.  Redraw the lines.
 * @pre window was exposed clp point to a correct CLIENT object that
 * identifies the client associated with the window exposure event.
 * @post lines belonging to this whiteboard be drawn and displayed. 
 * @param clp client pointer. 
 * @return void. 
 */
static void exposedwindow(CLIENT * clp)
{
  Linep p, *q = sendallmylines_1(&me.clientdata, clp);
  int n = 0;

  if (q == NULL)
    return;
  for (p = *q; p; p = p->next) {
    drawline(&p->ln);
    n++;
  }
}

/**
 * @brief Watch for mouse input.  Button 3 (right) ends this routine.
 * Pressing buttons 1 (left) or 2 (middle) sends the line to the
 * server who will distribute it to all member white boards.
 * @pre the user interface, and the signal handling procedures
 * properly set. And a mouse button was clicked. 
 * @post mouse events properly handled with registered handler. Or
 * simply dropped if no handlers were registered for some events. 
 * @param clp pointer to the client struct. 
 * @return void. 
 */
static void mousewatch()
{
  int btn = 5;

  for (;;)
    switch (btn) {
    case 1:
    case 2:
      me.ln.color = me.clientdata.color;
      addline_1(&me, clp);
      btn = 0;
      break;
    case 3:
      return;		/* <== */
    case 5:
      exposedwindow(clp);
      btn = 0;
      break;
    default:
      btn = trackpointer(&me.ln, 0);
      break;
    }
}

/**
 * @brief Called by client_s.c.  See ./ed-script-client. Start the
 * client.
 * @pre all the arguments passed to this function were properly set
 * with valid value. 
 * @post client specified with given parameter was started. 
 * @param nprogram the program number used by the client.
 * @param nversion the version number of the client .
 * @param servermcnm the name of the server machine. 
 * @param boardnm the name of the board that this client will be
 * associated with. 
 * @param xdisplaynm the x server location where the user interface
 * window will be displayed. 
 * @param pmcolor the color of the display color.
 * @return void. 
 */
void startclient (int nprogram, int nversion,
		  char *servermcnm, char *boardnm, char *xdisplaynm, char *pmcolor) {
  /* clients own details -- once set, these do not change */
  me.clientdata.color = atoir(pmcolor, 16);
  me.clientdata.nprogram = nprogram;
  me.clientdata.nversion = nversion;
  gethostname(me.clientdata.machinenm, sizeof(me.clientdata.machinenm));
  strcpy(me.clientdata.boardnm, boardnm);
  strcpy(me.clientdata.xdisplaynm, xdisplaynm);
  strcat(me.clientdata.xdisplaynm, getenv("DISPLAY"));

  char xwintitle[100];
  sprintf(xwintitle, "%s@%s color=%lx",
	  boardnm, me.clientdata.machinenm, me.clientdata.color);

  clp = clnt_create
    (servermcnm, WhiteBoardServer, WhiteBoardServerVersion, "tcp");
  if (!clp) {
    fprintf(stderr,
	    "client730: clnt_create(%s,0x%x,0x%x,%s) failed.\n",
	    servermcnm, WhiteBoardServer, WhiteBoardServerVersion, "tcp");
    exit(1);
  }

  if (pipe(xwinio) == -1) {
    fprintf(stderr, "client730: xindow io pipe failed.\n");
    exit(2);
  }

  childid = fork();
  if (childid == -1) {
    fprintf(stderr, "client730: fork was unsuccessful.\n");
    exit(3);
  }
  if (childid == 0) {
    /* the child process */
    close(xwinio[0]);
    parentid = getppid();
    return;			/* child returns to do svc_run() */
  }

  /* parent process continues */

  {				/* setup signal handling */
    struct sigaction asigterm, asiguser, asigtrans;
    asigterm.sa_handler = endtheclient;
    asigterm.sa_flags = 0;
    sigemptyset(&asigterm.sa_mask);
    sigaction(SIGTERM, &asigterm, 0);

    asiguser.sa_handler = readndraw;
    asiguser.sa_flags = 0;
    sigemptyset(&asiguser.sa_mask);
    sigaction(SIGUSR1, &asiguser, 0);

    // register to respond to transfer to new server.
    asigtrans.sa_handler = transfer;
    asigtrans.sa_flags = 0;
    sigemptyset(&asigtrans.sa_mask);
    sigaction(SIGUSR2, &asigtrans, 0);
  }

  close(xwinio[1]);
  int x = openxwindow(me.clientdata.xdisplaynm, xwintitle);
  if (x < 0) {
    fprintf(stderr, "client730: openxwindow(%s, %s) == %d, failed\n",
	    me.clientdata.xdisplaynm, xwintitle, x);
    exit(4);
  }

  addclient_1(&me.clientdata, clp);
  mousewatch(clp);	    /* returns only when button3 is clicked */
  endtheclient(0);
}

/**
 * @brief Transfer the client to the newly created server by the old
 * server. What the old server provides is the location of the new
 * machine and the program number for the new machine. And what the
 * client does is actually deregister with the old server and register
 * itself with the new server. 
 * @param targ client transfer argument. 
 * @param srq service request struct. 
 * @return -1 on failure and > 0 otherwise. 
 */
int * clienttransfer_1_svc(XferWBArg *targ, struct svc_req *srq) {
  static int retval = 0; 

#ifdef __DEBUG__ 
  fprintf(stdout, "clienttransfer: %s . %s . %x . %d\n",
	  targ->machinenm, targ->boardnm, 
	  targ->nprogram, targ->nversion);
#endif 

  write(xwinio[1], targ, sizeof(XferWBArg));

  // when this was called by the old server, current process should
  // tell the child client to reregister to the new server. 
  kill(parentid, SIGUSR2); 

  return &retval; 
}

/**
 * @brief The transfer operation on the client side. Actually the
 * client will update the its own client pointer by recreating the
 * connection to the new server using clnt_create rpc call. 
 * @pre unused has a valid value and the pipe named xwinio should be
 * filled with the new server information corrected. 
 * @post client connectioin to the new server should be updated so
 * that the afterward operations on the client should use the new
 * server rather than the old server. 
 * @param unused a dummy integer which is not used in the function. 
 * @return -1 on failure and 0 on success.
 */
static int transfer(int unused) {
  XferWBArg targ; 
  (void) read(xwinio[0], &targ, sizeof(XferWBArg));
  clnt_destroy(clp);

  clp = clnt_create
    (targ.machinenm, targ.nprogram, targ.nversion, "tcp");
#ifdef __DEBUG__
  fprintf(stdout, "Local client info: %s . %x . %d.\n",
	  me.clientdata.machinenm, me.clientdata.nprogram,
	  me.clientdata.nversion); 
#endif 

  if (!clp) {
    fprintf(stderr, "client730: clnt_create to %s failed %s : %d.\n", 
	    targ.machinenm, __FILE__, __LINE__);
    goto error;
  }

  printf("Client on %s with was transfered to server %s.\n", 
	 me.clientdata.machinenm, targ.machinenm); 

  return 0; 

 error: 
  return -1; 
}
/* -eof- */
