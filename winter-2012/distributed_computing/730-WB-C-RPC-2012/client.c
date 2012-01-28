
/* client.c */

#include "wb.h"

/*
 * A "white board client" is a pair of processes.  The forked child
 * receives server call-backs to callbackfromwbs() via its svc_run().
 * The parent process will handle the xwindow I/O.  These two
 * write/read via the pipe xwinio.
 */

static int parentid, childid;	/* process ids */
static int xwinio[2];		/* pipe: child writes, parent reads */
static AddLineArg me;		/* all the info there is about this client */
static CLIENT *clp;		/* librpc clnt_create()-ed */

/*
 * Terminate the client.  Remove all traces of the parent+child
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

/*
 * Get the call back from the server who is sending the coordinates of
 * a line to draw.  We simply write these four integers into the
 * xwinio pipe, and raise the signal so the parent can read.
 */
void *callbackfromwbs_1_svc(OneLn * p)
{
  static int i = 0;		/* note: static */

  write(xwinio[1], p, sizeof(OneLn));
  kill(parentid, SIGUSR1);	/* kill == "raise" */
  return (void *) &i;
}

/*
 * Invoked via callbackfromwbs/SIGUSR1 when a new line comes from the
 * server to the svc_run()-ning process.
 */
static void readndraw(int unused)
{
  OneLn lc;

  (void) read(xwinio[0], &lc, sizeof(lc));
  drawline(&lc);
}

/*
 * Client window got exposed.  Redraw the lines.
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

/*
 * Watch for mouse input.  Button 3 (right) ends this routine.
 * Pressing buttons 1 (left) or 2 (middle) sends the line to the
 * server who will distribute it to all member white boards.
 */
static void mousewatch(CLIENT * clp)
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

/*
 * Called by client_s.c.  See ./ed-script. Start the client.
 */
void startclient
    (int nprogram, int nversion,
     char *servermcnm, char *boardnm, char *xdisplaynm, char *pmcolor) {
  /* clients own details -- once set, these do not change */
  me.clientdata.color = atoir(pmcolor, 16);
  me.clientdata.nprogram = nprogram;
  me.clientdata.nversion = nversion;
  gethostname(me.clientdata.machinenm, sizeof(me.clientdata.machinenm));
  strcpy(me.clientdata.boardnm, boardnm);
  strcpy(me.clientdata.xdisplaynm, xdisplaynm);
  strcat(me.clientdata.xdisplaynm, ":0.0");

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
    struct sigaction asigterm, asiguser;
    asigterm.sa_handler = endtheclient;
    asigterm.sa_flags = 0;
    sigemptyset(&asigterm.sa_mask);
    sigaction(SIGTERM, &asigterm, 0);

    asiguser.sa_handler = readndraw;
    asiguser.sa_flags = 0;
    sigemptyset(&asiguser.sa_mask);
    sigaction(SIGUSR1, &asiguser, 0);
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

/* -eof- */
