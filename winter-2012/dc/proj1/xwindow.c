/*
 * xwindow.c
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

/* Following typedef is compatible with SR's linecoords rec */
typedef struct {
    int x1, y1, x2, y2, color;
} LineCoords;

typedef struct {
    Display *display;
    Window window;
    GC gc;
} Xwindow;


static Xwindow xw;

/*
 * Open an X window on display screen whose name is given in dispnm[]; the
 * title of the window will be wintitle[].  The private var xw of this module
 * gets initialized as a result.  The window so created will appear as soon
 * as trackpointer() is called.  dispnm[] is of the form "machinename:0.0",
 * wintitle[] is any arbitrary C string of non-control chars.
 */
int openxwindow(char dispnm[], char wintitle[])
{
  // printf("%s\n", dispnm);	       /*  */
  // strcpy(dispnm, ":0.0"); 
    Display *d = XOpenDisplay(dispnm);	/* connect to X */
    int screen;
    int dw, dh;
    XGCValues values;

    if (d == NULL) {
	/* "can't connect to X server; was dispnm bad? */
	return -1;
    }
    xw.display = d;

    /* get screen size from display structure macro */
    screen = DefaultScreen(d);
    dw = DisplayWidth(d, screen);
    dh = DisplayHeight(d, screen);

    /* create opaque window */
    xw.window = XCreateSimpleWindow
	(d, RootWindow(d, screen),
	 dw / 3, dh / 3, 350, dh / 4, 4,
	 BlackPixel(d, screen), WhitePixel(d, screen));

    /* set window/icon titles */
    XSetStandardProperties
	(d, xw.window, wintitle, "Doodle", None, 0, 0, 0);

    /* Select event types wanted; 0x00FFFFFF selects all! */
    XSelectInput(xw.display, xw.window, 0x00FFFFFF);

    /* Display window */
    XMapWindow(d, xw.window);

    /* Create Graphics Context */
    xw.gc = XCreateGC(d, xw.window, 0L, &values);

    /* Set Foreground color to black */
    XSetForeground(d, xw.gc, BlackPixel(d, screen));

    return 0;
}

/*
 * Close the X window opened by openxwindow().
 */
int closexwindow(void)
{
    return XCloseDisplay(xw.display);
}

/*
 * Draw a straight line from (coord.x1, coord.y1) to (coord.x2, coord.y2).
 */
void drawline(LineCoords * coord)
{
    XSetForeground(xw.display, xw.gc, coord->color);
    XDrawLine(xw.display, xw.window, xw.gc,
	      coord->x1, coord->y1, coord->x2, coord->y2);
    XFlush(xw.display);
}

/*
 * "Track" the mouse ptr of the X window server to which we established
 * connection via openxwindow().  The routine does *not* draw an individual
 * line (unless "exposed"), *nor* does it return until the mouseptr (a)
 * enters our window from outside, (b) leaves our window, or (c) mouse ptr is
 * inside the window, and one of the three mouse buttons is pressed.  Events
 * (a) and (b) return 0, event (c) returns the number of the button 1..3.
 * The routine remembers upto 2000 points, which is a hack.  When our window
 * gets (partially) hidden, and later revealed again (i.e., "exposed"), the
 * routine draws the lines it had (subject to this limit).  Ofcourse, the
 * routine assumes you properly opened a window via openxwindow().
 */
int trackpointer(LineCoords * newseg, int pending)
{
    XEvent report;
    Window rootw, childw;
    int rootx, rooty, wx, wy;
    unsigned int keys_buttons;
    static XPoint points[2000] = { {0}, {0} };
    static int index = 0, npts = 0;

    if (pending && !XPending(xw.display)) {
	return 0;
    }

    XNextEvent(xw.display, &report);
    switch (report.type) {
    case EnterNotify:
	return 6;

    case LeaveNotify:
	return 7;

    case ButtonPress:
	if (report.xbutton.button == 3)
	    return 3;

	/* get rid of any other motion events still on queue */
	while (XCheckMaskEvent(xw.display,
			       PointerMotionMask | ButtonMotionMask,
			       &report)
	    );

	/* now get current position */
	XQueryPointer
	    (xw.display, report.xmotion.window,
	     &rootw, &childw, &rootx, &rooty, &wx, &wy, &keys_buttons);

	newseg->x1 = points[index].x;
	newseg->y1 = points[index].y;

	index++;
	if (index >= 2000)
	    index = 0;
	else
	    npts = index;
	newseg->x2 = points[index].x = wx;
	newseg->y2 = points[index].y = wy;
	return report.xbutton.button;


    case Expose:
	/* XDrawLines
	   (xw.display, xw.window, xw.gc,
	   points, npts, CoordModeOrigin); */
	return 5;
    }
    return 0;
}

/* 
 * Convert string p of digits given in radix r into a long integer.
 * Typical r values are 10 and 16.  When r==16, do not give 0x prefix.
 * Our own because there is no stdlib one; but see strtoq()
 */
long atoir(char *p, int r)
{
  if (p == NULL) return 0;
  long x = 0L, d;
  char c, sign = '+';
  if (*p == '-')    {
    sign = '-';
    p++;
  }
  while((c = *p++)) {
    d = r;
    if (r == 16) {
      if ('A' <= c && c <= 'F') d = (long)(c + 10 - 'A');
      if ('a' <= c && c <= 'f') d = (long)(c + 10 - 'a');
    }
    if ('0' <= c && c <= '9') d = (long)(c - '0');
    if (d >= r) break;
    x = x * r + d;
  }
  if (sign == '-') x = -x;
  return x;
}

#ifdef  MAIN
#include <stdio.h>

/*
 * A little test harness.  Invoke the resulting program with arguments:
 * (1) name of the display screen (e.g., "diamond:0.0"), and (2) a
 * name that shows up in the title-bar of the window (e.g., "hello"),
 * (3) RGB color value in two hex digits for each. Also try illegal
 * args.
 */
int main(int argc, char *argv[], char **envp)
{
    LineCoords seg = { 0, 0, 100, 100, 0 };


    if (argc < 4) {
	fprintf(stderr, 
		"usage: %s <display-hostname> <color-number> <window-title>\n",
		argv[0]);
    }
    seg.color = atoir(argv[3], 16);
    if (seg.color < 0)
      seg.color = 0;
		     

    int b = openxwindow(argv[1], argv[2]);
    printf("openxwindow(%s, %s, %s) returned %d\n",
	   argv[1], argv[2], argv[3], b);

    printf("press button 3 (Right) to terminate; color %d", seg.color);

    do {
	b = trackpointer(&seg, 0);
	printf("line from (%2d, %2d) to (%2d, %2d) button %d\n",
	       seg.x1, seg.y1, seg.x2, seg.y2, b);
	drawline(&seg);
    } while (b != 3);
    return closexwindow();
}
#endif

/* -eof- */
