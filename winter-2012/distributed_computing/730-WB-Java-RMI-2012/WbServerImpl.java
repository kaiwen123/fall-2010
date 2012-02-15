// file: WbServerImpl.java   by pmateti@wright.edu

package WhiteBoard;

import java.io.*;
import java.util.*;
import java.rmi.*;
import java.rmi.server.*;

class ABoard {
    String boardName;		// Name of this board
    Vector <LineCoords> vLines;	// all lines on this board
    Vector <WbClient> vClients;	// all clients on this board

    public ABoard(String brdnm) {
	boardName = brdnm;
	vLines = new Vector <LineCoords> ();
	vClients = new Vector <WbClient> ();
    } 
}

public class WbServerImpl
    extends UnicastRemoteObject
    implements WbServer {

    private Vector <ABoard> vBoards; // all boards on this server
    private String myURL;

    public WbServerImpl(String [] args) throws Exception {
	// args = [serverID, serverMcnm]
	vBoards = new Vector <ABoard> ();
	myURL = Invoke.makeURL('S', args[0]);
	Naming.rebind(myURL, this); // rmi register ourselves
	Invoke.myPrint("WbServerImpl", myURL + " started");
    }

    private void pleaseDie() {
	int delay = 5000;	// in msec, delayed death
	Timer timer = new Timer();
	timer.schedule( new TimerTask(){
		public void run() {
		    try {Naming.unbind(myURL);}
		    catch (Exception e) {e.printStackTrace();}
		    Invoke.myPrint("WbServerImpl", myURL + " exits");
		    System.exit(0);
		}
	    }, delay);
    }

    private ABoard findAboard(String brdnm) {
	for (Enumeration e = vBoards.elements(); e.hasMoreElements();) {
	    ABoard b = (ABoard) e.nextElement();
	    if (brdnm.equals(b.boardName))
		return b;
	}
	return null;
    }

    public void sendAllLines(WbClient wc, String brdnm)
	throws java.rmi.RemoteException    {
	ABoard ab = findAboard(brdnm);
	sendAllLines(wc, ab);
    }

    private void sendAllLines(WbClient wc, ABoard ab) {
	for (Enumeration e = ab.vLines.elements(); e.hasMoreElements(); ) {
	    try {wc.updateBoard((LineCoords) e.nextElement());}
	    catch (Exception x) {x.printStackTrace();}
	}
    }

    public void addClient(WbClient wc, String brdnm)
	throws java.rmi.RemoteException    {
	ABoard ab = findAboard(brdnm);
	if (ab == null) {
	    ab = new ABoard(brdnm);
	    vBoards.addElement(ab);
	} else {
	    sendAllLines(wc, ab); // new client on an old board
	}
	ab.vClients.addElement(wc);
    }
 
    public void delClient(WbClient wc, String brdnm) 
	throws java.rmi.RemoteException    {
	ABoard ab = findAboard(brdnm);
	if (ab == null) return;
	     
	ab.vClients.removeElement(wc);

	// If this is the last client in board, delete board  
	if (ab.vClients.size() == 0) vBoards.removeElement(ab);

	// If this was the last board, terminate this server
	if (vBoards.size() == 0) pleaseDie();
    }
  	 	
    public void addLine(LineCoords ln, String brdnm)
	throws java.rmi.RemoteException    {
	ABoard ab = findAboard(brdnm);
	if (ab == null) return;

	ab.vLines.addElement(ln);

	// Broadcast to all the clients on this board
	for (Enumeration e = ab.vClients.elements(); e.hasMoreElements();) {
	    WbClient wc = (WbClient) e.nextElement();
	    try {wc.updateBoard(ln);}
	    catch (Exception x) {x.printStackTrace();}
	}  
    }

    public static void main(String args[]) {
	try { WbServerImpl wsi = new WbServerImpl(args);}
	catch (Exception e) {e.printStackTrace();}
    }
}

// -eof-
