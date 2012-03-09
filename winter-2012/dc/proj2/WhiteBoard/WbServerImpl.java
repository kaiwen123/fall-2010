// file: WbServerImpl.java   by pmateti@wright.edu

package WhiteBoard;

import java.io.*;
import java.util.*;
import java.rmi.*;
import java.rmi.server.*;

class ABoard implements Serializable {
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

    Vector <ABoard> vBoards; // all boards on this server
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

    // send all lines to the client. 
    public void sendAllLines(WbClient wc, String brdnm)
	throws java.rmi.RemoteException    {
	ABoard ab = findAboard(brdnm);
	sendAllLines(wc, ab);
    }
    
    /**
     * Send all line sof the given board to the given client. 
     * @pre wc is a proper WbClient object with all the values
     * properly set, and ab is an good ABoard object. 
     * @post Lines on the given board was sent to the given
     * WbClient object wc. 
     */
    private void sendAllLines(WbClient wc, ABoard ab) {
	for (Enumeration e = ab.vLines.elements(); e.hasMoreElements(); ) {
	    try {wc.updateBoard((LineCoords) e.nextElement());}
	    catch (Exception x) {x.printStackTrace();}
	}
    }

    /**
     * Add client to the server. 
     * @pre wc was a proper WbClient object and brdnm is a string with
     * value properly set. 
     * @post If named board does not exist, then add one new board
     * with given name, and add the client to the new board; if named
     * board already exists, and in the end, send all lines to the
     * client.
     */
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

    /**
     * Delete client from board. 
     * @pre wc was a proper WbClient Object with all values properly
     * set; brdnm is a valid string. 
     * @post wc is deleted from the server if the client is connected
     * to the named existing board, else if board does not exist,
     * nothing will be done; and the named board will be deleted if no
     * client is connected to the board any more; and if there is no
     * board in the server, the server will die on itself. 
     */
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
    
    /**
     * Add a line to the named board. 
     * @pre ln is a proper LineCoords object with valid coordinates,
     * and brdnm is a proper string. 
     * @post ln is added to the named board if board exists and
     * nothing will be done if board does not exist. And all the
     * clients connected to the named board will be notified to add
     * the line to its window.
     */
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

    /****************************************************
     * query the server. will be called by the client wbadmin. 
     * @pre wc is and valid WbClient object. 
     * @post query was done and a new server object with all the board
     * , client and line information was returned to the caller. 
     */
    public Vector <ABoard> query() {
	return vBoards; 	
    }

    /**
     * Create new server on another machine. 
     * @pre wc is a valid WbClient object and url string contains a valid url address. 
     * @post new server was create successfully or exception happened. 
     */
    public boolean newserver(String ServName, String location) {
	String sshcmd = "ssh -f " + location + " java -cp /home/ceg730/ceg73005/proj2/WhiteBoard WhiteBoard.WbServerImpl " + ServName + " " + location; 
	try { 
	    System.out.println("SSH COMMAND: " + sshcmd); 
	    Runtime.getRuntime().exec(sshcmd);
	}
	catch (Exception e) { e.printStackTrace(); return false; }
	return true;
    }

    /** 
     * transfer board to the new server. 
     * @pre ab is and good ABoard object, newservUrl is a valid URL
     * string pointing to the new server.
     * @post given ABoard was transfered to the new white board
     * server if server exists or fail if new server does not exist.
     */
    public void transfer(String brdnm, String newservUrl) {
	ABoard ab = findAboard(brdnm); 
	if(ab == null) return; 

	try { 
	    WbServer snew = (WbServer) Naming.lookup(newservUrl); 
	    //snew.
	    System.out.println("transfer board to new server: " + newservUrl); 
	    snew.pushtonewserver(ab); 
	    
	    // tell all the client about the changes. 
	    Iterator <WbClient> iter = ab.vClients.iterator(); 
	    while(iter.hasNext()) {
		iter.next().updateServer(newservUrl); 
	    }

	    vBoards.removeElement(ab); 
	} catch (Exception e) { e.printStackTrace(); }
    }

    /**
     * Push the client to the new server. 
     * @pre url Valid server url. 
     * @post The given board was associated with the new server. 
     * @param ab the board to the be pushed to the new server.
     * @param url the new server url. 
     * @return void. 
     */
    public void pushtonewserver(ABoard ab) {
	try { vBoards.addElement(ab); }
	catch (Exception e) { e.printStackTrace(); }
    }
 
    public static void main(String args[]) {
	try { 
	    WbServerImpl wsi = new WbServerImpl(args);
	} catch (Exception e) {e.printStackTrace();}
    }
}

// -eof-
