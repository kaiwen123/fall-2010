//ABoard object

package WhiteBoard;

import java.io.*;
import java.util.*;
import java.rmi.*;
import java.rmi.server.*;

public class ABoard implements Serializable{
    String boardName;		// Name of this board
    Vector <LineCoords> vLines;	// all lines on this board
    Vector <WbClient> vClients;	// all clients on this board

    public ABoard(String brdnm) {
	boardName = brdnm;
	vLines = new Vector <LineCoords> ();
	vClients = new Vector <WbClient> ();
    } 
}