// file: WbServer.java

package WhiteBoard;

import java.util.*;

public interface WbServer extends java.rmi.Remote {
    void addClient(WbClient wc, String brnm) throws java.rmi.RemoteException;
    void delClient(WbClient wc, String brnm) throws java.rmi.RemoteException;
    void addLine(LineCoords ln, String brnm) throws java.rmi.RemoteException;
    void sendAllLines(WbClient wc, String brnm) throws java.rmi.RemoteException;
    Vector <ABoard> query() throws java.rmi.RemoteException; 
    boolean newserver(String ServName, String location) throws java.rmi.RemoteException; 
    void transfer(String brdnm, String newservUrl) throws java.rmi.RemoteException;
    void pushtonewserver(ABoard ab) throws java.rmi.RemoteException;
}

// -eof-
