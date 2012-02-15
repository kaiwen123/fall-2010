// file: LineCoords.java by pmateti@wright.edu

package WhiteBoard;

public class LineCoords implements java.io.Serializable {
    int x1, y1, x2, y2;		// the two end points of a line
    java.awt.Color c;

    public String toString() {
	return "(" + x1 + ", " + y1 + ", " + x2 + ", " + y2 + ", " + c + ")" ;
    }
} 

// -eof-
