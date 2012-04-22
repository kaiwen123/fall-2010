package com.cloud.vista;

import java.util.Vector;
import processing.core.PApplet;

class View extends PApplet { 
    /**
	 * serial ID.
	 */
	private static final long serialVersionUID = 1L;
	public int _vid;
    public Vector<Point> _ps;
    
    //public ImageButtons button;
    public View(int vid) {
		_vid = vid;
		_ps = new Vector<Point>();
    }
   
    public void addPoint(Point pt) {
    	_ps.addElement(pt);
    }
    
    /**
     * Normalize number of points (density) so that it can be displayed. 
     * @param md
     * @param td
     */
    void normalize(float md, float td) {
		float mean = td/_ps.size();
		float var = 0;
		
		//mean =100;
		for (int i=0; i<_ps.size(); i++){
		    Point p =(Point) _ps.elementAt(i);
		    var += (p._dense-mean)*(p._dense-mean);
		}
	    
		var = (float) Math.sqrt(var/_ps.size());
		var = var/25;
		
		//var = var *2;
		float dense=0;
		for (int i = 0; i < _ps.size(); i++) {
			Point p =(Point) _ps.elementAt(i);
			//dense = p._dense/md *2 -1;
			dense = (p._dense-mean)/var;
			p._clr = VistaExplorer.myMap(dense);
		} 
		
		//System.out.println( String.valueOf(mean) + " " + String.valueOf(var));
	}
	
    /** count number of points. 
     * @param xstart. 
     * @param ystart. 
     * @param xend. 
     * @param yend. 
     */
    int count(int xstart, int ystart, int xend, int yend) {
		int cnt=0;
		for (int i = 0; i < _ps.size(); i++) {
		    Point p =(Point) _ps.elementAt(i);
		    int x = (int) (p._x * VistaExplorer.resol + VistaExplorer.viewx + 500);
		    int y = (int) (p._y * VistaExplorer.resol + VistaExplorer.viewy + 500);
		    
		    if (x >=xstart && x<=xend && y >=ystart && y<=yend)
		    	cnt+=p._dense;
		}
		return cnt;    
    }  
}