package com.cloud.vista;

import java.awt.Color;
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
		float mean = td / _ps.size();
		float var = 0;
		
		//mean =100;
		for (int i = 0; i < _ps.size(); i++){
		    Point p = (Point) _ps.elementAt(i);
		    var += (p._dense - mean) * (p._dense - mean);
		}
	    
		var = (float) Math.sqrt(var / _ps.size());
		var = var / 25;
		
		//var = var *2;
		float dense=0;
		for (int i = 0; i < _ps.size(); i++) {
			Point p = (Point) _ps.elementAt(i);
			//dense = p._dense/md *2 -1;
			dense = (p._dense - mean) / var;
			
			// set the color of the point. 
			p._clr = renderPointColor(dense);
		} 
		
		//System.out.println( String.valueOf(mean) + " " + String.valueOf(var));
	}
    
	/**
	 * set the color of point displayed in the heat map. 
	 * @param val the density value of a point in the heat map. 
	 * 
	 * @return color of the point. 
	 */
	public Color renderPointColor(float val) {
	    val = -val;
	    float mid = (float) 0.4;
	    float vv;
	    vv = PApplet.abs((float)val);
	    
	    if(val < 0) {
		    val = mid - (mid * vv);
		} else {
		    val = mid + ((1-mid) * vv);
		}
	    float r=0;
	    float b=0;
	    float g=0;
	    if(val < 0){val = 0;}
	    if(val > 1){val = 1;}
	    //System.out.println(val);

	    switch ((int)(val/.2)) {
		case 0:
		    r = 255;
		    b = (float) (0 + (255*(val%.2)/.2));
		    g = 0;
		    break;
		case 1:
		    r = (float) (255 - (255*(val%.2)/.2));
		    b = 255;
		    g = 0;
		    break;
		case 2:
		    r = 0;
		    b = 255;
		    g = (float) (0 + (200*(val%.2)/.2));
		    break;
		case 3:
		    r = 100;
		    b = (float) (255 - (255*(val%.2)/.2));
		    g = (float) (150 + (105*(val%.2)/.2));
		    break;
		case 4:
		    r = (float) (105 - (105*(val%.2)/.2));
		    b = 0;
		    g = (float) (105 - (105*(val%.2)/.2));
		    break;
		case 5:
		    r = 0;
		    b = 0;
		    g = 25;
		    break;
		}
//	    System.out.println("Test." + Integer.toString((int) r) + " " + Integer.toString((int) g) + " " + Integer.toString((int) b));
	    return new Color((int)r, (int)g, (int)b);
	}
	
    /** count number of points in the map. 
     * @param xstart. 
     * @param ystart. 
     * @param xend. 
     * @param yend. 
     */
    int count(int xstart, int ystart, int xend, int yend) {
		int cnt = 0;
		for (int i = 0; i < _ps.size(); i++) {
		    Point p = (Point) _ps.elementAt(i);
		    int x = (int) (p._x * VistaExplorer.resol + VistaExplorer.viewx + 500);
		    int y = (int) (p._y * VistaExplorer.resol + VistaExplorer.viewy + 500);
		    
		    if (x >= xstart && x <= xend && y >= ystart && y <= yend)
		    	cnt += p._dense;
		}
		return cnt;    
    }  
}