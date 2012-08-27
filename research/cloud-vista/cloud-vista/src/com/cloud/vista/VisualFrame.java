package com.cloud.vista;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.Vector;
import processing.core.PApplet;

class VisualFrame extends PApplet { 
    /**
	 * serial ID.
	 */
	private static final long serialVersionUID = 1L;
	public int m_visualFrameID;
    public Vector<Point> m_visualFramePoints;
    
    /**
     * The heat map visualization of the visual frames. 
     * @param vid the id of this view, usually determined by the 
     * id of the visual frame. 
     * @return none. 
     */
    public VisualFrame(int vid) {
		m_visualFrameID = vid;
		m_visualFramePoints = new Vector<Point>();
    }
   
    public void addPoint(Point pt) {
    	m_visualFramePoints.addElement(pt);
    }
    
    public int getVisualFrameID() {
    	return m_visualFrameID; 
    }
    
    public void setVisualFrameID(int vid) {
    	m_visualFrameID = vid; 
    }
    
    /**
     * Normalize number of points (density) so that it can be displayed. 
     * @param md
     * @param td
     */
    void normalize(float md, float td) {
		float mean = td / m_visualFramePoints.size();
		float var = 0;
		
		//mean =100;
		for (int i = 0; i < m_visualFramePoints.size(); i++){
		    Point p = (Point) m_visualFramePoints.elementAt(i);
		    var += (p.m_pointDensity - mean) * (p.m_pointDensity - mean);
		}
	    
		var = (float) Math.sqrt(var / m_visualFramePoints.size());
		var = var / 25;
		
		//var = var *2;
		float dense=0;
		for (int i = 0; i < m_visualFramePoints.size(); i++) {
			Point p = (Point) m_visualFramePoints.elementAt(i);
			//dense = p._dense/md *2 -1;
			dense = (p.m_pointDensity - mean) / var;
			
			// set the color of the point. 
			p.m_pointColor = renderPointColor(dense);
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
		for (int i = 0; i < m_visualFramePoints.size(); i++) {
		    Point p = (Point) m_visualFramePoints.elementAt(i);
		    int x = (int) (p.m_pointX * VistaExplorer.m_resolution + VistaExplorer.m_viewX + 500);
		    int y = (int) (p.m_pointY * VistaExplorer.m_resolution + VistaExplorer.m_viewY + 500);
		    
		    if (x >= xstart && x <= xend && y >= ystart && y <= yend)
		    	cnt += p.m_pointDensity;
		}
		return cnt;    
    }
   
}