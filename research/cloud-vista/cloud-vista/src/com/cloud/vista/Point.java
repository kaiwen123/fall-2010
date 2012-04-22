package com.cloud.vista;

import processing.core.*;
import java.awt.Color;

class Point extends PApplet {
    /**
	 * Serial ID.
	 */
	private static final long serialVersionUID = 1L;
	public float _x, _y;
    public float _dense;
    //public color  _clr;
    public Color _clr = new Color(1,2,3);  
    
    /**
     * Point with density.
     * @param x the x axis of the point.
     * @param y the y axis of the point.
     * @param dense how many points are in this location?
     */
    public Point(String x, String y, String dense){
		_x = Float.parseFloat(x);
		_y = Float.parseFloat(y);
		_dense = Float.parseFloat(dense);  
    }
}
