package com.cloud.vista;

import processing.core.*;
import java.awt.Color;

class Point extends PApplet {
    /**
	 * Serial ID.
	 */
	private static final long serialVersionUID = 1L;
	public float m_pointX, m_pointY;
    public float m_pointDensity;
    public Color m_pointColor = new Color(1,2,3);  
    
    /**
     * Point with density.
     * @param m_upperLeftX the m_upperLeftX axis of the point.
     * @param m_upperLeftY the m_upperLeftY axis of the point.
     * @param dense how many points are in this location?
     */
    public Point(String x, String y, String dense) {
		m_pointX = Float.parseFloat(x);
		m_pointY = Float.parseFloat(y);
		m_pointDensity = Float.parseFloat(dense);  
    }
    
    public float getPointX() { return m_pointX; }
    public float getPointY() { return m_pointY; }
    public float getDensity() { return m_pointDensity; }
    public Color getpointColor() { return m_pointColor; }
}
