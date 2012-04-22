package com.cloud.vista;

import java.awt.Color;

import processing.core.PApplet;

class Button extends PApplet {
    /**
	 * serial ID.
	 */
	private static final long serialVersionUID = 1L;
	int x, y;
    int w, h;
    Color basecolor, highlightcolor;
    Color currentcolor;
    boolean over = false;
    boolean pressed = false;   
  
    /**
     * Handling button pressed event.
     */
    void pressed() {
		if(over && mousePressed) {
		    pressed = true;
		} else {
		    pressed = false;
		}    
    }
  
    /**
     * Handling drawing rectangle event.
     * @param x x axis of the upper left corner.
     * @param y y axis of the upper left corner.
     * @param width width of the rectangle. 
     * @param height height of the rectangle.
     * @return true on valid rectangle and false otherwise.
     */
    boolean overRect(int x, int y, int width, int height) {
		if (mouseX >= x && 
			mouseX <= x + width && 
			mouseY >= y && 
			mouseY <= y + height) {
		    return true;
		} else {
		    return false;
		}
    }
}
