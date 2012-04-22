package com.cloud.vista;

import processing.core.*;

/**
 * The image button class.
 */
class ImageButtons extends Button {
    /**
	 * Serial ID.
	 */
	private static final long serialVersionUID = 1L;
	PImage base;
    PImage roll;
    PImage down;
    PImage currentimage;

    ImageButtons(int ix, int iy, int iw, int ih, PImage ibase, PImage iroll, PImage idown) {
		x = ix;
		y = iy;
		w = iw;
		h = ih;
		base = ibase;
		roll = iroll;
		down = idown;
		currentimage = base;
    }
  
    void update() {
		over();
		pressed();
		if(pressed) {
		    currentimage = down;
		} else if (over) {
		    currentimage = roll;
		} else {
		    currentimage = base;
		}
    }
  
    void over() {
		if (overRect(x, y, w, h)) {
		    over = true;
		} else {
		    over = false;
		}
    }
  
    void display() {
    	image(currentimage, x, y);
    }
}

