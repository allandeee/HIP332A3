// Hipster cellular automaton language.
//
// Simple one dimensional automaton, presented as a 2-dimensional
// progression down the screen.
//
// This example shows that we could do with simplifying things by
// adding a few more constructs to the Hipster language. How about
// a switch statement.
//
// © 2017, Dominic Verity, Macquarie University, All rights reserved.
// 
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// Constants giving the sizes of the dimensions.
int width = 402;
int height = 200;

// Constants determining the specific one-dimensional automaton to
// execute. These choices give the Sierpinski gasket.
boolean a0 = false;
boolean a1 = true;
boolean a2 = false;
boolean a3 = true;
boolean a4 = true;
boolean a5 = false;
boolean a6 = true;
boolean a7 = false;

// Automaton dimension
dimension(width + 1, height + 1);

// The neighbours of a cell are the three cells immediately above it.
neighbourhood L = [-1,1], C = [0,1], R = [1,1];

// A cell can be either alive or dead.
state {
  boolean alive = false;
}

// Updater. There are 8 possibilities for the conbination of states
// of the three neighbours of the current cell. For each such combination
// we set the state of the current cell in the next generation to the
// value of one of the constants a0...a7.
updater {
  if L:alive then {
    if C:alive then {
       if R:alive then {
         alive = a7;
       } else {
         alive = a6;
       }
    } else {
       if R:alive then {
         alive = a5; 
       } else {
         alive = a4;
       }
    }
  } else {
    if C:alive then {
       if R:alive then {
         alive = a3;
       } else {
         alive = a2;
       }
    } else {
       if R:alive then {
         alive = a1;
       } else {
         alive = a0;
       }
    }
  }
}

// Colour constants
int white = 0xFFFFFF;
int cyan = 0x00FFFF; 

// Dead cells displayed in white, live cells in cyan.
mapper {
  if alive then return(cyan); else return(white);
}

// Initialiser, set a single point at the top of the screen,
// but just outside of the world boundary, to alive.
// Since this cell is outside the boundary it won't get changed.
// But when this initialiser is selected nothing will change on
// on the screen.
initialiser select_me {
  cell [width/2, height] {
    C:alive = true;
  }
}
