// Hipster cellular automaton language.
//
// Conway's game of life written in Hipster.
//
// © 2017, Dominic Verity, Macquarie University, All rights reserved.
// 
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// Constant declarations giving the size of the automaton.
// All variable declarations at top level are assumed constant.

int width = 100;
int height = 100;

// Declare the dimensions of the automaton.
// The `cyclic` suffix specifies that a dimension should
// "wrap around" at its boundary (as in the video game Asteroids).

dimension(width cyclic, height);

// Declare the symbols used to refer to neighbouring cells.
// At any point the runtime keeps track of the location of a *current*
// cell. In the following declaration the locations of neighbours
// are specified relative to that current cell, which is always
// referred to by the name `me`.

neighbourhood   // specify a Moore neighbourhood.
  N = [0,1], S = [0,-1], W = [-1,0], E = [1,0],
  NE = [1,1], SE = [1,-1], SW = [-1,-1], NW = [-1,1];

// Declare the state information that each cell possesses.
// For the game of life this comprises a single boolean value
// which contains `true` if the cell is alive and `false` if
// it is dead.

state {
  boolean alive = false;  // default state of a cell is dead.
}

// The expressions `SE:alive` and `N:alive` refer to the values of
// the `alive` field of the `SE` and `N` neighbours of the current cell
// (in the current generation). The expression `alive` or `me:alive`
// refer to the value of the `alive` field of the current cell.

// In updater code we may make assignments to the fields of the current
// cell but not to those of its neighbours. Those assignments are made to
// the state of the cell in the *next* generation.

// Declare the updater function.
// During the update from one generation to the next
// this code is called once for each cell in the simulation.

updater {
  // Initialise counter of live neighbours.
  int count = 0;

  // Count cells that are alive.
  // The `others` keyword specifies iteration over all cells in
  // the neighbourhood except for the central cell.
  
  iterate current over others
    if current:alive then count = count+1;

  // Set new state of cell according to the GoL updating rules.
  // If no explicit assignment is made to a state field then
  // the current value of that field is automatically copied to
  // the next generation.

  if alive && (count < 2 || count > 3) then alive = false;
  if !alive && count == 3 then alive = true;
}

// Declare some colour constants. Colours are represented by sRGB
// integers comprising three 8-bit fields giving blue (bits 0-7),
// green (bits 8-15) and red (bits 16-23) colour channel intensities.

int black = 0;
int white = 0xFFFFFF;  // Hexadecimal integer notation.

// Declare the colour mapper function.
// Returns a colour integer computed from the state of the
// current cell. This colour is then used when displaying the state
// of that cell on the screen. Code in this mapper is not allowed to
// change the state of current cell.

mapper {
  if alive then return(white); else return(black);
}

// Declare a function to place a "glider" at a specified location.

function glider(int row, int col) {
  cell [row, column]
    iterate nbr over [N, E, SW, S, SE]
      nbr:alive = true;
}

// Note that the last function uses the `cell` statement. This sets the
// current cell to a specified location and then executes the associated
// statement (an `iterate` statement in this case). The body of the 
// iterate loop also makes assignments to the state of neighbouring cells.
// These operations are only allowed when initialising the automation.
// They are not allowed during updating, the updater should only change
// the value of the current cell and can only depend upon the values of 
// cells in the immediate neighbourhood.

// Declare initialisers. There can be more than one of these, each one
// is named and the particular initialiser to run at the start of the
// simulation can be selected by the user (default is the first listed).

// This initialiser places a single glider in the centre of the grid.

initialiser singleGlider {
  glider(height / 2, width / 2);
}

// This initialiser places 4 gliders at various equidistant points.

initialiser fourGliders {
  glider(height / 4, width / 4);
  glider(3 * height / 4, width / 4);
  glider(height / 4, 3 * width / 4);
  glider(3 * height / 4, 3 * width / 4);
}
