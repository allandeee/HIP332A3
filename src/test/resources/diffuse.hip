// Hipster cellular automaton language.
//
// Simple simulation of the diffusion of a chemical.
//
// Cells include a field which records the concentration of a chemical
// at that point. That chemical diffuses at a rate that is proportional
// to the difference of the concentrations between adjacent cells.
//
// © 2017, Dominic Verity, Macquarie University, All rights reserved.
// 
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

// Constants giving the sizes of the dimensions.
int width = 200;
int height = 200;

// Diffusion rate. This determines how strongly the diffusion gradient
// between a cell and its adjacent neighbours influences the rate of
// movement of chemical between them. Must be a value in the range
// 0 <= diff_rate < 0.25, a greater value means that the chemical
// diffuses more quickly.
float diff_rate = 0.16;

// Set width and height of the automaton. We make both dimensions cyclic,
// so our world is a torus, or in other words the surface of a donut.
dimension (width cyclic, height cyclic);

// Specify a Von-Neumann neighbourhood.
neighbourhood 
  N = [0,1], S = [0,-1], W = [-1,0], E = [1,0];

// The state fields of each cell.
state {
  // Each cell records the concentration of chemical at that point.
  float concentration = 0.0;
}

// Cell updater.
updater {
  // Initialise a variable to accumulate the diffusion gradient.
  float gradient = 0;

  // Iterate over neighbours to compute the diffusion gradient.
  iterate cell over others
    gradient = gradient + (cell:concentration - concentration);

  // Update the concentration in the next generation.
  // The amount of substance transferred into this cell is proportional
  // to the diffusion gradient with its neighbours, with constant of
  // propotionality given by the constant `diff_rate`
  concentration = concentration + gradient * diff_rate;
}

// We will display the concentrations of our chemical at each point
// using a grey scale, so setup appropriate colour values.
int black = 0;
int dimgrey = 0x696969;
int grey = 0x808080;
int darkgrey = 0xA9A9A9;
int silver = 0xC0C0C0;
int lightgrey = 0xD3D3D3;
int gainsboro = 0xDCDCDC;
int white = 0xFFFFFF;

// Chemical concentrations are classified into 8 logarithmic bands
float band_size = 1.0 / 128;
float band_7 = band_size * 127;
float band_6 = band_size * 63;
float band_5 = band_size * 31;
float band_4 = band_size * 15;
float band_3 = band_size * 7;
float band_2 = band_size * 3;
float band_1 = band_size * 1;

// Colour mapper. We hard code a binary search to improve
// efficiency. Really Hipster should provide an optimised
// switch statement that does this for us.
mapper {
  if concentration <= band_4 then {
    if concentration <= band_2 then {
      if concentration <= band_1 then
        return (black);
      else
        return (dimgrey);
    } else {
      if concentration <= band_3 then
        return (grey);
      else
        return (darkgrey);
    }
  } else {
    if concentration <= band_6 then {
      if concentration <= band_5 then
        return (silver);
      else
        return (lightgrey);
    } else {
      if concentration <= band_7 then
        return (gainsboro);
      else
        return (white);
    }
  }
}

// Simple function that sets a square of cells
// to all contain chemical at concentation 1.
function drop_blob(int x, int y, int w) {
  for i = (x - w) to (x + w)
    for j = (y - w) to (y + w) 
      cell [i, j]
        concentration = 1;
}

// Simple initialiser to drop a single blob in the
// middle of the screen.
initialiser blob {
  drop_blob(width / 2, height / 2, 20);
}

// Initialiser to drop six blobs at random locations.
initialiser random_blobs {
  for count = 1 to 6
    drop_blob(rnd(width), rnd(height), 10);
}

// Initialiser to randomly initialise all cells.
initialiser random_field {
  for i = 0 to (width - 1)
    for j = 0 to (height - 1)
      cell [i,j]
        if rnd(50) == 0 then
          concentration = 1;
        else
          concentration = 0;
}
