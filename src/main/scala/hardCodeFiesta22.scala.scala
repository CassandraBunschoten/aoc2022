package aoc22

import aoc22.Orientation
import aoc22.Coordinate

def wrapAroundForExample(side: Char, coord: Coordinate, orientation: Orientation): (Char, Coordinate, Orientation) =
  var newSide = side
  var newCoord = coord
  var newOrientation = orientation
  // ----------- SIDE A -------------------
  if (side == 'A' && orientation == Left) 
    newSide = 'C'
    newOrientation = Down
    newCoord = (coord._2 + 4, 5)

  else if (side == 'A' && orientation == Up)
    newSide = 'B'
    newOrientation = Down
    newCoord = (12 % coord._1 + 1, 5)

  else if (side == 'A' && orientation == Right)
    newSide = 'F'
    newOrientation = Left
    newCoord = (16, 12 - coord._2 + 1)

  else if (side == 'A' && orientation == Down)
      newSide = 'D'
      newOrientation = Down
      newCoord = (coord._1, coord._2 + 1)
      
  // ----------- SIDE B -------------------
  else if (side == 'B' && orientation == Left)
    newSide = 'F'
    newOrientation = Up
    newCoord = (8 % coord._2 + 13, 12)

  else if (side == 'B' && orientation == Up)
    newSide = 'A'
    newOrientation = Down
    newCoord = (12 - coord._1 + 1, 1)

  else if (side == 'B' && orientation == Right)
    newSide = 'C'
    newOrientation = Right
    newCoord = (coord._1 + 1, coord._2)  

  else if (side == 'B' && orientation == Down)
    newSide = 'E'
    newOrientation = Up
    newCoord = (4 % coord._1 + 9, 12)

  // ----------- SIDE C -------------------
  else if (side == 'C' && orientation == Left)
    newSide = 'B'
    newOrientation = Left
    newCoord = (coord._1 - 1, coord._2)

  else if (side == 'C' && orientation == Up)
    newSide = 'A'
    newOrientation = Right
    newCoord = (9, coord._1 - 4)

  else if (side == 'C' && orientation == Right)
    newSide = 'D'
    newOrientation = Right
    newCoord = (coord._1 + 1, coord._2)  

  else if (side == 'C' && orientation == Down)
    newSide = 'E'
    newOrientation = Right
    newCoord = (9, 8 % coord._1 + 9)
    
  // ----------- SIDE D -------------------
  else if (side == 'D' && orientation == Left)
    newSide = 'C'
    newOrientation = Left
    newCoord = (coord._1 - 1, coord._2)

  else if (side == 'D' && orientation == Up)
    newSide = 'A'
    newOrientation = Up
    newCoord = (coord._1, coord._2 - 1)

  else if (side == 'D' && orientation == Right)
    newSide = 'F'
    newOrientation = Down
    newCoord = (13 + 8 % coord._2,9)

  else if (side == 'D' && orientation == Down)
    newSide = 'E'
    newOrientation = Down
    newCoord = (coord._1, coord._2 + 1)  

  // ----------- SIDE E -------------------
  else if (side == 'E' && orientation == Left)
    newSide = 'C'
    newOrientation = Up
    newCoord = (12 % coord._2 + 5, 8)

  else if (side == 'E' && orientation == Up)
    newSide = 'D'
    newOrientation = Up
    newCoord = (coord._1, coord._2 - 1)

  else if (side == 'E' && orientation == Right)
    newSide = 'F'
    newOrientation = Right
    newCoord = (coord._1 + 1, coord._2)

  else if (side == 'E' && orientation == Down)
    newSide = 'B'
    newOrientation = Up
    newCoord = (12 % coord._1 + 1, 8)

  // ----------- SIDE F -------------------
  else if (side == 'F' && orientation == Left)
    newSide = 'E'
    newOrientation = Left
    newCoord = (coord._1 - 1, coord._2)
  
  else if (side == 'F' && orientation == Up)
    newSide = 'D'
    newOrientation = Left
    newCoord = (12, 16 % coord._1 + 5)

  else if (side == 'F' && orientation == Right)
    newSide = 'A'
    newOrientation = Left
    newCoord = (12, 12 % coord._2 + 1)

  else if (side == 'F' && orientation == Down)
    newSide = 'B'
    newOrientation = Right
    newCoord = (1, 16 % coord._1 + 5)

  (newSide, newCoord, newOrientation)

def wrapAroundForReal(side: Char, coord: Coordinate, orientation: Orientation): (Char, Coordinate, Orientation) =
  var newSide = side
  var newCoord = coord
  var newOrientation = orientation
  
  // ----------- SIDE A -------------------
  if (side == 'A' && orientation == Left) 
    newSide = 'D'
    newOrientation = Right
    newCoord = (1, 151 - coord._2)

  else if (side == 'A' && orientation == Up)
    newSide = 'F'
    newOrientation = Right
    newCoord = (1, coord._1 + 100)

  else if (side == 'A' && orientation == Right)
    newSide = 'B'
    newOrientation = Right
    newCoord = (coord._1 + 1, coord._2)

  else if (side == 'A' && orientation == Down)
      newSide = 'C'
      newOrientation = Down
      newCoord = (coord._1, coord._2 + 1)
      
  // ----------- SIDE B -------------------
  else if (side == 'B' && orientation == Left)
    newSide = 'A'
    newOrientation = Left
    newCoord = (coord._1 - 1, coord._2)

  else if (side == 'B' && orientation == Up)
    newSide = 'F'
    newOrientation = Up
    newCoord = (coord._1 - 100, 200)

  else if (side == 'B' && orientation == Right)
    newSide = 'E'
    newOrientation = Left
    newCoord = (100, 151 - coord._2)  

  else if (side == 'B' && orientation == Down)
    newSide = 'C'
    newOrientation = Left
    newCoord = (100, coord._1 - 50)

  // ----------- SIDE C -------------------
  else if (side == 'C' && orientation == Left)
    newSide = 'D'
    newOrientation = Down
    newCoord = (coord._2 - 50, 101)

  else if (side == 'C' && orientation == Up)
    newSide = 'A'
    newOrientation = Up
    newCoord = (coord._1, coord._2 - 1)

  else if (side == 'C' && orientation == Right)
    newSide = 'B'
    newOrientation = Up
    newCoord = (coord._2 + 50, 50)  

  else if (side == 'C' && orientation == Down)
    newSide = 'E'
    newOrientation = Down
    newCoord = (coord._1, coord._2 + 1)
    
  // ----------- SIDE D -------------------
  else if (side == 'D' && orientation == Left)
    newSide = 'A'
    newOrientation = Right
    newCoord = (51, 150 % coord._2 + 1)

  else if (side == 'D' && orientation == Up)
    newSide = 'C'
    newOrientation = Right
    newCoord = (51, coord._1 + 50)

  else if (side == 'D' && orientation == Right)
    newSide = 'E'
    newOrientation = Right
    newCoord = (coord._1 + 1, coord._2)

  else if (side == 'D' && orientation == Down)
    newSide = 'F'
    newOrientation = Down
    newCoord = (coord._1, coord._2 + 1)  

  // ----------- SIDE E -------------------
  else if (side == 'E' && orientation == Left)
    newSide = 'D'
    newOrientation = Left
    newCoord = (coord._1 - 1, coord._2)

  else if (side == 'E' && orientation == Up)
    newSide = 'C'
    newOrientation = Up
    newCoord = (coord._1, coord._2 - 1)

  else if (side == 'E' && orientation == Right)
    newSide = 'B'
    newOrientation = Left
    newCoord = (150, 150 % coord._2 + 1)

  else if (side == 'E' && orientation == Down)
    newSide = 'F'
    newOrientation = Left
    newCoord = (50, coord._1 + 100)

  // ----------- SIDE F -------------------
  else if (side == 'F' && orientation == Left)
    newSide = 'A'
    newOrientation = Down
    newCoord = (coord._2 - 100, 1)
  
  else if (side == 'F' && orientation == Up)
    newSide = 'D'
    newOrientation = Up
    newCoord = (coord._1, coord._2 - 1)

  else if (side == 'F' && orientation == Right)
    newSide = 'E'
    newOrientation = Up
    newCoord = (coord._2 - 100, 150)

  else if (side == 'F' && orientation == Down)
    newSide = 'B'
    newOrientation = Down
    newCoord = (coord._1 + 100, 1)

  (newSide, newCoord, newOrientation)