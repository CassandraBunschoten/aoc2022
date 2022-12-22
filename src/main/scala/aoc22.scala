package aoc22

import scala.io.Source
import scala.collection.mutable._
import aoc22.wrapAroundForExample

trait Orientation
case object Up extends Orientation
case object Right extends Orientation
case object Down extends Orientation
case object Left extends Orientation

def changeOrientation(t: Turn, o: Orientation): Orientation = {
  t match {
    case R => o match {
      case Up => Right
      case Right => Down 
      case Down => Left
      case Left => Up
    }
    case L => o match {
      case Up => Left
      case Right => Up
      case Down => Right
      case Left => Down
    }
  }
}

sealed trait Instruction
sealed trait Turn extends Instruction
case object R extends Turn
case object L extends Turn

case class Steps(n: Int) extends Instruction

case class CoordinateInfo(tile: Char, previousX: Int, nextX: Int, previousY: Int, nextY: Int)

type Coordinates = Map[Coordinate, CoordinateInfo]
type Coordinate = (Int, Int)

type SideCoordinates = (Int, Int, Int, Int)

case class Cube(s1: SideCoordinates, s2: SideCoordinates, s3: SideCoordinates, s4: SideCoordinates, s5: SideCoordinates, s6: SideCoordinates):
  def checkIfOutOfBounds(coord: Coordinate, side: Char): Boolean =
    val (minX, maxX, minY, maxY) = side match {
      case 'A' => s1
      case 'B' => s2
      case 'C' => s3
      case 'D' => s4
      case 'E' => s5
      case 'F' => s6
    }
    (coord._1 < minX || coord._1 > maxX || coord._2 < minY || coord._2 > maxY)

def makeSides(startX: Int, startY: Int, maxSize: Int): SideCoordinates = 
  (startX, startX + maxSize, startY, startY + maxSize)

def forY(board: List[List[(Char, Int, Int)]], i: Int = 1): List[(Int, (Int, Int))] = 
  if (i == board.map(x => x.size + 1).max) {
    Nil
  } else {
    val ys = board.map(x => x.filter{(c, x, y) => x == i}).flatten.filterNot{(c, x, y) => c == ' '}.map{(c, x, y) => y} 
    (i, (ys.min, ys.max)) :: forY(board, i + 1)
  }

def checkXRange(x: Int, y: Int, xRange: List[((Int, Int), Int)]): (Int, Int) = {
  val (minX, maxX) = xRange.filter(_._2 == y).head._1
  if (x >= maxX) {
    (maxX - 1, minX)
  } else if (x <= minX) {
    (maxX, minX + 1)
  } else {
    (x - 1, x + 1)
  }
}
def checkYRange(x: Int, y: Int, yRange: List[(Int,(Int, Int))]): (Int, Int) = {
  val (minY, maxY) = yRange.filter{_._1 == x}.head._2
  if (y >= maxY) {
    (maxY - 1, minY)
  } else if (y <= minY) {
    (maxY, minY + 1)
  } else {
    (y - 1, y + 1)
  } 
}

def makeCircularBoard(input: List[List[(Char, Int, Int)]], xRange: List[((Int, Int), Int)], yRange: List[(Int, (Int, Int))]): List[(Coordinate, CoordinateInfo)] = {
  def recForRow(row: List[(Char, Int, Int)]): List[(Coordinate, CoordinateInfo)] = 
    row match {
      case (c, x, y) :: t if c != ' ' => { val (prevX, nextX) = checkXRange(x, y, xRange)
                                           val (prevY, nextY) = checkYRange(x, y, yRange) 
                                           ((x, y), CoordinateInfo(c, prevX, nextX, prevY, nextY)) :: recForRow(t) 
                                        }
      case (c, x, y) :: t             => recForRow(t)
      case Nil                        => Nil 
  }
  input match {
    case h :: t => (recForRow(h) ::: makeCircularBoard(t, xRange, yRange))
    case Nil    => Nil
  }
}

def executeInstruction(steps: Int, orientation: Orientation, location: Coordinate, board: Coordinates): Coordinate = {
  if (steps == 0) {
    location
  } else {
    orientation match {
      case Up => {  val curCoordInfo = board(location)
                    val potentialNewCoord = (location._1, curCoordInfo.previousY)
                    if (board(potentialNewCoord).tile == '#') {
                      location
                    }  else {
                      executeInstruction(steps - 1, orientation, potentialNewCoord, board)
                    }
      }
      case Down => { val curCoordInfo = board(location)
                     val potentialNewCoord = (location._1, curCoordInfo.nextY)
                     if (board(potentialNewCoord).tile == '#') {
                        location
                      }  else {
                        executeInstruction(steps - 1, orientation, potentialNewCoord, board)
                      }
      }
      case Left => { val curCoordInfo = board(location)
                     val potentialNewCoord = (curCoordInfo.previousX, location._2)
                     if (board(potentialNewCoord).tile == '#') {
                        location
                      }  else {
                        executeInstruction(steps - 1, orientation, potentialNewCoord, board)
                      }
      }
      case Right => { val curCoordInfo = board(location)
                     val potentialNewCoord = (curCoordInfo.nextX, location._2)
                     if (board(potentialNewCoord).tile == '#') {
                        location
                      }  else {
                        executeInstruction(steps - 1, orientation, potentialNewCoord, board)
                      }
      }
    }
  }
} 

def moveOverBoard(board: Coordinates, instructions: List[Instruction], location: (Int, Int), orientation: Orientation): ((Int, Int), Orientation) = 
  instructions match {
    case R :: t         => moveOverBoard(board, t, location, changeOrientation(R, orientation))
    case L :: t         => moveOverBoard(board, t, location, changeOrientation(L, orientation))
    case Steps(i) :: t  => {val newLoc = executeInstruction(i, orientation, location, board); moveOverBoard(board, t, newLoc, orientation)}
    case Nil            => (location, orientation)
  }

def executeInstructionP2(steps: Int, orientation: Orientation, location: Coordinate, side: Char, board: Coordinates, cube: Cube): (Coordinate, Char, Orientation) = {
  if (steps == 0) {
    (location, side, orientation)
  } else {
    orientation match {
      case Up => {  val curCoordInfo = board(location)
                    val (newSide, potentialNewCoord, newOrientation) = 
                      if (cube.checkIfOutOfBounds((location._1, location._2 - 1), side)) {
                      wrapAroundForReal(side, location, orientation)
                      } else { (side, (location._1, curCoordInfo.previousY), orientation) }

                    if (board(potentialNewCoord).tile == '#') {
                      (location, side, orientation)
                    }  else {
                      executeInstructionP2(steps - 1, newOrientation, potentialNewCoord, newSide, board, cube)
                    }
      }
      case Down => { val curCoordInfo = board(location)
                     val (newSide, potentialNewCoord, newOrientation) = 
                      if (cube.checkIfOutOfBounds((location._1, location._2 + 1), side)) {
                      wrapAroundForReal(side, location, orientation)
                      } else { (side, (location._1, curCoordInfo.nextY), orientation) }
                      
                     if (board(potentialNewCoord).tile == '#') {
                        (location, side, orientation)
                      }  else {
                        executeInstructionP2(steps - 1, newOrientation, potentialNewCoord, newSide, board, cube)
                      }
      }
      case Left => { val curCoordInfo = board(location)
                     val (newSide, potentialNewCoord, newOrientation) = 
                      if (cube.checkIfOutOfBounds((location._1 - 1, location._2), side)) {
                      wrapAroundForReal(side, location, orientation) 
                      } else { (side, (curCoordInfo.previousX, location._2), orientation) }

                     if (board(potentialNewCoord).tile == '#') {
                        (location, side, orientation)
                      }  else {
                        executeInstructionP2(steps - 1, newOrientation, potentialNewCoord, newSide, board, cube)
                      }
      }
      case Right => { val curCoordInfo = board(location)
                      val (newSide, potentialNewCoord, newOrientation) = 
                        if (cube.checkIfOutOfBounds((location._1 + 1, location._2), side)) {
                        wrapAroundForReal(side, location, orientation) 
                        } else { (side, (curCoordInfo.nextX, location._2), orientation) }

                      if (board(potentialNewCoord).tile == '#') {
                         (location, side, orientation)
                        }  else {
                          executeInstructionP2(steps - 1, newOrientation, potentialNewCoord, newSide, board, cube)
                        } 
      }
    }
  }
}   

def moveOverCube(board: Coordinates, instructions: List[Instruction], location: (Int, Int), orientation: Orientation, side: Char, cube: Cube): ((Int, Int), Orientation) = 
  instructions match {
    case R :: t         => moveOverCube(board, t, location, changeOrientation(R, orientation), side, cube)
    case L :: t         => moveOverCube(board, t, location, changeOrientation(L, orientation), side, cube)
    case Steps(i) :: t  => {val (newLoc, newSide, newOr) = executeInstructionP2(i, orientation, location, side, board, cube); moveOverCube(board, t, newLoc, newOr, newSide, cube)}
    case Nil            => (location, orientation)
  }


object AOC22 extends App:
  val pattern = "([0-9]+)|([A-Z]+)".r
  val input = Source.fromResource("input_aoc22.txt")
                    .getLines
                    .map{i => i match {
                      case s if s.head.isLetterOrDigit => {
                        val ex = pattern.findAllIn(s).toList 
                        ex.map(x => x match {
                          case d if d.head.isDigit => Steps(d.toInt)
                          case "R"                 => R
                          case "L"                 => L
                            }
                          )
                        }.toList
                      case i => i.mkString.zipWithIndex.toList
                    }
                    }.toList

  val instructions: List[Instruction] = input.last.collect{case x: Instruction => x}

  val board = input.init.map(r => r.collect{case c: (Char, Int) => c}).zipWithIndex.map{(xs, y) => xs.map{(c, x) => (c, x + 1, y + 1)}}

  val rangeXPerRow = board.map(x => x.filterNot{(c, x, y) => c == ' '}.map{(c, x, y) => x}).map(xs => (xs.min, xs.max)).zipWithIndex.map{ case ((xmin, xmax), y) => ((xmin, xmax), y + 1)}
  val rangeYPerCol = forY(board)

  val coordinates: Coordinates = (makeCircularBoard(board, rangeXPerRow, rangeYPerCol).to(scala.collection.mutable.Map))

  val start = coordinates.keys.toList.sortBy{case (x,y) => (y, x)}.head

  val ran = moveOverBoard(coordinates, instructions, start, Right)
  
  val answerP1 = (ran._1._2 * 1000) + (ran._1._1 * 4)

  val cubeSize = (rangeXPerRow.map{case (xs, y) => xs._2 - xs._1}.min).min(rangeYPerCol.map{case (x, ys) => ys._2 - ys._1}.min)

  val constructCubeRealData: Cube =
    Cube(s1 = makeSides(51, 1, cubeSize),      
        s2 = makeSides(101, 1, cubeSize), 
        s3 = makeSides(51, 51, cubeSize), 
        s4 = makeSides(1, 101, cubeSize), 
        s5 = makeSides(51, 101, cubeSize), 
        s6 = makeSides(1, 151, cubeSize))

  val ran2 = moveOverCube(coordinates, instructions, start, Right, 'A', constructCubeRealData)

  val answerP2 = (ran2._1._2 * 1000) + (ran2._1._1 * 4) + 2

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)


  // val constructCubeExample: Cube = 
  //   Cube(s1 = makeSides(9, 1, cubeSize), 
  //       s2 = makeSides(1, 5, cubeSize), 
  //       s3 = makeSides(5, 5, cubeSize), 
  //       s4 = makeSides(9, 5, cubeSize), 
  //       s5 = makeSides(9, 9, cubeSize), 
  //       s6 = makeSides(12, 9, cubeSize))