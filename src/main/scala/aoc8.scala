package aoc2022
import scala.io.Source

case class Coordinates(x: Int, y: Int)

def getCoordinates(input: List[(String, Int)]): Map[Coordinates, Int]=
  input.flatMap{case (row, rowIndex) => 
    row.zipWithIndex.map{case (tree, colIndex) => Map(Coordinates(colIndex, rowIndex) -> tree.toString.toInt)}}
    .foldLeft(Map[Coordinates, Int]())((a, b) => b ++ a)

def visibleInRow(trees: Map[Coordinates, Int], width: Int, counter: Int, result: Map[Coordinates, Boolean]): Map[Coordinates, Boolean] = {
  if (trees.nonEmpty) {
    val row = trees.filter(x => x._1.y == counter)

     val res = row.map{case (coord, int) => 
      val neighsLeft = row.filter(x => x._1.x < coord.x)
      val neighsRight = row.filter(x => x._1.x > coord.x)

      if (coord.x == 0 | coord.x == width) (coord -> true) else {
      val visibleFromLeft =  (coord, neighsLeft.foldLeft(true)((acc, neigh) => acc && neigh._2 < int))
      val visibleFromRight = (coord, neighsRight.foldLeft(true)((acc, neigh) => acc && neigh._2 < int))

      (coord, visibleFromRight._2 || visibleFromLeft._2)
      }
      }
      visibleInRow(trees.filterNot(x => x._1.y == counter), width, counter + 1, res ++ result)
     } else {
    result
      }
    }

def visibleInCol(trees: Map[Coordinates, Int], height: Int, counter: Int, result: Map[Coordinates, Boolean]): Map[Coordinates, Boolean] = {
  if (trees.nonEmpty) {
    val col = trees.filter(x => x._1.x == counter)

     val res = col.map{case (coord, int) => 
      val neighsUp = col.filter(y => y._1.y > coord.y)
      val neighsDown = col.filter(y => y._1.y < coord.y)

      if (coord.y == 0 | coord.y == height) (coord -> true) else {
      val visibleFromUp =  (coord, neighsUp.foldLeft(true)((acc, neigh) => acc && neigh._2 < int))
      val visibleFromDown = (coord, neighsDown.foldLeft(true)((acc, neigh) => acc && neigh._2 < int))

      (coord, visibleFromUp._2 || visibleFromDown._2)
      }
      }
      visibleInCol(trees.filterNot(x => x._1.x == counter), height, counter + 1, res ++ result)
     } else {
    result
      }
    }

def viewInRow(trees: Map[Coordinates, Int], width: Int, counter: Int, result: Map[Coordinates, Int]): Map[Coordinates, Int] = {
  if (trees.nonEmpty) {
    val row = trees.filter(x => x._1.y == counter)

     val res = row.map{case (coord, int) => 
      val neighsLeft = row.filter(x => x._1.x < coord.x)
      val neighsRight = row.filter(x => x._1.x > coord.x)

      val visibleFromLeft = if (coord._1 == 0) 0 else {tinyViewLeft(coord, int, neighsLeft, counter = 1, result = 1)}
      val visibleFromRight = if (coord._1 == width) 0 else {tinyViewRight(coord, int, neighsRight, counter = 1, result = 1, width)}

      (coord, visibleFromLeft * visibleFromRight)
      }
      viewInRow(trees.filterNot(x => x._1.y == counter), width, counter + 1, res ++ result)
     } else {
    result
      }
    }

  def tinyViewLeft(currentCoord: Coordinates, currentNum: Int, neighs: Map[Coordinates, Int], counter: Int, result: Int): Int = 
    if (currentCoord.x - counter <= 0 ) {result} else {
      if (neighs(Coordinates(currentCoord.x - counter, currentCoord.y)) < currentNum) {
        tinyViewLeft(currentCoord, currentNum, neighs, counter + 1, result + 1)
      } else result
    }
  def tinyViewRight(currentCoord: Coordinates, currentNum: Int, neighs: Map[Coordinates, Int], counter: Int, result: Int, upper: Int): Int = 
    if (currentCoord.x + counter >= upper ) {result} else {
      if (neighs(Coordinates(currentCoord.x + counter, currentCoord.y)) < currentNum) {
        tinyViewRight(currentCoord, currentNum, neighs, counter + 1, result + 1, upper)
      } else result
    }

def viewInCol(trees: Map[Coordinates, Int], height: Int, counter: Int, result: Map[Coordinates, Int]): Map[Coordinates, Int] = {
  if (trees.nonEmpty) {
    val col = trees.filter(y => y._1.x == counter)

     val res = col.map{case (coord, int) => 
      val neighsUp = col.filter(y => y._1.y < coord.y)
      val neighsDown = col.filter(y => y._1.y > coord.y)

      val visibleFromUp = if (coord._2 == 0) 0 else {tinyViewUp(coord, int, neighsUp, counter = 1, result = 1)}
      val visibleFromDown = if (coord._2 == height) 0 else {tinyViewDown(coord, int, neighsDown, counter = 1, result = 1, height)}

      (coord, visibleFromUp * visibleFromDown)
      }
      viewInCol(trees.filterNot(y => y._1.x == counter), height, counter + 1, res ++ result)
     } else {
    result
      }
    }
// result sws 1

  def tinyViewDown(currentCoord: Coordinates, currentNum: Int, neighs: Map[Coordinates, Int], counter: Int, result: Int, upper: Int): Int = 
    if (currentCoord.y + counter >= upper) {result} else {
      if (neighs(Coordinates(currentCoord.x, currentCoord.y + counter)) < currentNum) {
        tinyViewDown(currentCoord, currentNum, neighs, counter + 1, result + 1, upper)
      } else result
    }
def tinyViewUp(currentCoord: Coordinates, currentNum: Int, neighs: Map[Coordinates, Int], counter: Int, result: Int): Int = 
  if (currentCoord.y - counter <= 0 ) {result} else {
    if (neighs(Coordinates(currentCoord.x, currentCoord.y - counter)) < currentNum) {
      tinyViewUp(currentCoord, currentNum, neighs, counter + 1, result + 1)
    } else result
    }


      // val visibleFromUp =  (coord, neighsUp.foldLeft(true)((acc, neigh) => acc && neigh._2 < int))

object AOC8 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc8.txt").getLines.zipWithIndex.toList

  val coords = getCoordinates(input)

  val width = coords.keys.map(x => x._1).max

  val height = coords.keys.map(y => y._2).max

  val visCols = visibleInCol(coords, width, counter = 0, result = Map[Coordinates, Boolean]())

  val visRows = visibleInRow(coords, height, counter = 0, result = Map[Coordinates, Boolean]())

  val viewCol = viewInCol(coords, height, 0, Map[Coordinates, Int]())

  val viewRow = viewInRow(coords, width, 0, Map[Coordinates, Int]())

  val together = visCols ++ visRows.map{ case (k, v) => k -> (v | visCols(k))}

  val togetherView = viewCol ++ viewRow.map{ case (k, v) => k -> v * viewCol(k)}

  println(s"${together.map{case (k, v) => v}.count(x => x)}")
  println(s"${togetherView.map{case (k, v) => v}.max}")

  // println(viewRow)


  