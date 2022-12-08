package aoc2022

import scala.io.Source

sealed trait Direction
case object L extends Direction
case object R extends Direction
case object U extends Direction
case object D extends Direction

case class Coordinates(x: Int, y: Int)

def getCoordinates(input: List[(String, Int)]): Map[Coordinates, Int] =
  input.flatMap { case (row, rowIndex) =>
    row.zipWithIndex.map { case (tree, colIndex) => Map(Coordinates(colIndex, rowIndex) -> tree.toString.toInt) }
  }
    .foldLeft(Map[Coordinates, Int]())((a, b) => b ++ a)

def visibleInRow(trees: Map[Coordinates, Int], width: Int, counter: Int = 0, result: Map[Coordinates, Boolean]): Map[Coordinates, Boolean] = {
  if (trees.nonEmpty) {
    val row = trees.filter(x => x._1.y == counter)

    val res = row.map { case (coord, int) =>
      if (coord.x == 0 | coord.x == width) (coord -> true) else {
        val visibleFromLeft = row.filter(x => x._1.x < coord.x).forall(neigh => neigh._2 < int)
        val visibleFromRight = row.filter(x => x._1.x > coord.x).forall(neigh => neigh._2 < int)
        (coord, visibleFromRight || visibleFromLeft)
      }
    }
    visibleInRow(trees.filterNot(x => x._1.y == counter), width, counter + 1, res ++ result)
  } else {
    result
  }
}

def visibleInCol(trees: Map[Coordinates, Int], height: Int, counter: Int = 0, result: Map[Coordinates, Boolean]): Map[Coordinates, Boolean] = {
  if (trees.nonEmpty) {
    val col = trees.filter(x => x._1.x == counter)

    val res = col.map { case (coord, int) =>
      if (coord.y == 0 | coord.y == height) (coord -> true) else {
        val visibleFromUp = col.filter(y => y._1.y > coord.y).forall(neigh => neigh._2 < int)
        val visibleFromDown = col.filter(y => y._1.y < coord.y).forall(neigh => neigh._2 < int)
        (coord, visibleFromUp || visibleFromDown)
      }
    }
    visibleInCol(trees.filterNot(x => x._1.x == counter), height, counter + 1, res ++ result)
  } else {
    result
  }
}

def viewInRow(trees: Map[Coordinates, Int], width: Int, counter: Int = 0, result: Map[Coordinates, Int]): Map[Coordinates, Int] = {
  if (trees.nonEmpty) {
    val row = trees.filter(x => x._1.y == counter)

    val res = row.map { case (coord, int) =>
      val neighsLeft = row.filter(x => x._1.x < coord.x)
      val neighsRight = row.filter(x => x._1.x > coord.x)
      val visibleFromLeft = if (coord._1 == 0) 0 else {
        tinyView(L, coord, int, neighsLeft, counter = 1, result = 1, 0)
      }
      val visibleFromRight = if (coord._1 == width) 0 else {
        tinyView(R, coord, int, neighsRight, counter = 1, result = 1, width)
      }
      (coord, visibleFromLeft * visibleFromRight)
    }
    viewInRow(trees.filterNot(x => x._1.y == counter), width, counter + 1, res ++ result)
  } else {
    result
  }
}

def viewInCol(trees: Map[Coordinates, Int], height: Int, counter: Int = 0, result: Map[Coordinates, Int]): Map[Coordinates, Int] = {
  if (trees.nonEmpty) {
    val col = trees.filter(y => y._1.x == counter)

    val res = col.map { case (coord, int) =>
      val neighsUp = col.filter(y => y._1.y < coord.y)
      val neighsDown = col.filter(y => y._1.y > coord.y)
      val visibleFromUp = if (coord._2 == 0) 0 else {
        tinyView(U, coord, int, neighsUp, counter = 1, result = 1, 0)
      }
      val visibleFromDown = if (coord._2 == height) 0 else {
        tinyView(D, coord, int, neighsDown, counter = 1, result = 1, height)
      }
      (coord, visibleFromUp * visibleFromDown)
    }
    viewInCol(trees.filterNot(y => y._1.x == counter), height, counter + 1, res ++ result)
  } else {
    result
  }
}

def tinyView(direction: Direction, currentCoord: Coordinates, currentNum: Int, neighs: Map[Coordinates, Int], counter: Int, result: Int, limit: Int): Int =
  direction match {
    case L => tinyViewRow(currentCoord, currentNum, neighs, counter, result, limit,  _ - _,  _ <= _)
    case R => tinyViewRow(currentCoord, currentNum, neighs, counter, result, limit, _ + _, _ >= _)
    case U => tinyViewCol(currentCoord, currentNum, neighs, counter, result, limit, _ - _, _ <= _)
    case D => tinyViewCol(currentCoord, currentNum, neighs, counter, result, limit, _ + _, _ >= _)
  }

def tinyViewRow(currentCoord: Coordinates, currentNum: Int, neighs: Map[Coordinates, Int], counter: Int, result: Int, limit: Int, dirFunc: (Int, Int) => Int, compareFunc: (Int, Int) => Boolean): Int =
  if (compareFunc(dirFunc(currentCoord.x, counter), limit)) {
    result
  } else {
    if (neighs(Coordinates(dirFunc(currentCoord.x, counter), currentCoord.y)) < currentNum) {
      tinyViewRow(currentCoord, currentNum, neighs, counter + 1, result + 1, limit, dirFunc, compareFunc)
    } else result
  }

def tinyViewCol(currentCoord: Coordinates, currentNum: Int, neighs: Map[Coordinates, Int], counter: Int, result: Int, limit: Int, dirFunc: (Int, Int) => Int, compareFunc: (Int, Int) => Boolean): Int =
  if (compareFunc(dirFunc(currentCoord.y, counter), limit)) {
    result
  } else {
    if (neighs(Coordinates(currentCoord.x, dirFunc(currentCoord.y, counter))) < currentNum) {
      tinyViewCol(currentCoord, currentNum, neighs, counter + 1, result + 1, limit, dirFunc, compareFunc)
    } else result
  }


object AOC8 extends App :

  val coords = getCoordinates(Source.fromFile("src/main/resources/input_aoc8.txt").getLines.zipWithIndex.toList)

  val width = coords.keys.map(x => x._1).max
  val height = coords.keys.map(y => y._2).max

  val visCols = visibleInCol(coords, width, result = Map[Coordinates, Boolean]())
  val visRows = visibleInRow(coords, height, result = Map[Coordinates, Boolean]())
  val together = visCols ++ visRows.map { case (k, v) => k -> (v | visCols(k)) }
  val answerP1 = together.map { case (k, v) => v }.count(x => x)

  val viewCol = viewInCol(coords, height, result = Map[Coordinates, Int]())
  val viewRow = viewInRow(coords, width, result = Map[Coordinates, Int]())
  val togetherView = viewCol ++ viewRow.map { case (k, v) => k -> v * viewCol(k) }
  val  answerP2 = togetherView.map { case (k, v) => v }.max

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)


  