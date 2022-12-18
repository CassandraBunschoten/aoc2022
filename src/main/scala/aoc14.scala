package aoc14

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io._

val pattern = "([0-9]+),([0-9]+)".r

def drawMap(i: List[String], startCoord: List[Coord]): List[Coord] = {
  i match {
    case s"$x,$y" :: next => drawMap(next, drawTo(startCoord, Coord(x.toInt, y.toInt)))
    case Nil => startCoord
    case _ => sys.error("uhoh")
  }
}

def drawTo(coord1: List[Coord], coord2: Coord): List[Coord] = {
  val (difx, dify) = coord1.head.difference(coord2)
  if (difx == dify) {
    coord1
  } else if (difx > 0) {
    drawTo(coord1.head.copy(x = coord1.head.x - 1) :: coord1, coord2)
  } else if (difx < 0) {
    drawTo(coord1.head.copy(x = coord1.head.x + 1) :: coord1, coord2)
  } else if (dify > 0) {
    drawTo(coord1.head.copy(y = coord1.head.y - 1) :: coord1, coord2)
  } else {
    drawTo(coord1.head.copy(y = coord1.head.y + 1) :: coord1, coord2)
  }
}

def checkMovement(p1: Boolean, cave: ArrayBuffer[ArrayBuffer[Char]], minX: Int, maxX: Int, maxY: Int, coord: Coord = Coord(500, 1), acc: Int = 0): Int = {
  if (coord.x - minX + 1 > maxX) {
    if (p1) {
      acc
    } else {
      val newCave = cave.map(r => r :+ '.')
      newCave(maxY)(coord.x - minX + 1) = '#'
      checkMovement(p1, newCave, minX, maxX + 1, maxY, coord = coord.copy(x = coord.x, y = coord.y), acc)
    }
  } else if (coord.x - minX - 1 < 0) {
    if (p1) {
      acc
    } else {
      val newCave = cave.map(r => '.' +: r)
      newCave(maxY)(0) = '#'
      checkMovement(p1, newCave, minX - 1, maxX, maxY, coord = coord.copy(x = coord.x, y = coord.y), acc)
    }
  } else if (coord.y + 1 == maxY && p1) {
    acc
  } else if (cave(coord.y + 1)(coord.x - minX) == '.') {
    checkMovement(p1, cave, minX, maxX, maxY, coord.copy(y = coord.y + 1), acc)
  } else if (cave(coord.y + 1)(coord.x - minX - 1) == '.') {
    checkMovement(p1, cave, minX, maxX, maxY, coord.copy(x = coord.x - 1, y = coord.y + 1), acc)
  } else if (cave(coord.y + 1)(coord.x - minX + 1) == '.') {
    checkMovement(p1, cave, minX, maxX, maxY, coord.copy(x = coord.x + 1, y = coord.y + 1), acc)
  } else {
    cave(coord.y)(coord.x - minX) = 'o';
    if (!p1 && (cave(0)(500 - minX) == 'o')) {
      acc
    } else {
      checkMovement(p1, cave, minX, maxX, maxY, acc = acc + 1)
    }
  }
}

def visualise(a: ArrayBuffer[ArrayBuffer[Char]]) =
  println(a.map(_.mkString("")).mkString("\n"))

case class Coord(x: Int, y: Int):
  def difference(coord2: Coord): (Int, Int) =
    (this.x - coord2.x, this.y - coord2.y)

object AOC14 extends App :
  val input = Source.fromFile("src/main/resources/input_aoc14.txt")
    .getLines.map(x => x.split(" -> ").toList)
    .flatMap(l => {
      val pattern(x, y) = l.head
      drawMap(l.tail, List(Coord(x.toInt, y.toInt)))
    }).toList

  val minX = input.map(_.x).min
  val maxX = input.map(_.x).max - minX + 1
  val maxY = input.map(_.y).max + 1

  val cave = ArrayBuffer.fill(maxY, maxX)('.')
  input.map(c => cave(c.y)(c.x - minX) = '#')

  val newCave = ArrayBuffer.fill(maxY, maxX)('.')
  input.map(c => newCave(c.y)(c.x - minX) = '#')
  val emptyLayer = ArrayBuffer.fill(maxX)('.')
  val rockLayer = ArrayBuffer.fill(maxX)('#')
  newCave += emptyLayer += rockLayer

  val answerP1 = checkMovement(p1 = true, cave, minX, maxX, maxY)
  val answerP2 = checkMovement(p1 = false, newCave, minX, maxX, maxY + 1)

  visualise(cave)

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)
