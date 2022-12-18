package aoc17

import scala.collection.mutable.ArrayBuffer
import aoc14.visualise
import scala.io.Source
import java.io._

type Height = Int
type Chamber = ArrayBuffer[ArrayBuffer[Char]]

sealed abstract class Shape(x: Int, y: Int, absolute: List[(Int, Int)]):
  val surrounding = absolute.map { case (x1, y1) => (x1 + x, y1 + y) }
  val (highestPoint, lowestPoint) = {
    val points = surrounding.map { case (x, y) => y }; (points.min, points.max)
  }

  def shiftOrigin(shiftX: Int = 0, shiftY: Int = 0): Shape =
    this match {
      case h: HStripe => h.copy(x = h.x + shiftX, y = h.y + shiftY)
      case p: Plus => p.copy(x = p.x + shiftX, y = p.y + shiftY)
      case r: RevL => r.copy(x = r.x + shiftX, y = r.y + shiftY)
      case v: VStripe => v.copy(x = v.x + shiftX, y = v.y + shiftY)
      case s: Square => s.copy(x = s.x + shiftX, y = s.y + shiftY)
    }

  def checkMove(s: Shape, c: Chamber): Boolean =
    s.surrounding.filterNot { case (x, y) => c(y)(x) == '.' }.isEmpty

  def moveSide(c: Chamber, g: Gas): Shape =
    g match {
      case LeftPush => if (checkMove(this.shiftOrigin(shiftX = -1), c)) {
        this.shiftOrigin(shiftX = -1)
      } else {
        this
      }
      case RightPush => if (checkMove(this.shiftOrigin(shiftX = 1), c)) {
        this.shiftOrigin(shiftX = 1)
      } else {
        this
      }
    }

  def moveDown(c: Chamber): Shape =
    if (checkMove(this.shiftOrigin(shiftY = 1), c)) {
      this.shiftOrigin(shiftY = 1)
    } else {
      this
    }

  def move(c: Chamber, g: Gas): Shape =
    this.moveSide(c, g).moveDown(c)

case class HStripe(x: Int = 3, y: Int = 0) extends Shape(x, y, List((0, 0), (1, 0), (2, 0), (3, 0)))
case class Plus(x: Int = 4, y: Int = 1) extends Shape(x, y, List((0, 0), (0, 1), (0, -1), (1, 0), (-1, 0)))
case class RevL(x: Int = 5, y: Int = 2) extends Shape(x, y, List((0, 0), (0, -1), (0, -2), (-1, 0), (-2, 0)))
case class VStripe(x: Int = 3, y: Int = 3) extends Shape(x, y, List((0, 0), (0, -1), (0, -2), (0, -3)))
case class Square(x: Int = 3, y: Int = 0) extends Shape(x, y, List((0, 0), (1, 0), (0, 1), (1, 1)))

sealed trait Gas

case object RightPush extends Gas
case object LeftPush extends Gas

def parseGas(c: Char): Gas =
  if (c == '<') {
    LeftPush
  } else {
    RightPush
  }

def enterShape(s: Shape, c: Chamber, h: Height): Chamber =
  def addLayers(s: Shape, c: Chamber, h: Height): Chamber = {
    s match {
      case _: HStripe if (h + 4 > c.size)        => c.prepend('|' +: (ArrayBuffer.fill(7)('.') :+ '|')); addLayers(s, c, h)
      case _: HStripe if (h + 4 < c.size)        => c.remove(0); addLayers(s,c,h)
      case _: Plus | _: RevL if (h + 6 > c.size) => c.prepend('|' +: (ArrayBuffer.fill(7)('.') :+ '|')); addLayers(s, c, h)
      case _: Plus | _: RevL if (h + 6 < c.size) => c.remove(0); addLayers(s,c,h)
      case _: VStripe if (h + 7 > c.size)        => c.prepend('|' +: (ArrayBuffer.fill(7)('.') :+ '|')); addLayers(s, c, h)
      case _: VStripe if (h + 7 < c.size)        => c.remove(0); addLayers(s,c,h)
      case _: Square  if (h + 5 > c.size)        => c.prepend('|' +: (ArrayBuffer.fill(7)('.') :+ '|')); addLayers(s, c, h)
      case _: Square  if (h + 5 < c.size)        => c.remove(0); addLayers(s,c,h)
      case _                                     => c
    }
  }
  addLayers(s, c, h)

def moveAndDrawRock(s: Shape, c: Chamber, gases: List[Gas], h: Height): (List[Gas], Height) =
  val g = if (gases.size == 1) {gases ::: AOC17.input} else {gases}
  val newShape = s.move(c, g.head)
  if (newShape.lowestPoint == s.lowestPoint) {
    val maybeSide = s.moveSide(c, g.head)
    maybeSide.surrounding.foreach { case (x, y) => c(y)(x) = '#' }
    if ((c.size - maybeSide.highestPoint) > h) {
      (g, c.size - maybeSide.highestPoint)
    } else {
      (g, h)
    }
  } else {
    moveAndDrawRock(newShape, c, g.tail, h)
  }

def fillChamberUntil(c: Chamber, h: Height, numRocks: Int = 0, goalRocks: Int = 2022, gases: List[Gas] = AOC17.input, shapes: List[Shape] = shapes): Height =
  if (numRocks == goalRocks) {h} else {
    enterShape(shapes.head, c, h)
    val (newGases, newH) = moveAndDrawRock(shapes.head, c, gases, h)
    if (shapes.size != 1) {
      fillChamberUntil(c, newH, numRocks + 1, goalRocks, newGases.tail, shapes.tail)
    } else {
      fillChamberUntil(c, newH, numRocks + 1, goalRocks, gases = newGases.tail)
    }
  }

val shapes = List(HStripe(), Plus(), RevL(), VStripe(), Square())

object AOC17 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc17.txt").mkString.map(parseGas).toList

  val chamber: Chamber = ArrayBuffer.fill(1, 7)('.').append(ArrayBuffer.fill(7)('_')).map(x => '|' +: x :+ '|')

  val answerP1 = fillChamberUntil(chamber, 1) - 1

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: missing :(")
