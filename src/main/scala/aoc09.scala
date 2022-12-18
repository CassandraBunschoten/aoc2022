package aoc09

import scala.io.Source
import aoc08._

case class Coord(x: Int, y: Int):
  def move(direction: Direction): Coord =
    direction match {
      case L => this.copy(x = this.x - 1)
      case R => this.copy(x = this.x + 1)
      case U => this.copy(y = this.y + 1)
      case D => this.copy(y = this.y - 1)
    }

case class LilSnake(body: List[Coord]):
  def slither(instructions: List[(Direction, Int)], tailList: List[Coord] = List(Coord(0,0)), curSnake: LilSnake = this): Int =
    instructions match {
      case Nil => tailList.distinct.size
      case h :: t => { val (newSnake, newList) = curSnake.lilSlither(h, tailList)
        slither(t, newList, newSnake)
      }
    }

  def lilSlither(instruction: (Direction, Int), tailList: List[Coord], curSnake: LilSnake = this): (LilSnake, List[Coord]) =
    val (dir, stepNum) = instruction
    if (stepNum == 0) { (curSnake, tailList)
    } else {
      val newHead = moveOneStep(curSnake.body(0), curSnake.body(1), dir)._1
      val newSnake = LilSnake(curSnake.body.tail.scanLeft(newHead)((a , c) => followLilSnake(a, c)))
      lilSlither((dir, stepNum - 1), newSnake.body.last :: tailList, newSnake)
    }

  private def followLilSnake(curNeigh: Coord, curT: Coord): Coord = {
    if (checkDist(curNeigh, curT)) curT else {
      val offsets = List((1, 2), (1, -2), (-1, 2), (-1, -2), (2, 1), (2, -1), (-2, 1), (-2, -1), (-2, -2), (2, 2), (-2, 2), (2, -2)).map { case (x, y) => Coord(x, y) }
      val moveOnOffset = List((1, 1), (1, -1), (-1, 1), (-1, -1), (1, 1), (1, -1), (-1, 1), (-1, -1), (-1, -1), (1, 1), (-1, 1), (1, -1)).map { case (x, y) => Coord(x, y) }
      val halfOffset = List((0, 2), (0, -2), (2, 0), (-2, 0)).map { case (x, y) => Coord(x, y) }
      val moveOnHalfOffset = List((0, 1), (0, -1), (1, 0), (-1, 0)).map { case (x, y) => Coord(x, y) }

      val diffCoord = Coord(curNeigh.x - curT.x, curNeigh.y - curT.y)

      val optOffset = offsets.zipWithIndex.find { case (coord, index) => coord == diffCoord }
      val optHalfOffset = halfOffset.zipWithIndex.find { case (coord, index) => coord == diffCoord }

      (optOffset, optHalfOffset) match {
        case (Some((_, i)), None) => Coord(curT.x + moveOnOffset(i).x, curT.y + moveOnOffset(i).y)
        case (None, Some((_, i))) => Coord(curT.x + moveOnHalfOffset(i).x, curT.y + moveOnHalfOffset(i).y)
        case _ => sys.error("booboo")
      }
    }
  }

    private def checkDist(curH: Coord, curT: Coord): Boolean = {
      Math.abs(curH.x - curT.x) < 2 && Math.abs(curH.y - curT.y) < 2
    }

    private def moveDir(curH: Coord, curT: Coord, moveDir: (Int, Int) => Int, horizontal: Boolean): (Coord, Coord) =
      if (checkDist(curH, curT)) (curH, curT) else {
        if (horizontal) {
          (curH, Coord(moveDir(curH.x, 1), curH.y))
        } else {
          (curH, Coord(curH.x, moveDir(curH.y, 1)))
        }
      }

    private def moveOneStep(curH: Coord, curT: Coord, d: Direction): (Coord, Coord) =
      d match {
        case L => moveDir(curH.move(L), curT, _ + _, true)
        case R => moveDir(curH.move(R), curT, _ - _, true)
        case U => moveDir(curH.move(U), curT, _ - _, false)
        case D => moveDir(curH.move(D), curT, _ + _, false)
      }

def parse(s: String): (Direction, Int) =
  s match {
    case s"R ${num}" => (R, num.toInt)
    case s"L ${num}" => (L, num.toInt)
    case s"U ${num}" => (U, num.toInt)
    case s"D ${num}" => (D, num.toInt)
  }

object AOC9 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc09.txt").getLines.toList.map(parse)

  val lilLilSnake = LilSnake(List(Coord(0,0), Coord(0,0)))
  val bigLilSnake = LilSnake(List(Coord(0,0), Coord(0,0), Coord(0,0), Coord(0,0), Coord(0,0), Coord(0,0), Coord(0,0), Coord(0,0), Coord(0,0), Coord(0,0)))

  println(s"Answer to part 1: ${lilLilSnake.slither(input)}")
  println(s"Answer to part 2: ${bigLilSnake.slither(input)}")