package aoc23

import scala.io.Source
import scala.collection.mutable._

trait CardinalPoint
case object North extends CardinalPoint
case object South extends CardinalPoint
case object West extends CardinalPoint
case object East extends CardinalPoint

type Proposal = Coord

case class Coord(x: Int, y: Int):
  def canMoveNorth(elves: List[Coord]): Boolean = List((-1, -1), (0, -1), (1, -1)).map{case (x1, y1) => Coord(x + x1, y + y1)}.intersect(elves).isEmpty
  def canMoveSouth(elves: List[Coord]): Boolean = List((-1, 1), (0, 1), (1, 1)).map{case (x1, y1) => Coord(x + x1, y + y1)}.intersect(elves).isEmpty
  def canMoveEast(elves: List[Coord]): Boolean = List((1, -1), (1, 0), (1, 1)).map{case (x1, y1) => Coord(x + x1, y + y1)}.intersect(elves).isEmpty
  def canMoveWest(elves: List[Coord]): Boolean = List((-1, -1), (-1, 0), (-1, 1)).map{case (x1, y1) => Coord(x + x1, y + y1)}.intersect(elves).isEmpty

  def canMove(elves: List[Coord]): Boolean =  List((1, 0), (-1, 0), (0, 1), (0, -1), (1,1), (-1, -1), (-1, 1), (1, -1))
                                                  .map{case (x1,y1) => Coord(x + x1, y + y1)}
                                                  .intersect(elves)
                                                  .nonEmpty

def nextCardinalPointOrder(current: List[CardinalPoint]): List[CardinalPoint] =
  current.tail :+ current.head

def proposal(instructions: List[CardinalPoint], elf: Coord, elves: List[Coord]): Option[(Proposal, Coord)] =
  if (elf.canMove(elves)) {
    instructions match {
      case North :: t => if (elf.canMoveNorth(elves)) Some(elf.copy(y = elf.y - 1), elf) else proposal(t, elf, elves)
      case South :: t => if (elf.canMoveSouth(elves)) Some(elf.copy(y = elf.y + 1), elf) else proposal(t, elf, elves)
      case East :: t  => if (elf.canMoveEast(elves)) Some(elf.copy(x = elf.x + 1), elf) else proposal(t, elf, elves)
      case West :: t  => if (elf.canMoveWest(elves)) Some(elf.copy(x = elf.x - 1), elf) else proposal(t, elf, elves)
      case Nil        => None
    }
  } else {
    None
  }

def proposals(instructions: List[CardinalPoint], elves: List[Coord]): List[(Proposal, Coord)] =
  elves.flatMap(e => proposal(instructions, e, elves))
       .groupBy{case (prop, _) => prop}
        .filter{case (_, v) => v.lengthCompare(1) == 0}.values
        .flatten
        .toList

def moveElves(instructions: List[CardinalPoint], elves: Map[Coord, Int], round: Int, goalRounds: Int): Map[Coord, Int] =
  if (round == goalRounds) {
    elves
  } else {
    val finalProposals = proposals(instructions, elves.keys.toList)
    val curCoord = finalProposals.map(x => x._2)
    val newCoord = finalProposals.map(x => x._1)

    val newElves = elves.filterNot{ case (k, _) =>  curCoord.contains(k)} ++= newCoord.map(n => (n -> 1))
    moveElves(nextCardinalPointOrder(instructions), newElves, round + 1, goalRounds)
  }

def moveElvesTillStop(instructions: List[CardinalPoint], elves: Map[Coord, Int], round: Int): Int =
  val finalProposals = proposals(instructions, elves.keys.toList)
  val curCoord = finalProposals.map(x => x._2)
  val newCoord = finalProposals.map(x => x._1)

  val newElves = elves.filterNot{ case (k, _) =>  curCoord.contains(k)} ++= newCoord.map(n => n -> 1)
  if (newElves == elves) {round} else {moveElvesTillStop(nextCardinalPointOrder(instructions), newElves, round + 1)
  }


object AOC23 extends App:
  val input = Source.fromResource("input_aoc23.txt")
                    .getLines
                    .map(x => x.zipWithIndex.toList)
                    .zipWithIndex.map(xy => xy._1.map(x => (x._1, (x._2, xy._2))))
                    .toList
                    .flatten
                    .filterNot{ case (e, _) => e == '.'}
                    .map{case (_, (x, y)) => (Coord(x, y), 1)}
                    .to(scala.collection.mutable.Map)

  val startInstructions = List(North, South, West, East)
  val elves = input.keys.toList

  val finalForm = moveElves(startInstructions, input, 0, 10).keys.toList

  val answerP1 = (finalForm.map(xy => xy.x).max - finalForm.map(xy => xy.x).min + 1) * (finalForm.map(xy => xy.y).max - finalForm.map(xy => xy.y).min + 1) - finalForm.size
  val answerP2 = moveElvesTillStop(startInstructions, input, 1)

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)
