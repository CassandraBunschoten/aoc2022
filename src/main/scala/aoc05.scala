package aoc05

import scala.io.Source

case class Stack(crates: List[Char])

case class Move(many: Int, p1: Int, p2: Int):
  def oneMove(stacks: List[Stack], reverse: Boolean): List[Stack] = 
    val loc = this.p1 - 1
    val loc2 = this.p2 - 1
    val (moveStack, newStack) = { val curStack = stacks((loc)).crates
                                  val removed = curStack.take(this.many)
                                  val smallerStack = Stack(curStack.drop(this.many))
                                  if (reverse) (removed.reverse, smallerStack) else (removed, smallerStack)
  }
    stacks.updated((loc), newStack).updated((loc2), Stack(moveStack ++ stacks(loc2).crates))


def moves(stacks: List[Stack], moveList: List[Move], reverse: Boolean): List[Stack] = 
  moveList match {
    case move :: restMoves => moves(move.oneMove(stacks, reverse), moveList.takeRight(moveList.size - 1), reverse)
    case Nil => stacks
  }

def getResult(stacks: List[Stack]): String = 
  stacks.map(stack => stack.crates.head).mkString

object AOC5 extends App:
  val List(boxes, moveList) = Source.fromFile("src/main/resources/input_aoc05.txt").mkString.split("\n\n").toList

   val finalBoxes = boxes
                    .split("\n")
                    .map(_.toList)
                    .dropRight(1)
                    .toList
                    .transpose
                    .filter(row => row.exists(x => x.isLetter))
                    .map(stack => Stack(stack.filter(_.isLetter)))

   val finalMoves = moveList
                    .split('\n')
                    .map{case s"move ${many} from ${p1} to ${p2}" => Move(many.toInt, p1.toInt, p2.toInt)}
                    .toList

  println("Answer to part 1: " + getResult(moves(finalBoxes, finalMoves, true)))
  println("Answer to part 2: " + getResult(moves(finalBoxes, finalMoves, false)))
