package aoc2022

import scala.io.Source

def checkFullOverlap(r1: Range, r2: Range): Boolean = 
  val overlap = r1.intersect(r2)
  overlap == r1 | overlap == r2

def checkAnyOverlap(r1: Range, r2: Range): Boolean = 
  r1.intersect(r2).nonEmpty

object AOC4 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc4.txt")
                    .getLines
                    .map{ case s"${e1}-${e2},${e3}-${e4}" => (Range.inclusive(e1.toInt, e2.toInt), Range.inclusive(e3.toInt, e4.toInt))}.toList
  
  val answerP1 = input.map(checkFullOverlap).count(_ == true)

  val answerP2 = input.map(checkAnyOverlap).count(_ == true)

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)
