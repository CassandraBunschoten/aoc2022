package aoc06

import scala.io.Source

object AOC6 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc06.txt").getLines.toList

  def findFirstDistinctGroup(inputList: List[String], window: Int) = 
    inputList
      .map(_.sliding(window)
      .zipWithIndex.filter{case (letters, index) => (letters.distinct.size == window)}
      .map{case (letters, index) => (index + window)}
      .min)
      .sum
  
  println("Answer to part 1: " + findFirstDistinctGroup(input, 4))
  println("Answer to part 2: " + findFirstDistinctGroup(input, 14))
