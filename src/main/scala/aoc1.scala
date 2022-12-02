package aoc2022 

import scala.io.Source

object AOC1 extends App:
    val input = Source
        .fromFile("src/main/resources/input_aoc1.txt").mkString
        
    val outputPart1 = input.split("\n\n").toList.map(_.split("\n").map(_.toInt).toList).filter(!_.isEmpty).map(_.sum)

    val outputPart2 = outputPart1.sorted.takeRight(3).sum


    println("Answer to part 1: " + outputPart1.max)

    println("Answer to part 2: " + outputPart2)
