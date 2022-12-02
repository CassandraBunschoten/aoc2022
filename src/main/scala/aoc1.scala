package aoc2022 

import scala.io.Source

object AOC1 extends App:
    val outputPart1 = Source
        .fromFile("src/main/resources/input_aoc1.txt")
        .mkString
        .split("\n\n")
        .map(_.split("\n").map(_.toInt))
        .map(_.sum)

    val outputPart2 = outputPart1.sorted.takeRight(3).sum


    println("Answer to part 1: " + outputPart1.max)

    println("Answer to part 2: " + outputPart2)
