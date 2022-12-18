package aoc01

import scala.io.Source

object AOC1 extends App:
    val outputP1 = Source
        .fromFile("src/main/resources/input_aoc01.txt")
        .mkString
        .split("\n\n")
        .map(_.split("\n").map(_.toInt))
        .map(_.sum)

    val answerP2 = outputP1.sorted.takeRight(3).sum


    println("Answer to part 1: " + outputP1.max)

    println("Answer to part 2: " + answerP2)
