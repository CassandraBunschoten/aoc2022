package aoc2022

import scala.io.Source

case class Pairs(p1: String, p2: String, num: Int):
  def compareBraces: (Boolean, Int) =
    println((compareStack(this.p1.toList, this.p2.toList), this.num)) ; (compareStack(this.p1.toList, this.p2.toList), this.num)

def compareStack(s1: List[Char], s2: List[Char]): Boolean = {
    println(s"STACK 1: $s1 \n STACK 2: $s2 \n\n")
    if (s1.isEmpty && s2.nonEmpty) { true } else if (s2.isEmpty) {false} else {
      (s1, s2) match {
        case (h1 :: t1, h2 :: t2) if (h1 == h2) => compareStack(t1, t2)
        case (']' :: t1, h2 :: t2)              => true
        case (h1 :: t1, ']' :: t2)              => false
        case ('[' :: t1, h2 :: t2)              => compareStack(t1, h2 :: ']' :: t2)
        case (h1 :: t1, '[' :: t2)              => compareStack(h1 :: ']' :: t1, t2)
        case (n1 :: t1, n2 :: t2)               => compareNumber(n1, n2)
        case _                                  => sys.error("booboobooboo")
      }
    }
  }

def compareNumber(n1: Char, n2: Char): Boolean = 
    n1.toInt < n2.toInt

object AOC13 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc13.txt")
              .mkString
              .split("\n\n")
              .map(x => x.replace("10", ":").filterNot(x => x == ',').trim.split("\n")).zipWithIndex.map{ case (p, i) => Pairs(p(0), p(1), i + 1)}.toList

  val input2 = (Source.fromFile("src/main/resources/input_aoc13.txt")
              .getLines
              .filterNot(_.isBlank)
              .map(x => x.replace("10", ":").filterNot(x => x == ',')).toList :+ "[[2]]" :+ "[[6]]").sortWith{ case (x, y) => compareStack(x.toList, y.toList)}

  val index1 = input2.indexOf("[[2]]") + 1
  val index2 = input2.indexOf("[[6]]") + 1

  println(index1 * index2)
  // println(input)