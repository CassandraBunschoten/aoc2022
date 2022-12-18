package aoc03

import scala.io.Source

def getDuplicate(l1: String, l2: String) =
      l1.map(ch => l2.find(_ == ch)).distinct.filter(_.isDefined).map(_.get)

def getDuplicateBy3(l1: String, l2: String, l3: String) =
   val compare2 =  getDuplicate(l1, l2).mkString
   getDuplicate(compare2, l3)

def getValue(ch: Char) = 
  (('a' to 'z') ++ ('A' to 'Z')).indexOf(ch) + 1

object AOC3 extends App:

  val input = Source.fromFile("src/main/resources/input_aoc03.txt").getLines.toList

  val answerP1 = input
                 .map(rs => rs.grouped(rs.size/2).toArray)
                 .toList
                 .map(c => getDuplicate(c(0), c(1)))
                 .map(ar => ar.map(ch => getValue(ch)).sum)
                 .sum

  val answerP2 = input
                 .grouped(3)
                 .map(rs => getDuplicateBy3(rs(0), rs(1), rs(2)))
                 .map(is => is.map(ch => getValue(ch)).sum)
                 .sum

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)
