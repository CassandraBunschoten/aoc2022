package aoc25

import scala.io.Source

def parseChar(c: Char): Int =
  c match
    case '=' => -2
    case '-' => -1
    case n   => n.toString.toInt

def toSNAFU(dec: Long, acc: List[String] = List()): String =
  val curRem = dec % 5

  val (newAcc, newDec) = curRem match
    case 0 | 1 | 2 => (curRem.toString :: acc, dec / 5)
    case 3         => ("=" :: acc, (dec + 2) / 5)
    case 4         => ("-" :: acc, (dec + 1) / 5)

  if (newDec == 0) {
    newAcc.flatten.mkString
  } else {
    toSNAFU(newDec, newAcc)
  }

object AOC25 extends App:
  val answerInDecimal = Source.fromResource("input_aoc25.txt")
                        .getLines
                        .map(s => s.reverse.zipWithIndex.map{case (c, i) => math.pow(5, i) * parseChar(c)}.sum).sum.toLong

  val answer = toSNAFU(answerInDecimal)

  println("Answer to final part: " + answer)

