package aoc21

import scala.io.Source

sealed trait Monkey
case class NumberMonkey(name: String, number: Long) extends Monkey
case class MathMonkey(name: String, neighMonkey1: String, neighMonkey2: String, mathOp: (Long, Long) => Long) extends Monkey
//case class SpecialMonkey(name: String, neighMonkey1: String, neighMonkey2: String, equalOp: (Long, Long) => Boolean) extends Monkey
//// because evolution
//case class Human(num: Long = 0) extends Monkey

def lookForNumberMonkeys(nm: String, allMonkeys: List[Monkey]): Option[NumberMonkey] =
  allMonkeys match {
    case h :: t => h match {
      case n: NumberMonkey if (n.name == nm) => Some(n)
      case _                                 => lookForNumberMonkeys(nm, t)
    }
    case Nil                                 => None
  }

def updateMonkey(mathMonkey: MathMonkey, allMonkeys: List[Monkey]): Monkey =
  val nm1 = mathMonkey.neighMonkey1
  val nm2 = mathMonkey.neighMonkey2
  val opNeighMonkey1 = lookForNumberMonkeys(nm1, allMonkeys)
  val opNeighMonkey2 = lookForNumberMonkeys(nm2, allMonkeys)
  if (opNeighMonkey1.isDefined && opNeighMonkey2.isDefined) {
    val nm = NumberMonkey(mathMonkey.name, mathMonkey.mathOp(opNeighMonkey1.get.number, opNeighMonkey2.get.number))
    nm
  } else {
    mathMonkey
  }

//def updateMonkeyP2(mathMonkey: MathMonkey, allMonkeys: List[Monkey], human: Long): Monkey =
////  println(s"FOR $mathMonkey")
//  val nm1 = mathMonkey.neighMonkey1
//  val nm2 = mathMonkey.neighMonkey2
////  println(s"LOOK AT $nm1 AND $nm2")
//  val opNeighMonkey1 = lookForNumberMonkeys(nm1, allMonkeys)
//  val opNeighMonkey2 = lookForNumberMonkeys(nm2, allMonkeys)
////  println(s"FOUND $opNeighMonkey1, $opNeighMonkey2")
//  if (opNeighMonkey1.isDefined && opNeighMonkey2.isDefined) {
//    val nm = NumberMonkey(mathMonkey.name, mathMonkey.mathOp(opNeighMonkey1.get.number, opNeighMonkey2.get.number))
////    println(s"NEW MONKEY: $nm")
//    nm
//  } else if (nm1 == "humn" && opNeighMonkey2.isDefined){
//    val nm = NumberMonkey(mathMonkey.name, mathMonkey.mathOp(human, opNeighMonkey2.get.number))
////    println(s"NEW MONKEY WITH HUMAN: $nm")
//    nm
//  } else if (nm2 == "humn" && opNeighMonkey1.isDefined){
//    val nm = NumberMonkey(mathMonkey.name, mathMonkey.mathOp(opNeighMonkey1.get.number, human))
////    println(s"NEW MONKEY WITH HUMAN: $nm")
//    nm
//  } else {
////      println(s"NEW MONKEY: $mathMonkey")
//      mathMonkey
//  }

def goOverMonkeys(allMonkeys: List[Monkey], goal: String = "root"): Monkey =
  if (lookForNumberMonkeys(goal, allMonkeys).isDefined) {lookForNumberMonkeys(goal, allMonkeys).get} else {
  val newMonkeys = allMonkeys.scanLeft(allMonkeys)((updatedMonkeys, m) => m match {
    case m: MathMonkey => {
      val index = updatedMonkeys.indexOf(m)
      updatedMonkeys.updated(index, updateMonkey(m, updatedMonkeys))
    }
    case _ => updatedMonkeys
  })
  goOverMonkeys(newMonkeys.last)
}

//def goOverMonkeysP2(allMonkeys: List[Monkey], human: Long, neigh1: String, neigh2: String): (Long, Long) =
//  if (lookForNumberMonkeys(neigh1, allMonkeys).isDefined && lookForNumberMonkeys(neigh2, allMonkeys).isDefined)
//    {(lookForNumberMonkeys(neigh1, allMonkeys).get.number, lookForNumberMonkeys(neigh2, allMonkeys).get.number)} else {
//    val newMonkeys = allMonkeys.scanLeft(allMonkeys)((updatedMonkeys, m) => m match {
//      case m: MathMonkey => {
//        val index = updatedMonkeys.indexOf(m)
//        updatedMonkeys.updated(index, updateMonkeyP2(m, updatedMonkeys, human))
//      }
//      case _ => updatedMonkeys
//    })
//    goOverMonkeysP2(newMonkeys.last, human, neigh1, neigh2)
//  }
//
//def findHumanShout(allMonkeys: List[Monkey], num: Long, locationHuman: Int, neigh1: String, neigh2: String): Long = {
//  println(num)
//  val nextTry = allMonkeys.updated(locationHuman, Human(num))
//  val neighs = goOverMonkeysP2(nextTry, num, neigh1, neigh2)
//  val diff = neighs._1 - neighs._2
//  if (diff == 0) {num} else {
//    println(s"new num: ${num + diff}")
//    findHumanShout(allMonkeys, num + diff, locationHuman, neigh1, neigh2)
//  }
//}


object AOC21 extends App: 
  val input = Source.fromFile("src/main/resources/input_kat.txt")
                    .getLines
                    .map{l => l match {
                      case s"$name: $monkeyName1 + $monkeyName2" => MathMonkey(name, monkeyName1, monkeyName2, _+_)
                      case s"$name: $monkeyName1 - $monkeyName2" => MathMonkey(name, monkeyName1, monkeyName2, _-_)
                      case s"$name: $monkeyName1 / $monkeyName2" => MathMonkey(name, monkeyName1, monkeyName2, _/_)
                      case s"$name: $monkeyName1 * $monkeyName2" => MathMonkey(name, monkeyName1, monkeyName2, _*_)
                      case s"$name: $number"                     => NumberMonkey(name, number.toLong)
                    }}
                    .toList

//  val inputP2 = Source.fromFile("src/main/resources/input_example.txt")
//    .getLines
//    .map{l => l match {
//      case s"root: $monkeyName1 + $monkeyName2"  => SpecialMonkey("root", monkeyName1, monkeyName2, _==_)
//      case s"humn: $number"                      => Human()
//      case s"$name: $monkeyName1 + $monkeyName2" => MathMonkey(name, monkeyName1, monkeyName2, _+_)
//      case s"$name: $monkeyName1 - $monkeyName2" => MathMonkey(name, monkeyName1, monkeyName2, _-_)
//      case s"$name: $monkeyName1 / $monkeyName2" => MathMonkey(name, monkeyName1, monkeyName2, _/_)
//      case s"$name: $monkeyName1 * $monkeyName2" => MathMonkey(name, monkeyName1, monkeyName2, _*_)
//      case s"$name: $number" => NumberMonkey(name, number.toLong)
//    }}
//    .toList
//
//  val indexHuman = inputP2.indexOf(Human())


//  println(findHumanShout(inputP2, 0, indexHuman)) neigh1: String = "qntq", neigh2: String = "qgth"
//  println(findHumanShout(inputP2, 0, indexHuman, "pppw", "sjmn"))

//  println(updateMonkey(MathMonkey("drzm", "hmdt", "zczc", _ - _), input))

  println(goOverMonkeys(input))
