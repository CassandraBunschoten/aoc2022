package aoc2022

import aoc2022.AO11.input

import scala.io.Source

case class Monkey(name: Int, items: List[BigInt], operation: (BigInt, BigInt) => BigInt, opNum: Option[BigInt], divisible: Int, testTrue: Int, testFalse: Int, tracker: BigInt)

def parseMonkey(input: List[String], monkey: Monkey = Monkey(0, List[BigInt](), _ + _, None, 0, 0, 0, 0)): Monkey =
  input match {
    case s"Monkey ${name}:"  :: t                   => parseMonkey(t, monkey.copy(name = name.toInt))
    case s"Starting items: ${worryLevel}" :: t      => { val worryItems = worryLevel.split(",").map(x => BigInt(x.trim.toInt)).toList
                                                    parseMonkey(t, monkey.copy(items =  worryItems))}
    case s"Operation: new = old + ${num}" :: t      =>  { if (num.head.isLetter){ parseMonkey(t, monkey.copy(operation = (_ + _), opNum = None))
                                                      } else { parseMonkey(t, monkey.copy(operation = (_ + _), opNum = Some(num.toInt)))}}
    case s"Operation: new = old * ${num}" :: t      =>  { if (num.head.isLetter){ parseMonkey(t, monkey.copy(operation = (_ * _), opNum = None))
                                                      } else { parseMonkey(t, monkey.copy(operation = (_ * _), opNum = Some(num.toInt)))}}
    case s"Test: divisible by ${test}" :: t         => parseMonkey(t, monkey.copy(divisible = test.toInt))
    case s"If true: throw to monkey ${name}" :: t   => parseMonkey(t, monkey.copy(testTrue = name.toInt))
    case s"If false: throw to monkey ${name}" :: t  => parseMonkey(t, monkey.copy(testFalse = name.toInt))
    case _                                          => monkey
  }

def shenanigans(monkeys: List[Monkey], curMonkey: Int, op: (BigInt, BigInt) => BigInt, num: Int): List[Monkey] = {
  if (curMonkey > monkeys.map(x => x.name).max) monkeys else {
    monkeys(curMonkey).items match
      case Nil => shenanigans(monkeys, curMonkey + 1, op, num)
      case _   => shenanigans(yeetItem(monkeys(curMonkey), monkeys, op, num), curMonkey, op, num)
  }
}

def yeetItem(m: Monkey, allMonkeys: List[Monkey], op: (BigInt, BigInt) => BigInt, num: Int): List[Monkey] = {
  val newMonkeys = allMonkeys.updated(m.name, allMonkeys(m.name).copy(items = m.items.tail, tracker = m.tracker + 1))
  if (m.opNum.isDefined) {
    val newWorry = op(m.operation(m.items.head, m.opNum.get), num)
    if (newWorry % m.divisible == 0) {
      newMonkeys.updated(m.testTrue, newMonkeys(m.testTrue).copy(items = newMonkeys(m.testTrue).items :+ newWorry))
    }
    else {
      {
        newMonkeys.updated(m.testFalse, newMonkeys(m.testFalse).copy(items = newMonkeys(m.testFalse).items :+ newWorry))
      }
    }
  } else {
    val newWorry = op(m.operation(m.items.head, m.items.head), num)
    if (newWorry % m.divisible == 0) {
      newMonkeys.updated(m.testTrue, newMonkeys(m.testTrue).copy(items = newMonkeys(m.testTrue).items :+ newWorry))
    }
    else {
      {
        newMonkeys.updated(m.testFalse, newMonkeys(m.testFalse).copy(items = newMonkeys(m.testFalse).items :+ newWorry))
      }
    }
  }
}

def answer(monkeys: List[Monkey], rounds: Int, op: (BigInt, BigInt) => BigInt, number: Int) = {
  val max = (1 to rounds).foldLeft(monkeys)((a, _) => shenanigans(a, 0, op, number)).map(x => x.tracker)

  max.sorted.reverse.take(2).product
}

object AO11 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc11.txt")
              .mkString
              .split("\n\n")
              .toList
              .map(x => x.split("\n").toList.map(x => x.trim()))
              .map(m => parseMonkey(m))

  val lcm = input.map(_.divisible).product

  println(s"Answer to part 1: ${answer(input, 20, (_ / _), 3)}")
  println(s"Answer to part 2: ${answer(input, 10000, (_ % _), lcm)}")
