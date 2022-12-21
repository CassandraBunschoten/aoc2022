package aoc21
import scala.io.Source
import scala.collection.mutable._
import scala.annotation.newMain


type Monkey = String
type Numbers = Map[Monkey, Long]
type Formulas = Map[Monkey, (Char, Monkey, Monkey)]

def parse: (Numbers, Formulas) = {
  val numbers = scala.collection.mutable.Map[Monkey, Long]()
  val formulas = scala.collection.mutable.Map[Monkey, (Char, Monkey, Monkey)]()

  Source.fromResource("input_aoc21.txt")
   .getLines
   .foreach { l => 
    l match {
      case s"$name: $monkeyName1 + $monkeyName2" => formulas += (name -> ('+', monkeyName1, monkeyName2))
      case s"$name: $monkeyName1 - $monkeyName2" => formulas += (name -> ('-', monkeyName1, monkeyName2))
      case s"$name: $monkeyName1 / $monkeyName2" => formulas += (name -> ('/', monkeyName1, monkeyName2))
      case s"$name: $monkeyName1 * $monkeyName2" => formulas += (name -> ('*', monkeyName1, monkeyName2))
      case s"$name: $number"                     => numbers  += (name -> number.toLong) 
   }}

  (numbers, formulas)
}

def evaluate(numbers: Numbers, formulas: Formulas): (Numbers, Formulas) = {
  var formulaSize = formulas.size
  formulas.foreach { 
    case (monkey, (op, lhs, rhs)) => 
      val neigh1 = numbers.get(lhs)
      val neigh2 = numbers.get(rhs)

      if (neigh1.isDefined && neigh2.isDefined) {
        val number = (op, neigh1, neigh2) match {
          case ('+', Some(l), Some(r)) => l + r
          case ('-', Some(l), Some(r)) => l - r
          case ('*', Some(l), Some(r)) => l * r
          case ('/', Some(l), Some(r)) => l / r 
          case _ => sys.error("booboo")
        }
        numbers += (monkey -> number)
        formulas -= (monkey)
      }
      else ()
    }
  if (formulas.size == formulaSize)
    (numbers, formulas) 
  else 
  evaluate(numbers, formulas)
}

def findShout(numbers: Numbers, formulas: Formulas, search: Monkey = "root", result: Long = 0L): Long = 
 if (search != "humn") {
    val (op, lhs, rhs) = formulas(search)
    val (newSearch, newResult) = (op, numbers.get(lhs), numbers.get(rhs)) match {
      case ('=', None, Some(x)) => (lhs, x)
      case ('=', Some(x), None) => (rhs, x)
      case ('+', None, Some(x)) => (lhs, result - x)
      case ('+', Some(x), None) => (rhs, result - x)
      case ('-', None, Some(x)) => (lhs, result + x)
      case ('-', Some(x), None) => (rhs, x - result)
      case ('*', None, Some(x)) => (lhs, result / x)
      case ('*', Some(x), None) => (rhs, result / x)
      case ('/', None, Some(x)) => (lhs, result * x)
      case ('/', Some(x), None) => (rhs, x / result)
      case _ => sys.error("booooo")
    }  
    findShout(numbers, formulas, newSearch, newResult)
 } else {
  result
 }
  

object AOC21 extends App:
  val (numbersP1, formulasP1) = parse

  val answerP1 = evaluate(numbersP1, formulasP1)._1("root")

  val (numbersP2, formulasP2) = parse
  val newFormulas = formulasP2.map { case (monkey, formula) => if (monkey == "root") ("root" -> ('=', formula._2, formula._3)) else (monkey, formula)}
  val newNumbers = numbersP2 -= "humn"
  val (evaluatedNumbers, evaluatedFormulas) = evaluate(newNumbers, newFormulas)

  val answerP2 = findShout(evaluatedNumbers, evaluatedFormulas)

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)

