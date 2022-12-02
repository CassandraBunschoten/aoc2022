package aoc2022 

import scala.io.Source

// Part 1

sealed trait Response
case object Rock extends Response
case object Paper extends Response
case object Scissors extends Response

def toResponse(input: String): Response = 
  input match {
    case "A" | "X"  => Rock
    case "B" | "Y"  => Paper
    case "C" | "Z" => Scissors 
  }

def toGameP1(opponent: String, me: String) = 
  GameP1(toResponse(opponent), toResponse(me))  

// PART 1
case class GameP1(opponent: Response, me: Response): 
  def getScore: Int = {
    this match {
      case GameP1(Rock, Rock)         => 3 + 1
      case GameP1(Rock, Paper)        => 6 + 2
      case GameP1(Rock, Scissors)     => 0 + 3
      case GameP1(Paper, Paper)       => 3 + 2
      case GameP1(Paper, Rock)        => 0 + 1
      case GameP1(Paper, Scissors)    => 6 + 3 
      case GameP1(Scissors, Scissors) => 3 + 3
      case GameP1(Scissors, Rock)     => 6 + 1
      case GameP1(Scissors, Paper)    => 0 + 2
      case _ => sys.error("booboo")
    }
  }

// PART 2

case class GameP2(opponent: Response, result: Result): 
   def getScoreP2: Int = {
    this match {
      case GameP2(Rock, Draw)     => 3 + 1
      case GameP2(Rock, Win)      => 6 + 2
      case GameP2(Rock, Lose)     => 0 + 3
      case GameP2(Paper, Draw)    => 3 + 2
      case GameP2(Paper, Lose)    => 0 + 1
      case GameP2(Paper, Win)     => 6 + 3 
      case GameP2(Scissors, Draw) => 3 + 3
      case GameP2(Scissors, Win)  => 6 + 1
      case GameP2(Scissors, Lose) => 0 + 2
      case _ => sys.error("booboo")
    }
  }

sealed trait Result
case object Lose extends Result
case object Draw extends Result
case object Win extends Result

def toResponsePart2(input: String): Response = 
  input match {
    case "A"  => Rock
    case "B"  => Paper
    case "C"  => Scissors
  }

def toResult(input: String): Result = 
  input match {
    case "X" => Lose
    case "Y" => Draw
    case "Z" => Win
  }

def toGameP2(opponent: String, result: String) = 
  GameP2(toResponse(opponent), toResult(result))

object AOC2 extends App {

  val input = Source
      .fromFile("src/main/resources/input_aoc2.txt")
      .getLines.map(_.split(" ")).toList

  val inputP1 = input
    .map(x => toGameP1(x(0), x(1)).getScore).sum

  val inputP2 = input
     .map(x => toGameP2(x(0), x(1)).getScoreP2).sum


  println("Answer to part 1: " + inputP1)
  println("Answer to part 2: " + inputP2)

}
