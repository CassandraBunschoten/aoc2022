package aoc2022

import scala.io.Source

sealed trait Instruction
case object Noop extends Instruction
case class Addx(num: Int, cycleCount: Int = 2) extends Instruction

def parseInstructions(input: String): Instruction =
  input match {
    case "noop"         => Noop
    case s"addx ${num}" => Addx(num.toInt)
  }

case class CPU(instructions: List[Instruction], register: Int = 1, cycle: Int = 0):
  val signalStrength: Int =
    register * (cycle + 1)

  val spritePosition: List[Int] =
    val middle = register % 40
    List(middle - 1, middle, middle + 1)

  def drawPixel: Char =
    if (spritePosition.contains(this.cycle % 40)) '#' else '.'

  def nextCycle: CPU = {
    instructions match {
      case Noop :: t          => CPU(t, register, cycle + 1)
      case Addx(num, 2) :: t  => CPU(Addx(num, 1) :: t, register, cycle + 1)
      case Addx(num, 1) :: t  => CPU(t, register + num, cycle + 1)
    }
  }

def getSignalStrengths(cpu: CPU, acc: Int): Int =
  val solutionCycles = List(20, 60, 100, 140, 180, 220).map(_ - 1)

  if (cpu.cycle > solutionCycles.max) {
    acc
  } else if (solutionCycles.contains(cpu.cycle)) {
      getSignalStrengths(cpu.nextCycle, acc + cpu.signalStrength)
  } else {
      getSignalStrengths(cpu.nextCycle, acc)
    }

def drawOnCRT(cpu: CPU, acc: List[Char]): String = {
  if (cpu.instructions.isEmpty) {
    acc.grouped(40).toList.map(x => x.mkString + "\n").mkString
  } else {
    drawOnCRT(cpu.nextCycle, acc :+ cpu.drawPixel)
  }
}

object AO10 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc10.txt").getLines.map(parseInstructions).toList

  println(s"Answer to part 1: ${getSignalStrengths(CPU(input), 0)}")
  println(s"Answer to part 2: \n ${drawOnCRT(CPU(input), List[Char]())}")

