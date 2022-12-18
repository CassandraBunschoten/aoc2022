package aoc16

import scala.io.Source

case class Valve(name: String, rate: Int, tunnels: List[String])

def breadthFirstSearch(from: String, goal: String, neighbours: Map[String,List[String]], queue: List[(Int, String)], visited: List[(Int, String)] = List[(Int, String)]()): ((String, String), Int) = {
  val (dist, valve)  = queue.head
  val newVisited     = queue.head :: visited
  val add            = neighbours(valve).filterNot(p => newVisited.exists(_._2 == p) || queue.exists(_._2 == p))
  if (add.exists(_  == goal)) {((from, goal), dist + 1) } else {
    val newQueue = (queue ::: add.map(p => (dist + 1, p)))
    breadthFirstSearch(from, goal, neighbours, newQueue.tail, newVisited)
  }
}
def shortestPathMatrix(valves: List[Valve], neighbours: Map[String, List[String]]) =
  valves.foldLeft(Map[(String, String), Int]())((mat, rv) => mat ++ valves.filterNot(_ == rv).map(rv2 => breadthFirstSearch(rv.name, rv2.name, neighbours, List((1, rv.name)))))

def pressureReleasePaths(shortPaths: Map[(String,String),Int], pressures: Map[String,Int], remaining: List[String], timeLimit: Int, currentTunnel: String = "AA", currentPressure: Int = 0, currentFlow: Int = 0, currentTime: Int = 0): Int =
  val curEndScore = currentPressure + (timeLimit - currentTime) * currentFlow
  remaining.foldLeft(curEndScore)((max, valve) =>
    val distanceAndOpen = shortPaths((currentTunnel, valve))
    if ((currentTime + distanceAndOpen) < timeLimit) {
      val newPressure   = currentPressure + (distanceAndOpen * currentFlow)
      val newFlow       = currentFlow + pressures(valve)
      val newTime       = currentTime + distanceAndOpen
      val possibleScore = pressureReleasePaths(shortPaths, pressures, remaining.filter(_ != valve), timeLimit, valve, newPressure, newFlow, newTime)
      if (possibleScore > max) {
        possibleScore
      } else
        max
    }
    else max
  )

def elephantTeamWork(shortPaths: Map[(String,String),Int], pressure: Map[String,Int], valves: List[String]): Int = {
  valves.combinations(8).foldLeft(0)((max, me) => {
    val maxMe       = pressureReleasePaths(shortPaths, pressure, me, 26)
    val elephant    = valves.filterNot(v => me.contains(v))
    val maxElephant = pressureReleasePaths(shortPaths, pressure, elephant, 26)
    if (maxMe + maxElephant > max) {
      maxMe + maxElephant
    } else { max }
  }
  )
}

object AOC16 extends App:
  val valves: List[Valve] = Source.fromFile("src/main/resources/input_aoc16.txt")
                                  .getLines
                                  .toList
                                  .map {
                                    case s"Valve $name has flow rate=$rate; tunnels lead to valves $tunnels" =>
                                      Valve(name, rate.toInt, tunnels.trim.split(',').map(_.trim).toList)
                                    case s"Valve $name has flow rate=$rate; tunnel leads to valve $tunnel" =>
                                      Valve(name, rate.toInt, List(tunnel))
                                  }

  val neighbours: Map[String,List[String]] = valves.map(v => v.name -> v.tunnels).toMap
  val relevantValves: List[Valve] = valves.filter(_.rate != 0)
  val pressure: Map[String,Int] = valves.map(v => v.name -> v.rate).toMap

  val relevantValvesPaths = shortestPathMatrix(valves.filter(v => v.name == "AA").head :: relevantValves, neighbours = neighbours)
  val relValveNames = relevantValves.map(_.name)

  val answerP1 = pressureReleasePaths(relevantValvesPaths, pressure, relValveNames, 30)

  val answerP2 = elephantTeamWork(relevantValvesPaths, pressure, relValveNames)

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)


