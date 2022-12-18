package aoc12

import scala.io.Source

case class Point(x: Int, y: Int, z: Int)

def getValue(ch: Char) =
  (('a' to 'z') ++ ('A' to 'Z')).indexOf(ch)

def getNeighbours(points: List[Point], currentPoint: Point) = {
  val neighs = List((currentPoint.x, currentPoint.y - 1),
                     (currentPoint.x, currentPoint.y + 1),
                     (currentPoint.x - 1, currentPoint.y),
                     (currentPoint.x + 1, currentPoint.y))

  points.filter{ case Point(x, y, z) => (currentPoint.z - 1 <= z && neighs.contains((x, y)))}
}

def breadthFirstSearch(goal: Point, points: List[Point], queue: List[(Int, Point)], visited: List[(Int, Point)]): Int = {
  val (dist, point)  = queue.head
  val newVisited     = queue.head :: visited
  val add            = getNeighbours(points, point).filterNot(p => newVisited.exists(_._2 == p) || queue.exists(_._2 == p))  
  if (add.exists(_  == goal)) { dist + 1 } else {
    val newQueue = (queue ::: add.map(p => (dist + 1, p)))
    breadthFirstSearch(goal, points, newQueue.tail, newVisited)
  }
}

def breadthFirstSearch(goal: Int, points: List[Point], queue: List[(Int, Point)], visited: List[(Int, Point)]): Int = {
  val (dist, point)  = queue.head
  val newVisited     = queue.head :: visited
  val add            = getNeighbours(points, point).filterNot(p => newVisited.exists(_._2 == p) || queue.exists(_._2 == p))  
  if (add.exists(_.z == goal)) { dist + 1 } else {
    val newQueue = (queue ::: add.map(p => (dist + 1, p)))
    breadthFirstSearch(goal, points, newQueue.tail, newVisited)
  }
}

object AOC12 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc12.txt")
    .getLines
    .zipWithIndex
    .flatMap{case (l, y) => l.zipWithIndex.map { case (z, x) => Point(x, y, getValue(z))}}
    .toList
  
  val startPoint = input.find(_.z == 44).get
  val endPoint   = input.find(_.z == 30).get

  val finalInput = input.updated(input.indexOf(startPoint), startPoint.copy(z = 0)).updated(input.indexOf(endPoint), endPoint.copy(z = 25))

  val answerP1 = breadthFirstSearch(startPoint.copy(z = 0), finalInput, List((0, endPoint.copy(z = 25))), List[(Int, Point)]())

  val answerP2 = breadthFirstSearch(0, finalInput, List((0, endPoint.copy(z = 25))), List[(Int, Point)]())

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)