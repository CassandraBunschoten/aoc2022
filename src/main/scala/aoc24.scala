package aoc24

import scala.io.Source
import scala.collection._

case class Coord(x: Int, y: Int):
  def neighboursAndSelf =
    List(Coord(x + 1, y + 0), Coord(x -1, y + 0), Coord(x + 0, y + 1), Coord(x + 0, y -1), this)

case class Bounds(xMin: Int, xMax: Int, yMin: Int, yMax: Int):
  def checkBounds(c: Coord): Boolean =
    (c.x < xMax + 1 && c.x > xMin && c.y > yMin - 1 && c.y < yMax + 1)

def addOrUpdateVList(m: mutable.Map[Coord, List[Char]], k: Coord, kv: (Coord, List[Char]), v: Char) =
  m.get(k) match {
    case Some(lc) => if (v == '.') m else {m.update(k, v :: lc)}
    case None     => m += kv
  }

def makeNewMapState(currentMap: mutable.Map[Coord, List[Char]], bounds: Bounds): mutable.Map[Coord, List[Char]] =
  val toUpdate = currentMap.map{case (coord, chars) => chars.map(c => (c, coord))}.flatten.toList
  val updated = toUpdate.map{case (char, coord) => moveBlizzard(char, coord, bounds)}
  val newMap = mutable.Map[Coord, List[Char]]()

  def rec(m: mutable.Map[Coord, List[Char]], l: List[(Char, Coord)]): mutable.Map[Coord, List[Char]] =
    l match {
      case (ch, crd) :: t => addOrUpdateVList(m, crd, (crd, List(ch)), ch); rec(m, t)
      case Nil => m
    }

  rec(newMap, updated)
  newMap

def moveBlizzard(b: Char, coord: Coord, bounds: Bounds): (Char, Coord) =
  val (xMin, xMax, yMin, yMax) = (bounds.xMin, bounds.xMax, bounds.yMin, bounds.yMax)
  b match {
    case '^' => (b, if (coord.y - 1 <= yMin && coord.x != xMin + 1) {
                coord.copy(y = yMax - 1)
                } else if (coord.y - 1 == yMin && coord.x == xMax - 1) {
                coord.copy(y = yMax)
                } else {coord.copy(y = coord.y - 1)})
    case 'v' => (b, if (coord.y + 1 == yMax && coord.x != xMin + 1) {
                coord.copy(y = yMin + 1)
                } else if (coord.y + 1 >= yMax && coord.x == xMin + 1) {
                coord.copy(y = yMin)}
                else {coord.copy(y = coord.y + 1)})
    case '>' => (b, if(coord.x + 1 == xMax) {coord.copy(x = xMin + 1)} else {coord.copy(x = coord.x + 1)})
    case '<' => (b, if (coord.x - 1 == xMin) {coord.copy(x = xMax - 1)} else {coord.copy(x = coord.x - 1)})
    case '#' => (b, coord)
    case _   => sys.error("booboo")
  }

def breadthFirstSearch(goal: Coord,
                       mapState: mutable.Map[Coord,List[Char]],
                       queue: List[(Coord, Int)],
                       time: Int,
                       bounds: Bounds,
                       visited: List[(Coord, Int)] = List[(Coord, Int)]()): (Int, mutable.Map[Coord,List[Char]]) =
  val (coord, timeQueue)  = queue.head
  val newVisited     = queue.head :: visited
  val (maybeNewMap, maybeNewTime) = if (timeQueue > time) {
    (makeNewMapState(mapState, bounds), timeQueue)
  } else {(mapState, time)}
  val add            = coord.neighboursAndSelf
                            .filterNot(c => maybeNewMap.contains(c))
                            .filterNot(c => queue.contains((c, maybeNewTime + 1)))
                            .filter(c => bounds.checkBounds(c))
  if (add.contains(goal)) {(maybeNewTime + 1, maybeNewMap)} else {
    val newQueue = queue.tail ::: add.map(c => (c, maybeNewTime + 1))
    breadthFirstSearch(goal, maybeNewMap, newQueue, maybeNewTime, bounds, newVisited)
  }

object AOC24 extends App:
  val input = Source.fromResource("input_aoc24.txt")
                    .getLines
                    .map(x => x.zipWithIndex.toList)
                    .zipWithIndex.flatMap(xy => xy._1.filterNot{case (c, _) => c == '.'}.map{case (c, x) => Coord(x, xy._2) -> List(c)})
                    .toList
                    .to(mutable.Map)

  val startKeys = input.keys
  val xMin = startKeys.map(xy => xy.x).min
  val xMax = startKeys.map(xy => xy.x).max
  val yMin = startKeys.map(xy => xy.y).min
  val yMax = startKeys.map(xy => xy.y).max
  val bounds = Bounds(xMin, xMax, yMin, yMax)

  val (answerP1, mapOne) = breadthFirstSearch(Coord(xMax - 1, yMax), input, List((Coord(xMin + 1, yMin), 0)), -1, bounds)
  val (timeTwo, mapTwo) = breadthFirstSearch(Coord(xMin + 1, yMin), mapOne, List((Coord(xMax - 1, yMax), answerP1 + 1)), answerP1, bounds)
  val (answerP2, mapThree) = breadthFirstSearch(Coord(xMax - 1, yMax), mapTwo, List((Coord(xMin + 1, yMin), timeTwo + 1)), timeTwo, bounds)

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)