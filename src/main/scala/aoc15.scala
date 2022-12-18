package aoc15

import scala.io.Source

def parse(l: String) =
  l match {
    case s"Sensor at x=$x1, y=$y1: closest beacon is at x=${x2}, y=${y2}" => SensorBeaconPair(sensor = Coord(x1.toInt, y1.toInt), beacon = Coord(x2.toInt, y2.toInt))
    case _  => sys.error("uhoh")
  }

case class Interval(lower: Coord, upper: Coord)

case class Diamond(lowerX: Int, upperX: Int, lowerY: Int, upperY: Int)

case class Coord(x: Int, y: Int):
  def manhattansDistance(coord2: Coord): Int =
    math.abs(this.x - coord2.x) + math.abs(this.y - coord2.y)

case class SensorBeaconPair(sensor: Coord, beacon: Coord):
  val manDist = sensor.manhattansDistance(beacon)
  def findIntervalForRow(row: Int): Option[Interval] = {
    val colInt = Interval(Coord(sensor.x, sensor.y - manDist), Coord(sensor.x, sensor.y + manDist))

    def intervalForManDistance: Interval = {
      val dist = manDist - math.abs(row - this.sensor.y)
      Interval(Coord(sensor.x - dist, row), Coord(sensor.x + dist, row))
    }

    if (row <= colInt.upper.y && row >= colInt.lower.y) {
      Some(intervalForManDistance)
    } else {
      None
    }
  }

var answer: (Int, Int) = (-1, -1)
def findHoleInRow(li: List[SensorBeaconPair], row: Int, rowMax: Int): Unit =
  val intervalsRow = li.foldLeft(List[Option[Interval]]())((acc, sbp) => sbp.findIntervalForRow(row) :: acc).filter(oi => oi.isDefined).map(_.get)

  val sortedRanges = intervalsRow.sortBy(x => x.lower.x)

  sortedRanges.foldLeft(0)((acc, x) => if (x.lower.x > acc + 1) {answer = (acc + 1, row) ; math.max(acc, x.upper.x)} else {math.max(acc, x.upper.x)})
  if (row < rowMax) {
    findHoleInRow(li, row + 1, rowMax)
  } else println("bloop")

object AOC15 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc15.txt").getLines.map(parse).toList

  val intervalsP1 = input.foldLeft(List[Option[Interval]]())((acc, sbp) => (sbp.findIntervalForRow(2000000) :: acc)).filter(_.isDefined).distinct

  val beacons = input.filter(x => x.beacon.y == 2000000).map(b => b.beacon).distinct.size
  val lowerX = intervalsP1.map(i => i.get.lower.x).min
  val upperX = intervalsP1.map(i => i.get.upper.x).max

  val answerP1 = if (lowerX < 0 ) {(upperX - lowerX) - beacons + 1} else {(upperX - lowerX) - beacons}

  val answerP2 = (findHoleInRow(input, 0, 4000000))

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answer)
