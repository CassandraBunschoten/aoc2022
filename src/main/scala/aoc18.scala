package aoc18

import scala.io.Source

case class Droplet(x: Int, y: Int, z: Int):
  val surrounding =
    List((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)).map{case (x1,y1,z1) => (x + x1, y + y1, z + z1)}

  def countSides(droplets: List[Droplet]) =
    6 - droplets.map(d => this.surrounding.filter(s => (d.x, d.y, d.z) == s)).filterNot(_.isEmpty).distinct.size

def surroundingCube(droplets: List[Droplet]): (Droplet, Droplet) =
  val a = droplets.map{case Droplet(x, _, _) => (x)}
  val b = droplets.map{case Droplet(_, y, _) => (y)}
  val c = droplets.map{case Droplet(_, _, z) => (z)}
  (Droplet(a.min - 1, b.min - 1, c.min - 1), Droplet(a.max + 1, b.max + 1, c.max + 1))

def floodOutside(searchDrop: Droplet,
                 toVisit: List[Droplet],
                 visited: List[Droplet],
                 allDroplet: List[Droplet],
                 bubble: List[Droplet],
                 minDroplet: Droplet,
                 maxDroplet: Droplet): List[Droplet] =
  if (allDroplet.contains(searchDrop)) {
    toVisit match {
      case h :: t => floodOutside(h, t, searchDrop :: visited, allDroplet, bubble, minDroplet, maxDroplet)
      case Nil    => bubble
    }
  } else {
    val searchNeighbours = searchDrop.surrounding.map({case (x,y,z) => Droplet(x,y,z)})
                           .filterNot(x => (searchDrop :: visited).contains(x))
                           .filterNot(x => outsideEdges(x, minDroplet, maxDroplet))
                           .filterNot(x => allDroplet.contains(x))
    val newToVisit = (toVisit ::: searchNeighbours).distinct
    newToVisit match {
      case h :: t =>  floodOutside(newToVisit.head, newToVisit.tail, searchDrop :: visited, allDroplet, searchDrop :: bubble, minDroplet, maxDroplet)
      case Nil    => searchDrop :: bubble
    }
     }

def outsideEdges(d: Droplet, minDroplet: Droplet, maxDroplet: Droplet): Boolean =
  (d.x < minDroplet.x || d.x > maxDroplet.x) || (d.y < minDroplet.y || d.y > maxDroplet.y) || (d.z < minDroplet.z || d.z > maxDroplet.z)

object AOC18 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc18.txt")
              .getLines
              .map{case s"$x,$y,$z" => Droplet(x.toInt, y.toInt, z.toInt)}.toList

  val (outsideMin, outsideMax) = surroundingCube(input)

  val steam = floodOutside(outsideMin, List[Droplet](), List[Droplet](), input, List[Droplet](), outsideMin, outsideMax)

  val answerP1 = input.map(d => d.countSides(input.filterNot(_ == d))).sum

  val answerP2 = input.map(d => 6 - d.countSides(steam)).sum

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)