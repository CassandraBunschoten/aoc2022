package aoc19

import scala.io.Source

case class Blueprint(index: Int, oreRobotOre: Int, clayRobotOre: Int, obsidianRobotOre: Int, obsidianRobotClay: Int, geodeRobotOre: Int, geodeRobotObsidian: Int):
  val highestOreCost = List(oreRobotOre, clayRobotOre, obsidianRobotOre, geodeRobotOre).max

case class MaterialStash(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0)

sealed trait Robot
case object OreRobot extends Robot
case object ClayRobot extends Robot
case object ObsidianRobot extends Robot
case object GeodeRobot extends Robot

sealed trait Action
case object DoNothing extends Action
case object BuildOreRobot extends Action
case object BuildClayRobot extends Action
case object BuildObsidianRobot extends Action
case object BuildGeodeRobot extends Action

def parseBlueprint(s: String): Blueprint = {
  s match {
    case s"Blueprint ${index}: Each ore robot costs ${oro} ore. Each clay robot costs ${cro} ore. Each obsidian robot costs ${obro} ore and ${obrc} clay. Each geode robot costs ${gror} ore and ${grob} obsidian." => Blueprint(index.toInt, oro.toInt, cro.toInt, obro.toInt, obrc.toInt, gror.toInt, grob.toInt)
    case _ => sys.error("booboo")
  }
}

def checkBuildRobot(blueprint: Blueprint, material: MaterialStash, robots: List[Robot]): List[Action] = {
  val oreRobot: Option[Action] = if (material.ore >= blueprint.oreRobotOre && robots.count(_ == OreRobot) <= (blueprint.highestOreCost)) Some(BuildOreRobot) else None
  val clayRobot: Option[Action] = if (material.ore >= blueprint.clayRobotOre && robots.count(_ == ClayRobot) <= (blueprint.obsidianRobotClay)) Some(BuildClayRobot) else None
  val obsidianRobot: Option[Action] = if (material.ore >= blueprint.obsidianRobotOre && material.clay >= blueprint.obsidianRobotClay) Some(BuildObsidianRobot) else None
  val geodeRobot: Option[Action] = if (material.ore >= blueprint.geodeRobotOre && material.obsidian >= blueprint.geodeRobotObsidian) Some(BuildGeodeRobot) else None

  if (material.obsidian >= blueprint.geodeRobotObsidian) {
    List(geodeRobot, Some(DoNothing)).flatten
  } else if (material.clay >= blueprint.obsidianRobotClay) {
    List(geodeRobot, obsidianRobot, Some(DoNothing)).flatten
  } else if (material.ore > blueprint.highestOreCost) {
    List(oreRobot, clayRobot, obsidianRobot, geodeRobot).flatten
  } else {
    List(oreRobot, clayRobot, obsidianRobot, geodeRobot, Some(DoNothing)).flatten
  }
}

def buildRobot(action: Action): Robot =
  action match {
    case BuildOreRobot      => OreRobot
    case BuildClayRobot     => ClayRobot
    case BuildObsidianRobot => ObsidianRobot
    case BuildGeodeRobot    => GeodeRobot
  }

def produceMaterial(robots: List[Robot], stash: MaterialStash): MaterialStash = {
  robots match {
    case h :: t => h match {
      case OreRobot       => produceMaterial(t, stash.copy(ore = stash.ore + 1))
      case ClayRobot      => produceMaterial(t, stash.copy(clay = stash.clay + 1))
      case ObsidianRobot  => produceMaterial(t, stash.copy(obsidian = stash.obsidian + 1))
      case GeodeRobot     => produceMaterial(t, stash.copy(geode = stash.geode + 1))
    }
    case Nil => stash
  }
}

def consumeMaterial(action: Action, blueprint: Blueprint, stash: MaterialStash): MaterialStash = {
  action match {
    case BuildOreRobot      => stash.copy(ore = stash.ore - blueprint.oreRobotOre)
    case BuildClayRobot     => stash.copy(ore = stash.ore - blueprint.clayRobotOre)
    case BuildObsidianRobot => stash.copy(ore = stash.ore - blueprint.obsidianRobotOre, clay = stash.clay - blueprint.obsidianRobotClay)
    case BuildGeodeRobot    => stash.copy(ore = stash.ore - blueprint.geodeRobotOre, obsidian = stash.obsidian - blueprint.geodeRobotObsidian)
  }
}

def maxGeodeCollection(blueprint: Blueprint, robots: List[Robot], timeLimit: Int, material: MaterialStash = MaterialStash(), curMax: Int = 0, currentTime: Int = 1): Int =
  val newMax = if (currentTime == timeLimit && (material.geode + robots.count(_ == GeodeRobot)) > curMax) {material.geode + robots.count(_ == GeodeRobot)} else curMax
  if (currentTime == timeLimit) material.geode + robots.count(_ == GeodeRobot) else {
    val newActions = checkBuildRobot(blueprint, material, robots)
    newActions.foldLeft(newMax)((max, action) =>
      if (action == DoNothing) {
        val newMaterial = produceMaterial(robots, material)
        val possibleScore = maxGeodeCollection(blueprint, robots, timeLimit, newMaterial, newMax, currentTime + 1)
        if (possibleScore > max) {possibleScore} else {max}
      } else {
        val newRobot = buildRobot(action)
        val newMaterial = consumeMaterial(action, blueprint, produceMaterial(robots, material))
        val possibleScore = maxGeodeCollection(blueprint, newRobot :: robots, timeLimit, newMaterial, newMax, currentTime + 1)
        if (possibleScore > max) {possibleScore} else {max}}
    )
  }

object AOC19 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc19.txt").getLines.map(parseBlueprint).toList

  val answerP1 = input.map(b => maxGeodeCollection(b, List(OreRobot), timeLimit = 24) * b.index).sum

  val answerP2 = input.take(3).map(b => maxGeodeCollection(b, List(OreRobot), timeLimit = 32)).product

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)


