package aoc20

import scala.io.Source
import scala.collection.mutable._

case class Node(value: Long, var previous: Node, var next: Node)

def makeCircularList(input: ArrayBuffer[Long], key: Long = 1L): ArrayBuffer[Node] = {
  var nodesWithNeigh = input.map(n => Node(n * key, null, null))

  nodesWithNeigh.zipWithIndex.map{ (node, index) =>
    if (index == 0) {
      node.previous = nodesWithNeigh(nodesWithNeigh.size - 1)
      node.next     = nodesWithNeigh(index + 1)
    } else if (index == nodesWithNeigh.size - 1) {
      node.previous = nodesWithNeigh(((index - 1) % nodesWithNeigh.size))
      node.next     = nodesWithNeigh(0)
    } else {
      node.previous = nodesWithNeigh(index - 1)
      node.next     = nodesWithNeigh(index + 1)
    }
  }
  nodesWithNeigh
}

def shiftNodes(nodes: ArrayBuffer[Node], rounds: Int): ArrayBuffer[Node] =
  if (rounds > 0) {
    nodes.foreach {node => 
      val shifts    = (node.value % (nodes.size - 1)).toLong
      val nodeRange = (0L until shifts.abs.toLong)

      if (shifts < 0L) {
        nodeRange.foreach(_ => 
          val nextNode = node.next
          val currentNode = node
          val previousNode = node.previous
          val previousPreviousNode = node.previous.previous
          
          nextNode.previous = previousNode
          previousNode.next = nextNode
          previousNode.previous = currentNode
          currentNode.next = previousNode
          currentNode.previous = previousPreviousNode
          previousPreviousNode.next = currentNode
        )
       } else {
        nodeRange.foreach(_ => 
          val nextNextNode = node.next.next
          val nextNode = node.next
          val currentNode = node
          val previousNode = node.previous
        
          nextNextNode.previous = currentNode
          currentNode.next = nextNextNode
          currentNode.previous = nextNode
          nextNode.next = currentNode
          nextNode.previous = previousNode
          previousNode.next = nextNode
        )
    }
  }
    shiftNodes(nodes, rounds - 1)
  }
  else
    {nodes}

def positionFromZero(nodes: ArrayBuffer[Node], goal: Int): Long =
  var current = nodes.find(_.value == 0).get
  var max: Long = 0
  var nodeRange = Range(0, goal)

  nodeRange.foreach(_ => 
    current = current.next
    max = current.value
  )
  max

object AOC20 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc20.txt").getLines.map(_.toLong).to(scala.collection.mutable.ArrayBuffer)
  val shiftedNodesP1 = shiftNodes(makeCircularList(input), 1)
  val answerP1 = List(positionFromZero(shiftedNodesP1, 1000), positionFromZero(shiftedNodesP1, 2000), positionFromZero(shiftedNodesP1, 3000)).sum
  
  val shiftedNodesP2 = shiftNodes(makeCircularList(input, 811589153L), 10)
  val answerP2 = List(positionFromZero(shiftedNodesP2, 1000), positionFromZero(shiftedNodesP2, 2000), positionFromZero(shiftedNodesP2, 3000)).sum

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)