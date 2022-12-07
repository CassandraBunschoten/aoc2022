package aoc2022

import scala.io.Source 

case class FolderStructure(directories: List[Directory], path: List[String])

val startFolderStructure = FolderStructure(directories = List(Directory("/", path = List("/"), files = List[File]())), path = List("/"))

case class SummedDirectories(nameDirectory: String, path: List[String], summed: Int)

case class File(num: Int, name: String)

case class Directory(str: String, path: List[String], files: List[File]):
  def sumInDirectory: SummedDirectories = {
    val summed = this.files.map(x => x.num).sum
    SummedDirectories(this.str, this.path, summed)
  }


def getDirStructure(input: List[String], fs: FolderStructure): FolderStructure = {
  input match {
    case Nil          => fs
    case head :: tail => { val newFs = processCommand(head, fs)
      getDirStructure(tail, newFs)
    }
  }
}

def processCommand(input: String, fs: FolderStructure): FolderStructure = {
  input match {
    case s"dir ${str}"      => fs.copy(directories = Directory(str, path = str :: fs.path, files = List[File]()) :: fs.directories)
    case "$ ls"             => fs
    case """$ cd .."""      => fs.copy(path = fs.path.tail)
    case """$ cd /"""       => fs
    case s"$$ cd ${str}"    => { val foundDir = findDir(str, fs); fs.copy(path = foundDir.path)}
    case s"${num} ${rest}"  => { val newFile = File(num.toInt, rest); updateDir(newFile, fs) }
  }  
}

def findDir(input: String, fs: FolderStructure): Directory = {
  val curPath = fs.directories.filter(x => x.path.tail == fs.path)
  curPath.find(directory => directory.str == input).getOrElse(sys.error("boo"))
}

def updateDir(file: File, fs: FolderStructure) = {
  val (directory, location) = fs.directories.zipWithIndex.find{ case (dir, index) => dir.path == fs.path}.getOrElse(sys.error("booboo"))
  fs.copy(directories = fs.directories.updated(location, directory.copy(files = file :: directory.files)))
}

object AOC7 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc7.txt").getLines.toList

  val summedInDir = getDirStructure(input, startFolderStructure).directories.map(directory => directory.sumInDirectory)

  val summedOverDir = summedInDir.map{ summedDir => val grouped = summedInDir.filter(x => x.path.mkString("/").contains(s"${summedDir.path.mkString("/")}"))
                                 (summedDir.nameDirectory, grouped.foldLeft(0)(_ + _.summed))
  }

  val answerP1 = summedOverDir.filter(_._2 < 100000).map(x => x._2).sum

  val curSpace = 70000000 - summedOverDir.filter(x => x._1 == "/").head._2
  val saveSpace = summedOverDir.filter(x => 30000000 < curSpace + x._2).map(x => x._2).min
  val answerP2 = summedOverDir.find(x => x._2 == saveSpace).get._2

  println("Answer to part 1: " + answerP1)
  println("Answer to part 2: " + answerP2)
