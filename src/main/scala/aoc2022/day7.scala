package aoc2022

import scala.collection.mutable

import io.io
object day7 extends App {

  val regex = "^(\\d+).*".r
  val stack = mutable.Stack.empty[Folder]
  val list = mutable.Stack.empty[Folder]

  case class File(name: String, size: Int)
  case class Folder(name: String, var files: List[File], var childs: List[Folder], parent: String ) {
    def getSizeOfFolder(): List[Int] = {
      files.map(_.size).appended(childs.flatMap(f => f.getSizeOfFolder()).sum)
    }
  }

  private def yeet(lines: List[String]) = {
    lines.drop(1).foreach {
      case cd if cd.contains("$ cd ..") =>
        stack.pop()
      case cd if cd.contains("$ cd") =>
        val foldername = cd.split(" ")(2)
        val newFolder = Folder(foldername, List.empty, List.empty, stack.head.name)
        val e = stack.head.childs.appended(newFolder)
        stack.head.childs = e
        stack.push(newFolder)
        list.push(newFolder)
      case content if !content.startsWith("ls") && regex.matches(content) =>
        val e = regex.findAllIn(content).matchData.toList.head.group(1)
        val file = File(content.split(" ")(1), Integer.parseInt(e))
        stack.head.files = stack.head.files.appended(file)
      case l =>
    }
    list.foreach(folder => println(s"${folder.name} ${folder.getSizeOfFolder().sum}"))
    list
  }

  val top = Folder("/", List.empty, List.empty, null)
  stack.push(top)
  list.push(top)

  io.load("aoc2022/day7") { lines =>
    yeet(lines)
    val c = list.map(e => e.getSizeOfFolder()).map(_.sum)
    println(c.filter(f => f <= 100000).sum)
  }

  io.load("aoc2022/day7") { lines =>
    val unused_space = 70000000 - 47052440
    list.foreach(folder => {
      val size = unused_space + folder.getSizeOfFolder().sum
      if (size >= 30000000) {
        println(s"${folder.name}, ${folder.getSizeOfFolder().sum}")
      }
    })
  }
}
