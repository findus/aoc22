import scala.io.Source
import io.io

object day1 extends App {

  val numberMapping = Map.from(List("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9));

  io.load("aoc2023/day1") { lines =>

//    val numbers = lines
//      .map(line => line.filter(character => character.isDigit).toList)
//      .map(line => Integer.parseInt(s"${line.head}${line.last}"))
//      .sum
//    println(numbers)

    def containsNumber(str: String) = {
       val e = numberMapping.keys.filter(number => str.contains(number) ).toList
      if (e.length > 1){
        println("a")
      }
      e.headOption
    }

    def parsenumbers(line: String) = {
      var newString = ""
      line.foreach(char => {
        newString = newString.appended(char)
        val number = containsNumber(newString)
        if (number.nonEmpty) {
          val old = newString
          newString = newString.replace(number.get, numberMapping(number.get).toString)
          newString
        } else {
          newString
        }
      })
      val a = line
      newString
    }

    val numbers2 = lines
      .map(line => (line,parsenumbers(line)) )
      .map(line => (line._1,line._2.filter(character => character.isDigit).toList)).toList
      .map(line => {
        if (line._2.length == 1) {
          Integer.parseInt(s"${line._2.head}${line._2.head}")
        } else {
          Integer.parseInt(s"${line._2.head}${line._2.last}")
        }
      })


    println(numbers2.sum)

  }

}
