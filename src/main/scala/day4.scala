object day4 extends App {

  io.load("day4") { lines =>
    val entries = lines.map(line => (line.split(",").head.split("-").map(Integer.parseInt),line.split(",").last.split("-").map(Integer.parseInt)))
    println(entries.count { case (first, second) => (first(0) <= second(0) && first(1) >= second(1)) || (second(0) <= first(0) && second(1) >= first(1)) })
  }

  io.load("day4") { lines =>
    val entries = lines.map(line => (line.split(",").head.split("-").map(Integer.parseInt),line.split(",").last.split("-").map(Integer.parseInt)))
    println(entries.map(entry => (Range.inclusive(entry._1(0), entry._1(1)), Range.inclusive(entry._2(0), entry._2(1)))).count(entry => entry._1.intersect(entry._2).nonEmpty))
  }

}