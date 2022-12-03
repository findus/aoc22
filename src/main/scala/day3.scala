object day3 extends App {

  def getAllCharsPresentInBoth(set: List[Set[Char]]): Set[Char] = { set.head.intersect(set.last) }

  def getPosition(character: Char) = { character.toInt - (if (character.isUpper) { 38 } else { 96 }) }

  io.load("day3") { lines =>
    val items = lines.map(line => line.grouped(line.length / 2).map(_.sorted.distinct.toSet).toList)
    println(items.flatMap(set => getAllCharsPresentInBoth(set).map(getPosition)).sum)
  }

}