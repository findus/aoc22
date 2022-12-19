object  day6 extends App {

  def hasOnlyDifferentChars(list: List[Char]) = {
    list.sorted.distinct.length == list.length
  }

  io.load("day6") { lines =>
    val haval_gib_mir_einfach_ein_köftespieß = lines.map(line => line.zipWithIndex.sliding(4,1).filter(entry => hasOnlyDifferentChars(entry.map(_._1).toList)).toList.head.last._2 + 1)
    println(haval_gib_mir_einfach_ein_köftespieß.head)
  }

  io.load("day6") { lines =>
    val haval_gib_mir_einfach_ein_köftespieß = lines.map(line => line.zipWithIndex.sliding(14,1).filter(entry => hasOnlyDifferentChars(entry.map(_._1).toList)).toList.head.last._2 + 1)
    println(haval_gib_mir_einfach_ein_köftespieß.head)
  }
}
