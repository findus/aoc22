object day8 extends App{

  def getColumnHeightsForRange(range: Range, tree: ((Int,Int),Int), row: Int)(implicit entries: Map[(Int, Int), Int]) = range.filter(colIdx => (row, colIdx) != tree._1).map(colIdx => entries(colIdx, row))
  def getRowHeightsForRange(range: Range, tree: ((Int,Int),Int), column: Int)(implicit entries: Map[(Int, Int), Int]) = range.filter(rowidx => (rowidx, column) != tree._1).map(rowidx => entries(column, rowidx))

  io.load("day8") { lines =>
    implicit val entries: Map[(Int, Int), Int] = lines.zipWithIndex.flatMap(row => row._1.zipWithIndex.map(column => ((row._2, column._2), Integer.parseInt(column._1.toString)))).toMap
    val max = entries.keys.max
    val innerTrees = entries.filter { case ((x,y),_) => x > 0 && x < max._1 && y > 0 && y < max._2 }
    val visibleTrees = innerTrees.foldLeft(0)((prev, tree) => {
      val column = tree._1._1
      val row = tree._1._2
      val columnHeightsTo = getColumnHeightsForRange((0 until tree._1._1), tree, row)
      val columnHeightsFrom = getColumnHeightsForRange((tree._1._1 + 1 to max._1), tree, row)
      val rowHeightsTo = getRowHeightsForRange(0 until tree._1._2, tree, column)
      val rowHeightsFrom = getRowHeightsForRange(tree._1._2 + 1 to max._2, tree, column)
      val isVisible = List(columnHeightsTo, columnHeightsFrom, rowHeightsTo, rowHeightsFrom).count(lineOfSight => { lineOfSight.max >= tree._2 } ) < 4
      if (isVisible) { prev + 1 } else { prev }
    })
    println(visibleTrees + ((max._1 + 1) * 2) + (((max._2 + 1) * 2) - 4));
  }

  io.load("day8") { lines =>

  }
}
