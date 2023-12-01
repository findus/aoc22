package aoc2022

import io.io
object day8 extends App{

  def getColumnHeightsForRange(range: Range, tree: MapEntry, row: Int)(implicit entries: TreeMap) = range.filter(colIdx => (row, colIdx) != tree._1).map(colIdx => entries(colIdx, row))
  def getRowHeightsForRange(range: Range, tree: MapEntry, column: Int)(implicit entries: TreeMap) = range.filter(rowidx => (rowidx, column) != tree._1).map(rowidx => entries(column, rowidx))

  type MapEntry = ((Int, Int), Int)
  type TreeMap =  Map[(Int, Int), Int]
  type Coord = (Int,Int)

  def doStuffWithTrees(lines: List[String])(cb: (TreeMap, TreeMap, Coord ) => Unit) = {
    implicit val entries: Map[(Int, Int), Int] = lines.zipWithIndex.flatMap(row => row._1.zipWithIndex.map(column => ((column._2, row._2), Integer.parseInt(column._1.toString)))).toMap
    val max = entries.keys.max
    val innerTrees = entries.filter { case ((x,y),_) => x > 0 && x < max._1 && y > 0 && y < max._2 }
    cb(entries, innerTrees, max)
  }

  io.load("aoc2022/day8") { lines =>
    doStuffWithTrees(lines) { (entries, innerTrees, max) =>
      val visibleTrees = innerTrees.foldLeft(0)((prev, tree) => {
        val column = tree._1._1
        val row = tree._1._2
        val columnHeightsTo = getColumnHeightsForRange((0 until tree._1._1), tree, row)(entries)
        val columnHeightsFrom = getColumnHeightsForRange((tree._1._1 + 1 to max._1), tree, row)(entries)
        val rowHeightsTo = getRowHeightsForRange(0 until tree._1._2, tree, column)(entries)
        val rowHeightsFrom = getRowHeightsForRange(tree._1._2 + 1 to max._2, tree, column)(entries)
        val isVisible = List(columnHeightsTo, columnHeightsFrom, rowHeightsTo, rowHeightsFrom).count(lineOfSight => { lineOfSight.max >= tree._2 } ) < 4
        if (isVisible) { prev + 1 } else { prev }
      })
      println(visibleTrees + ((max._1 + 1) * 2) + (((max._2 + 1) * 2) - 4));
    }
  }

  def getViewScore(range: Range, treeHeight: Int, forHeight: (Int) => Int): Int = {
    range.zipWithIndex.find(c => forHeight(c._1) >= treeHeight).map(_._2 + 1).getOrElse(range.size)
  }

  io.load("aoc2022/day8") { lines =>
    doStuffWithTrees(lines) { (trees, innerTrees, max) =>
      val viewScores = innerTrees.map(tree => {
        val column = tree._1._1
        val row = tree._1._2

        val toWest = getViewScore((0 until tree._1._1).reverse, tree._2, c => trees(c, row))
        val toEast = getViewScore((tree._1._1 + 1 to max._1), tree._2, c => trees(c, row))
        val toNorth = getViewScore((0 until tree._1._2).reverse, tree._2, c => trees(column, c))
        val toSouth =  getViewScore((tree._1._2 + 1 to max._2), tree._2, c => trees(column, c))

        toWest * toEast * toNorth * toSouth
      })
      println(viewScores.max);
    }
  }
}
