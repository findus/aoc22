
import scala.io.Source
import io.io

object day3 extends App {

  case class PartMatrix(field: Seq[Seq[Char]]) {
    def fits(coord: Coord) = {
      coord._1 >= 0 && coord._2 >= 0 && coord._1 < field.length && coord._2 < field.head.length
    }
  }

  type Coord = (Int,Int)

  val regex = "(\\d{1,3})".r

  case class EngineNumber(x: Int, y: Int, number: Int)  {
  }

  def getCoordsToCheck(matrix: PartMatrix, engineNumber: EngineNumber) = {
    val numberLen = engineNumber.number.toString.length
    val nw = (engineNumber.x -1, engineNumber.y -1)
    val w = (engineNumber.x -1, engineNumber.y)
    val sw = (engineNumber.x -1, engineNumber.y + 1)
    val s = (engineNumber.x.to(engineNumber.x + numberLen)).map(entry => (entry, engineNumber.y + 1)).toSeq
    val se = (engineNumber.x + numberLen, engineNumber.y + 1)
    val e = (engineNumber.x + numberLen, engineNumber.y)
    val ne = (engineNumber.x + numberLen, engineNumber.y - 1)
    val n = (engineNumber.x.to(engineNumber.x + numberLen)).map(entry => (entry, engineNumber.y - 1)).toSeq
    (Seq(nw,w,sw,se,e,ne) ++ n ++ s).filter(matrix.fits)
  }

  io.load("aoc2023/day3") { lines =>

    val eee = lines.zipWithIndex.flatMap(e => {
      val matches = regex.findAllMatchIn(e._1).toList
      matches.map(m => {
        EngineNumber(m.start, e._2, m.matched.toInt)
      })

    })

    val matrix = PartMatrix(lines.map(l => l.toCharArray.toList))
    val check = eee.map(e => {
      val f = getCoordsToCheck(matrix, e)
      println(e.number,f.map(fd=>matrix.field(fd._2)(fd._1)).toList)
      (e, f.exists(coord => matrix.field(coord._2)(coord._1) != '.'))
    })
    println(check)
    println(check.filter(_._2).map(dddd => dddd._1.number).sum )



  }

}
