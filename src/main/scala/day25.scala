import scala.annotation.tailrec

object  day25 extends App {

  io.load("day25") { lines =>

    def snafuToDec(nr: String):Long = {
      val len = nr.length - 1
      nr.foldLeft((0,0L))((prev,action) => {
        val convNr = nr(prev._1) match  {
          case '2' => 2
          case '1' => 1
          case '0' => 0
          case '-' => -1
          case '=' => -2
        }
        val pos = len - prev._1
        val power = getPower(pos)
        val n = (convNr * power).toLong
        val e = (prev._1 + 1,prev._2 + n)
        e
      })._2
    }

    def decToSnafu(decNr: Long) = {
      @tailrec
       def calc(rr: Long,str: String): (Long,String) = {
        if (rr == 0) return (rr,str)
        val r = (rr % 5)
        val remainder = r match {
          case 0 => '0'
          case 1 => '1'
          case 2 => '2'
          case 3 => '='
          case 4 => '-'
        }
        val c = str.prepended(remainder)
        calc(((rr + 2) / 5),c) // +2 to avoid negative modulo
      }
      calc(decNr, "")
    }

    def getPower(position: Long) = {
      position match {
        case(0) => 1
        case(1) => 5
        case(x) => math.pow(5,x)
      }
    }

    val sum = lines.map(l => snafuToDec(l)).sum
    println(sum)
    val snafu = decToSnafu(sum)
    println(snafu._2)
    println(snafuToDec(snafu._2))
  }

}
