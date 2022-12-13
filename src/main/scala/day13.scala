import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.seqOrdering
import scala.util.parsing.combinator.RegexParsers

object day13 extends App with RegexParsers {

  trait Packet extends Ordered[Packet] {
    override def compare(that: Packet): Int = {
      (this, that) match {
        case (IntNode(val1), IntNode(val2)) => val1.compare(val2)
        case (ListNode(l1), ListNode(l2)) => l1.compare(l2)
        case (IntNode(_), ListNode(_)) => ListNode(List(this)).compare(that)
        case (ListNode(_), IntNode(_)) => this.compare(ListNode(List(that)))
      }
    }
  }
  case class IntNode(value: Int) extends Packet
  case class ListNode(nodes: List[Packet]) extends Packet

  def parsePacket(input: String) = {
    def packetNode: Parser[Packet] = "\\d+".r ^^ (value => IntNode(value.toInt)) | "[" ~> repsep(packetNode, ",") <~ "]" ^^ ListNode.apply
    parseAll(packetNode, input).get
  }

  val regex = "(\\d+)".r

  io.load("day13") { lines =>
    val packets = lines.filterNot(_.isEmpty).map(parsePacket)
    println(packets)
    val result =
      packets.grouped(2).zipWithIndex.map(e => (e._1, e._2 + 1)).map( entry => {
      val left = entry._1.head
      val right = entry._1.last
      left.compare(right)
    })
        .zipWithIndex
        .filter(entry => entry._1 == -1)
        .map(_._2 + 1)
        .sum

    println(result)
  }

}
