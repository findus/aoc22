import scala.annotation.tailrec

object  day20 extends App {

  @tailrec
  def iterate(state : State, level: Int): State = {
    if (level == state.list.length) return state
    val arr_len = (state.list.length - 1)
    val from = state.list.indexWhere(_._2 == level)
    val n = from + state.list(from)._1
    val newIdx = if (n >= state.list.length) (n % arr_len).toInt else if (n < 0) (arr_len - (n.abs % arr_len)).toInt else if (n == 0) arr_len.toInt else if (n == (arr_len)) 0 else n.toInt

    val newState = if (state.list(from)._1 != 0) {
      state.copy(list = state.list.patch(from, Seq(), 1).patch(newIdx, Seq(state.list(from)), 0), idx = state.idx + 1)
    } else {
      state.copy(idx = state.idx + 1)
    }
    iterate(newState, level + 1)
  }

  def iterate(state: State): State = {
    iterate(state, 0)
  }

  case class State(list :List[(Long,Int)], idx: Int) {
    def getNth(n: Int): Long = {
      val idx = (n + list.indexWhere(_._1 == 0)) % list.length
      list(idx)._1
    }
  }

  io.load("day20") { lines =>
    val numbers = State(lines.map(Integer.parseInt).map(_.toLong).zipWithIndex,0)
    val iterator = Iterator.iterate(numbers)(iterate)
    val next = iterator.drop(1).next()
    val sum = List(next.getNth(1000),next.getNth(2000),next.getNth(3000)).sum
    println(sum)
  }

  io.load("day20") { lines =>
    val numbers = State(lines.map(Integer.parseInt).map(e => e.toLong * 811589153L).zipWithIndex,0)
    val iterator = Iterator.iterate(numbers)(iterate)
    val next = iterator.drop(10).next()
    val sum = List(next.getNth(1000),next.getNth(2000),next.getNth(3000)).sum
    println(sum)
  }
}
