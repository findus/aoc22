import day17.State

object  day20 extends App {

  case class Entry(idx: Int, value: Int, next: Entry, prev: Entry)

  def iterate(state : State): State = {
    val arr_len = state.list.length
    val from = state.list.indexWhere(_._2 == state.idx)
    val n = from + state.list(from)._1
    val newIdx = if (n >= state.list.length)
      (n % arr_len) + 1
    else if (n < 0)
      (arr_len - (n.abs % arr_len) - 1)
    else if (n == 0)
      arr_len - 1
    else if (n == (arr_len -1))
      0
    else
      n

    val newState = if (state.list(from)._1 != 0) {
      state.copy(list = state.list.patch(from, Seq(), 1).patch(newIdx, Seq(state.list(from)), 0), idx = state.idx + 1)
    } else {
      state.copy(idx = state.idx + 1)
    }
    newState
  }

  case class State(list :List[(Int,Int)], idx: Int) {
    def getNth(n: Int): Long = {
      val idx = (n + list.indexWhere(_._1 == 0)) % list.length
      list(idx)._1
    }
  }

  io.load("day20") { lines =>
    val numbers = State(lines.map(Integer.parseInt).zipWithIndex,0)
    val iterator = Iterator.iterate(numbers)(iterate)
    val next = iterator.drop(numbers.list.length).next()
    val sum = List(next.getNth(1000),next.getNth(2000),next.getNth(3000)).sum
    println(sum)
  }

}
