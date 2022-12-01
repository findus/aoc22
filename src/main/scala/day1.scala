import scala.io.Source

object day1 extends App {

   io.load("day1") { lines =>

           val numbers = lines.map(line => {
               if (line.equals("")) { 0 } else { Integer.parseInt(line) }
           })

           val addedCalories = numbers.foldLeft(List(0))((prev, next) => {
               if (next == 0) {
                   prev.appended(0)
               } else {
                   val last = prev.last
                   val newList = prev.dropRight(1)
                   newList.appended(last + next)
               }
           })

           //a
           println(addedCalories.sorted.max)

           //b
           println(addedCalories.sorted.drop(addedCalories.length - 3).sum)
   }

}
