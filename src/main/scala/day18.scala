object day18 extends App {

  case class Cube(x: Int, y: Int, z: Int) {
    def unConnectedSides(list: List[Cube]) = {
      val around = List((-1,0,0),(0,0,-1),(0,-1,0),( 1,0,0),(0,0, 1),(0, 1,0)).map { case (dx,dy,dz) => Cube(x+dx, y+dy, z+dz) }
      val amount = around.count { c => list.contains(c) }
      6 - amount
    }
  }

  io.load("day18") { lines =>
    val cubes = lines.map(entry => {
      val coords = entry.split(",")
      Cube(Integer.parseInt(coords(0)), Integer.parseInt(coords(1)), Integer.parseInt(coords(2)))
    })

    val e = cubes.map(c => c.unConnectedSides(cubes)).sum
    println(e)

  }

}
