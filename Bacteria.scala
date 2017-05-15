class Bacteria(xCoordinate:Int, yCoordinate:Int, initialState:Int) {
  val x = xCoordinate
  val y = yCoordinate
  var state = initialState
  var neighbourCount = 0
  def addNeighbour = { neighbourCount = neighbourCount + 1 }
  def changeState = {
    state match {
      case 0 => state = 1
      case 1 => state = 0
    }
  }
}

object Bacteria {
  // Make a string to use as a Map key
  def coordinateString(x:Int, y:Int):String = x + "," + y

  // For (x,y), return each positive neighbouring coordinate and self
  def neighbours(x:Int, y:Int):Set[(Int, Int)] = for (_x <- Set(math.abs(x - 1), x, x + 1); _y <- Set(math.abs(y - 1), y, y + 1))
    yield (_x, _y)

  def main(args:Array[String]):Unit = {

    // 1. Initial bacteria list. Function to add if not already in list
    var bacteriaList:Map[String, Bacteria] = Map()
    def bacteriaListAdd(x:Int, y:Int, state:Int):Bacteria = {
      val key = coordinateString(x, y)
      bacteriaList.getOrElse(key, {
        bacteriaList = bacteriaList.updated(key, new Bacteria(x, y, state))
        bacteriaList(key)
      })
    }


    // 2. Read StdIn until an end line is reached
    var in = ""
    while (in != "end") {
      in = io.StdIn.readLine()
      if (in != "end")
        bacteriaListAdd(in.split(",")(0).toInt, in.split(",")(1).toInt, 1)
    }


    // 3. Find neighbours and add to list
    bacteriaList.values foreach { bacteria =>
      neighbours(bacteria.x, bacteria.y) foreach { neighbour =>
        bacteriaListAdd(neighbour._1, neighbour._2, 0)
      }
    }


    // 4. Add a live neighbour count to each bacteria
    bacteriaList.values foreach (bacteria => {
      bacteriaList.values foreach (neighbour => {
        if (neighbour.state == 1 &&
            math.abs(bacteria.x - neighbour.x) <= 1 && math.abs(bacteria.y - neighbour.y) <= 1 &&
            !(bacteria.x == neighbour.x && bacteria.y == neighbour.y))
          bacteria.addNeighbour
      })
    })


    // 5. Apply rules
    bacteriaList.values foreach (bacteria => {
      if (bacteria.state == 1 && (bacteria.neighbourCount < 2 || bacteria.neighbourCount > 3))
        bacteria.changeState
      if (bacteria.state == 0 && bacteria.neighbourCount == 3)
        bacteria.changeState
    })

    
    // 6. Output
    bacteriaList.values foreach { bacteria => if (bacteria.state == 1) println(coordinateString(bacteria.x, bacteria.y)) }
    println("end")
  }
}
