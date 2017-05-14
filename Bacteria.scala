class Bacteria(xCoordinate:Int = -1, yCoordinate:Int = -1, initialState:Int = 0) {
  val localX = xCoordinate
  val localY = yCoordinate
  var localState = initialState
  var localNeighbourCount = 0
  def x = localX
  def y = localY
  def state = localState
  def state_(n:Int) = { localState = n }
  def neighbourCount = localNeighbourCount
  def neighbourCount_(n:Int) = { localNeighbourCount = n } // is this a normal way to name setters?

  def print:Unit = println("x = " + localX + ", y = " + localY + ", state = " + localState + ", neighbourCount = " + localNeighbourCount) // for testing, to delete
}

object Bacteria {
  def hashFunction(x:Int, y:Int):String = x + "," + y

  def printAll(bacteriaList:Map[String, Bacteria]):Unit = bacteriaList foreach (x => x._2.print)

  def main(args:Array[String]):Unit = {

    // 1. Initial bacteria list with a method for insertion
    var bacteriaList:Map[String, Bacteria] = Map()
    def bacteriaListAdd(x:Int, y:Int, state:Int):Bacteria = {
      val key = hashFunction(x, y)
      bacteriaList.getOrElse(key, {
        bacteriaList = bacteriaList.updated(key, new Bacteria(x, y, state))
        bacteriaList(key)
      })
    }

    // 2. Read StdIn until an end line is reached
    var data:Array[String] = Array()
    var end = ""
    while (end != "end") {
      end = io.StdIn.readLine()
      if (end != "end")
        data = data :+ end
    }


    // 3. Create object Map[String, Bacteria]. A map guarentees uniqueness - not being used because it overwrites instead of rejecting
    data.foreach(x => bacteriaListAdd(x.split(",")(0).toInt, x.split(",")(1).toInt, 1))
    

    // 4. Find neighbours. Get the x and y, make surrounding 8 x,y pairs, add to neighbourList with new Bacteria
    var neighbourList:Map[String, Bacteria] = Map()
    bacteriaList foreach(bacteria => {
      val x = bacteria._2.x
      val y = bacteria._2.y
      val xCoordinates = Set(Math.abs(x - 1), x ,x + 1)
      val yCoordinates = Set(Math.abs(y - 1), y ,y + 1)
      val neighbourCoordinatesAndSelf = for (xCoordinates_ <- xCoordinates; yCoordinates_ <- yCoordinates) yield (xCoordinates_, yCoordinates_)
      val neighbourCoordinates = neighbourCoordinatesAndSelf filter (coordinates => !(coordinates._1 == x && coordinates._2 == y) )
      val neighbourBacteria = neighbourCoordinates map((site:(Int,Int)) => hashFunction(site._1, site._2) -> new Bacteria(site._1, site._2) )
      neighbourList = neighbourList ++ neighbourBacteria.toMap
    })

    //val newKeys = neighbourList.keySet.diff(bacteriaList.keySet)
    neighbourList foreach(x => bacteriaListAdd(x._1.split(",")(0).toInt, x._1.split(",")(1).toInt, 0)) // if this works, it can be massively simplified, i only need the keylist from the block above


    // 5. Add a live neighbour count to each bacteria
    bacteriaList foreach(currentSite => {
      var neighbourCount = 0
      bacteriaList foreach(neighbour => {
        if (neighbour._2.state == 1 && Math.abs(currentSite._2.x - neighbour._2.x) <= 1 && Math.abs(currentSite._2.y - neighbour._2.y) <= 1 && !(currentSite._2.x == neighbour._2.x && currentSite._2.y == neighbour._2.y))
          neighbourCount = neighbourCount + 1
      })
      currentSite._2.neighbourCount_(neighbourCount)
    })


    // 6. Apply rules and output
    bacteriaList foreach(bacteria => {
      if (bacteria._2.state == 1 && (bacteria._2.neighbourCount < 2 || bacteria._2.neighbourCount > 3))
        bacteria._2.state_(0)
      if (bacteria._2.state == 0 && bacteria._2.neighbourCount == 3)
        bacteria._2.state_(1)
    })

    bacteriaList foreach(bacteria => {
      if (bacteria._2.state == 1)
        println(hashFunction(bacteria._2.x, bacteria._2.y))
    })
    println("end")
  }
}

