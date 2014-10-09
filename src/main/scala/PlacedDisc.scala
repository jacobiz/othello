case class PlacedDisc(coordinates: Coordinates, disc: Disc) {
  
}

object PlacedDisc {
  def parseString(input: String): Option[Coordinates] = {
    val inputs = for (x <- 'a' to 'h'; y <- 1 to 8) yield x.toString + y
    val outputs = for (x <- 1 to 8; y <- 1 to 8) yield Coordinates(x, y) //
    val inputToOutput = (inputs zip outputs).toMap
    inputToOutput.get(input)
  }
}