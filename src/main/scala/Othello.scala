object Othello extends App {
  
  var board = OthelloBoard.init
  println(board)
  
  var flag = true
  while (board.move.moveNumber < 10) {
    val inputCoordinates = readNextMove()
    board = board.next(inputCoordinates)
    println(board)
  }

  def readNextMove(): Coordinates = {
    println("Type where to place a disc:'a1'-'f8'")
    val input = readLine()
    PlacedDisc.parseString(input) match {
      case None    => println("Wrong input. Type again.")
                      readNextMove()
      case Some(x) => x
    }
  }
}
