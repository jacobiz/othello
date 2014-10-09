object Othello extends App {
  
  var board = OthelloBoard.init
  println(board)
  
  while (board.move.moveNumber < 10) { //仮に10手まで
    val inputCoordinates = readNextMove()
    board = board.next(inputCoordinates) //TODO おけない場所に置こうとした場合の処理追加
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
