object Othello extends App {
  
  val board = OthelloBoard.init
  println(board)
  var nextBoard = board.next((4, 3, Black))
  println(nextBoard)
  nextBoard = nextBoard.next((3, 3, White))
  println(nextBoard)
  // nextBoard = nextBoard.next(Disc(5, 6, Black))
  // println(nextBoard)
  // nextBoard = nextBoard.next(Disc(4, 6, White))
  // println(nextBoard)
  // nextBoard = nextBoard.next(Disc(3, 6, Black))
  // println(nextBoard)
  // nextBoard = nextBoard.next(Disc(4, 7, White))
  // println(nextBoard)
}
