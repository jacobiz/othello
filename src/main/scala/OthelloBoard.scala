class OthelloBoard(val discs: Map[(Int, Int), Disc], val move: Move) {
  def next(playedDisc: (Int, Int, Disc)): OthelloBoard = {
    val directions = List((1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1))
    val discListToFlip: List[((Int, Int), Disc)] = (
      for (direction <- directions) yield 
        discsToFlip((playedDisc._1, playedDisc._2), direction, playedDisc._3, Nil)
    ).flatten

    var nextDiscs: Map[(Int, Int), Disc] = discs
    nextDiscs += (playedDisc._1, playedDisc._2) -> playedDisc._3
    for (discToFlip <- discListToFlip) {
      nextDiscs += discToFlip._1 -> discToFlip._2.flipped
    }
    
    new OthelloBoard(nextDiscs, move.next)
  }
  
  //始点と向きを指定して、反転させるべきコマのリストを返す
  def discsToFlip(start: (Int, Int), direction: (Int, Int), startDisc: Disc, buf: List[((Int, Int), Disc)]): List[((Int, Int), Disc)] = {
    val adjacentLocation = (start._1 + direction._1, start._2 + direction._2)
    val adjacentDisc = discs(adjacentLocation)
    adjacentDisc match {
      case Blank => Nil
      case Wall => Nil
      case t if t == startDisc => buf
      case t if t == startDisc.flipped => 
        discsToFlip(adjacentLocation, direction, startDisc, (adjacentLocation, adjacentDisc) :: buf)
    }
  }

  override def toString() = {
    val buf = new StringBuilder
    buf.append("Player: " + move.player + "\n")
    buf.append(" a b c d e f g h" + "\n")
    for (y <- 1 to 8) {
      buf.append(y)
      for (x <- 1 to 8) {
        val char = discs((x, y)) match {
          case Blank => "□"
          case Black => "●"
          case White => "○"
          case Wall => ""
        }
        buf.append(char)
      }
      buf.append("\n")
    }
    buf.toString
  }
}

object OthelloBoard {
  def init: OthelloBoard = {
    val discs = (
      for (y <- 0 to 9; x <- 0 to 9) yield {
        val disc = (x, y) match {
          case (0, _) => Wall
          case (9, _) => Wall
          case (_, 0) => Wall
          case (_, 9) => Wall
          case (4, 4) => White
          case (4, 5) => Black
          case (5, 4) => Black
          case (5, 5) => White
          case _ => Blank
        }
        (x, y) -> disc
      }).toMap
    new OthelloBoard(discs, Move(Player.Black, moveNumber = 1))
  }
}