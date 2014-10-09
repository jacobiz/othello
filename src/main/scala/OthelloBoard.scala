import Disc._

class OthelloBoard(val discs: Map[Coordinates, Disc], val move: Move) {
  def next(placedCoordinates: Coordinates): OthelloBoard = {
    val playedDisc = PlacedDisc(placedCoordinates, move.player.disc)
    var nextDiscs: Map[Coordinates, Disc] = discs
    nextDiscs += playedDisc.coordinates -> playedDisc.disc

    for (discToFlip <- discsToFlip(playedDisc)) {
      nextDiscs += discToFlip.coordinates -> discToFlip.disc.flipped
    }
    
    new OthelloBoard(nextDiscs, move.next)
  }
  
  //始点と向きを指定して、反転させるべきコマのリストを返す
  def discsToFlip(playedDisc: PlacedDisc): List[PlacedDisc] = {
    def discsToFlipWithSingleDirection(coordinates: Coordinates, direction: (Int, Int), list: List[PlacedDisc]): List[PlacedDisc] = {
      val adjacentLocation = Coordinates(coordinates.x + direction._1, coordinates.y + direction._2)
      val adjacentDisc = discs(adjacentLocation)
      adjacentDisc match {
        case Blank => Nil
        case Wall => Nil
        case t if t == playedDisc.disc => list  //始点と同じ色まできたら、その間の反対色のlistを返す
        case t if t == playedDisc.disc.flipped => 
          discsToFlipWithSingleDirection(adjacentLocation, direction, PlacedDisc(adjacentLocation, adjacentDisc) :: list)
      }
    }

    val directions = List((1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1))
    val returnList = (
      for (direction <- directions) yield 
        discsToFlipWithSingleDirection(playedDisc.coordinates, direction, Nil)
    ).flatten
    returnList
  }

  override def toString() = {
    val buf = new StringBuilder
    buf.append("Player: " + move.player + "\n")
    buf.append(" a b c d e f g h" + "\n")
    for (y <- 1 to 8) {
      buf.append(y)
      for (x <- 1 to 8) {
        val char = discs(Coordinates(x, y)) match {
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
        Coordinates(x, y) -> disc
      }).toMap
    new OthelloBoard(discs, Move(Player.Black, moveNumber = 1))
  }
}