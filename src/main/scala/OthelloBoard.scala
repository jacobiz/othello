// import scala.collection.mutable.Map
import DiscType._

class OthelloBoard(val discs: Map[(Int, Int), Disc], val move: Move) {
  def next(playedDisc: Disc): OthelloBoard = {
    val directions = List((1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1))
    val discListToFlip: List[Disc] = (
      for (direction <- directions) yield 
        discsToFlip((playedDisc.x, playedDisc.y), direction, playedDisc.discType, Nil)
    ).flatten

    var nextDiscs: Map[(Int, Int), Disc] = discs
    nextDiscs += (playedDisc.x, playedDisc.y) -> playedDisc
    for (discToFlip: Disc <- discListToFlip) {
      nextDiscs += (discToFlip.x, discToFlip.y) -> discToFlip.flipped
    }
    
    new OthelloBoard(nextDiscs, move.next)
  }
  
  //始点と向きを指定して、反転させるべきコマのリストを返す
  def discsToFlip(start: (Int, Int), direction: (Int, Int), startDiscType: DiscType, buf: List[Disc]): List[Disc] = {
    val adjacentDisc = discs((start._1 + direction._1, start._2 + direction._2))
    adjacentDisc.discType match {
      case Blank => Nil
      case Wall => Nil
      case t if t == startDiscType => buf
      case t if t == startDiscType.flipped => 
        discsToFlip((adjacentDisc.x, adjacentDisc.y), direction, startDiscType, adjacentDisc :: buf)
    }
  }

  override def toString() = {
    val buf = new StringBuilder
    buf.append("Player: " + move.player + "\n")
    buf.append(" a b c d e f g h" + "\n")
    for (y <- 1 to 8) {
      buf.append(y)
      for (x <- 1 to 8) {
        val char = discs((x, y)).discType match {
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
        val discType = (x, y) match {
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
        val disc = Disc(x, y, discType)
        (x, y) -> disc
      }).toMap
    new OthelloBoard(discs, Move(Player.Black, moveNumber = 1))
  }
}