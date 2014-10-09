case class Move(player: Player, moveNumber: Int) {
  def next = Move(player.next, moveNumber + 1)
}

object Player {
  case object Black extends Player {
    def next = White
    def disc = Disc.Black
  }
  case object White extends Player {
    def next = Black
    def disc = Disc.White
  }
}

sealed abstract class Player {
  def next: Player
  def disc: Disc
}