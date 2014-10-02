case class Move(player: Player, moveNumber: Int) {
  def next = Move(player.next, moveNumber + 1)
}

object Player {
  case object Black extends Player {
    def next = White
  }
  case object White extends Player {
    def next = Black
  }
}

sealed abstract class Player {
  def next: Player
}