case class Disc(x: Int, y: Int, discType: DiscType) {
  def flipped = Disc(x, y, discType.flipped)
}

object DiscType {
  case object Black extends DiscType {
    def flipped = White
  }
  case object White extends DiscType {
    def flipped = Black
  }
  case object Blank extends DiscType {
    def flipped = Blank
  }
  case object Wall extends DiscType {
    def flipped = Wall
  }
}

sealed abstract class DiscType {
  def flipped: DiscType
}