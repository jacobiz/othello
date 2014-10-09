sealed abstract class Disc {
  def flipped: Disc
}

case object Black extends Disc {
  def flipped = White
}
case object White extends Disc {
  def flipped = Black
}
case object Blank extends Disc {
  def flipped = Blank
}
case object Wall extends Disc {
  def flipped = Wall
}
