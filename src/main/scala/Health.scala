trait Health {}

case class Alive(health: Double) extends Health

case class Dead() extends Health

object Health {
  def apply(health: Double): Health = {
    if (health > 0) {
      Alive(Math.min(health, 1000))
    } else {
      Dead()
    }
  }
}
