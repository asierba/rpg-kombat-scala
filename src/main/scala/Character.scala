trait Health {}
case class Alive(health: Int) extends Health
case class Dead() extends Health

case class Character(health: Health = Health(1000), level: Int = 1)

object Health {
  def apply(health: Int): Health = {
    if (health > 0) {
      Alive(Math.min(health, 1000))
    } else {
      Dead()
    }
  }
}

object Character {
  def attack(attacker: Character, receiver: Character, damage: Int) = {
    if (attacker == receiver) {
      attacker
    } else {
      receiver.copy(health = recalculateHealth(receiver, damage, _ - _))
    }
  }

  def heal(from: Character, to: Character, health: Int): Character = {
    if (from == to) {
      to.copy(health = recalculateHealth(to, health, _ + _))
    } else {
      to
    }
  }

  private def recalculateHealth(to: Character,
                                health: Int,
                                f: (Int, Int) => Int) = {
    val healthAfterHeal = to.health match {
      case Alive(x) => Health(f(x, health))
      case Dead()   => Dead()
    }
    healthAfterHeal
  }
}
