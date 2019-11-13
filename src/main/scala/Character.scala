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
    val healthAfterDamage = receiver.health match {
      case Alive(x) => Health(x - damage)
      case Dead()   => Dead()
    }
    receiver.copy(health = healthAfterDamage)
  }

  def heal(from: Character, to: Character, health: Int): Character = {
    val healthAfterHeal = to.health match {
      case Alive(x) => Health(x + health)
      case Dead()   => Dead()
    }
    to.copy(health = healthAfterHeal)
  }
}
