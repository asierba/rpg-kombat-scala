trait Health {}

case class Alive(health: Double) extends Health

case class Dead() extends Health

case class Character(health: Health = Health(1000), level: Int = 1)

object Health {
  def apply(health: Double): Health = {
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
      val newDamage = recalculateDamage(attacker, receiver, damage)
      receiver.copy(health = recalculateHealth(receiver, newDamage, _ - _))
    }
  }

  private def recalculateDamage(attacker: Character, receiver: Character, damage: Int): Double = {
    if (attacker.level - receiver.level >= 5) {
      damage * 1.5
    }else if (receiver.level - attacker.level >= 5) {
      damage / 2
    } else {
      damage
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
                                health: Double,
                                f: (Double, Double) => Double) = {
    val healthAfterHeal = to.health match {
      case Alive(x) => Health(f(x, health))
      case Dead() => Dead()
    }
    healthAfterHeal
  }
}
