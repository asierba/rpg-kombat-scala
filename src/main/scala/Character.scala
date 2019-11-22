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

trait Character{
  val health: Health
  val level: Int
}
case class MeleeCharacter(health: Health = Health(1000), level: Int = 1) extends Character
case class RangedCharacter(health: Health = Health(1000), level: Int = 1) extends Character

object Character {
  val DamageThreshold = 5

  def copyWithHealth(character: Character, newHealth: Health): Character = {
    character match {
      case x: MeleeCharacter => x.copy(health = newHealth)
      case x: RangedCharacter => x.copy(health = newHealth)
    }
  }

  def attack(attacker: Character, receiver: Character, damage: Int, distance: Int) = {
    if (attacker == receiver) {
      attacker
    } else {
      val newDamage = recalculateDamage(attacker, receiver, damage)
      Character.copyWithHealth(receiver, recalculateHealth(receiver, newDamage, _ - _))
    }
  }

  private def recalculateDamage(attacker: Character, receiver: Character, damage: Int): Double = {
    val levelDiff = attacker.level - receiver.level

    val greaterAttacker = levelDiff >= DamageThreshold
    val lowerAttacker = levelDiff <= -DamageThreshold
    (greaterAttacker, lowerAttacker) match  {
      case (true, _) =>  damage * 1.5
      case (_, true) =>  damage / 2
      case _ => damage
    }
  }

  def heal(from: Character, to: Character, health: Int): Character = {
    if (from == to) {
      Character.copyWithHealth(to, recalculateHealth(to, health, _ + _))
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
