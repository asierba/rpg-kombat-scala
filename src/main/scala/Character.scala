case class Character(health: Int = 1000, level: Int = 1, isAlive: Boolean = true)

object Character {
  def attack(attacker: Character, receiver: Character, damage: Int) = {
    val healthAfterDamage = if (damage > receiver.health) 0 else receiver.health - damage
    receiver.copy(health = healthAfterDamage, isAlive = healthAfterDamage != 0)
  }

  def heal(from: Character, to: Character, health: Int): Character = {
    to match {
      case Character(x,_,_) if (x + health) > 1000 => to.copy(health = 1000)
      case Character(_,_,true) => to.copy(health = to.health + health)
      case _ => to
    }
  }
}
