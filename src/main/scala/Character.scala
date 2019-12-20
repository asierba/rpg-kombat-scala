case class Faction(name: String)

trait Character {
  val health: Health
  val level: Int
  val name: String
  val factions: Set[Faction]

  def copy(health: Health = this.health, level: Int = this.level, name: String = this.name, factions: Set[Faction] = this.factions): Character = {
    this match {
      case _: MeleeCharacter => MeleeCharacter(health, level, name, factions)
      case _: RangedCharacter => RangedCharacter(health, level, name, factions)
    }
  }
}

case class MeleeCharacter(health: Health = Health(1000), level: Int = 1, name: String = "Melee Warrior", factions: Set[Faction] = Set()) extends Character

case class RangedCharacter(health: Health = Health(1000), level: Int = 1, name: String = "Ranged Warrior", factions: Set[Faction] = Set()) extends Character

object Character {
  val DamageThreshold = 5

  def attack(attacker: Character, receiver: Character, damage: Int, distance: Int) = {
    if (attacker == receiver) {
      attacker
    } else {
      attacker match {
        case _: MeleeCharacter if (distance > 2) => receiver
        case _: RangedCharacter if (distance > 20) => receiver
        case _ => {
          val newDamage = recalculateDamage(attacker, receiver, damage)
          receiver.copy(health = recalculateHealth(receiver, newDamage, _ - _))
        }
      }
    }
  }

  private def recalculateDamage(attacker: Character, receiver: Character, damage: Int): Double = {
    val levelDiff = attacker.level - receiver.level
    val greaterAttacker = levelDiff >= DamageThreshold
    val lowerAttacker = levelDiff <= -DamageThreshold
    (greaterAttacker, lowerAttacker) match {
      case (true, _) => damage * 1.5
      case (_, true) => damage / 2
      case _ => damage
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
    to.health match {
      case Alive(x) => Health(f(x, health))
      case Dead() => Dead()
    }
  }

  def joinFaction(character: Character, faction: Faction): Character =
    character.copy(factions = character.factions + faction)
}

