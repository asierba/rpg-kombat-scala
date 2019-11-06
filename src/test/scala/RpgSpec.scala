import org.scalatest.{FlatSpec, Matchers}

case class Character(health: Int = 1000, level: Int = 1, isAlive: Boolean = true)

class RpgSpec extends FlatSpec with Matchers {

  it should "create a character" in {
    Character() should equal(Character(health = 1000, level = 1, isAlive = true))
  }

  def attack(attacker: Character, receiver: Character, damage: Int) = {
    val healthAfterDamage = if (damage > receiver.health) 0 else receiver.health - damage
    receiver.copy(health = healthAfterDamage, isAlive = healthAfterDamage != 0)
  }

  it should "damage another default character" in {
    val nelson = Character()
    val juanma = Character(level = 3)

    val damagedJuanma = attack(nelson, juanma, 1)

    damagedJuanma should be(Character(health = 999, level = 3))
  }

  it should "damage another default character till die" in {
    val nelson = Character()
    val juanma = Character(level = 3)

    val damagedJuanma = attack(nelson, juanma, 1000)

    damagedJuanma should be(Character(health = 0, level = 3, isAlive = false))
  }

  it should "damage another character with health lower than damage" in {
    val nelson = Character()
    val juanma = Character(level = 3, health = 50)

    val damagedJuanma = attack(nelson, juanma, 1000)

    damagedJuanma should be(Character(health = 0, level = 3, isAlive = false))
  }


  def heal(from: Character, to: Character, health: Int): Character = {
    to match {
      case Character(x,_,_) if (x + health) > 1000 => to.copy(health = 1000)
      case Character(_,_,true) => to.copy(health = to.health + health)
      case _ => to
    }
  }

  it should "juanma heals nelson" in {
    val nelson = Character(health = 1)
    val juanma = Character()
    val healedNelson = heal(juanma, nelson, 99)
    healedNelson should be(Character(health = 100))
  }

  it should "juanma doesnt heal dead nelson" in {
    val deadNelson = Character(health = 0, isAlive = false)
    val juanma = Character()
    val healedNelson = heal(juanma, deadNelson, 99)
    healedNelson should be(deadNelson)
  }

  it should "when juanma tries to heal a full health nelson, nelson health does not increase" in {
    val fullytHealthNelson = Character(health = 1000)
    val juanma = Character()
    val healedNelson = heal(juanma, fullytHealthNelson, 1)
    healedNelson should be(fullytHealthNelson)
  }

  it should "when juanma tries to heal a full health nelson, nelson health can not increase over 1000" in {
    val nelson = Character(health = 999)
    val juanma = Character()
    val healedNelson = heal(juanma, nelson, 2)
    val expectedNelson = Character(health = 1000)
    healedNelson should be(expectedNelson)
  }

  it should "when juanma tries to heal a full health dead nelson, nelson health does not increase" in {
    val fullytHealthNelson = Character(health = 1000, isAlive = false)
    val juanma = Character()
    val healedNelson = heal(juanma, fullytHealthNelson, 1)
    healedNelson should be(fullytHealthNelson)
  }

}

