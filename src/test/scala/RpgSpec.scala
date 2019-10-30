import org.scalatest.{FlatSpec, Matchers}

case class Character(health: Int = 1000, level: Int = 1, isAlive: Boolean = true)

class RpgSpec extends FlatSpec with Matchers {

  it should "create a character" in {
    Character() should equal(Character(health = 1000, level = 1, isAlive = true))
  }

  def attack(attacker: Character, receiver: Character, damage: Int) = {
    val newCharacterHealth = receiver.health - damage
    receiver.copy(health = newCharacterHealth, isAlive = newCharacterHealth != 0)
  }

  it should "damage another default character" in {
    val nelson = Character()
    val juanma = Character(level = 3)

    val damagedJuanma = attack(nelson, juanma, 1)

    damagedJuanma should be (Character(health = 999, level = 3))
  }

  it should "damage another default character till die" in {
    val nelson = Character()
    val juanma = Character(level = 3)

    val damagedJuanma = attack(nelson, juanma, 1000)

    damagedJuanma should be (Character(health = 0, level = 3, isAlive = false))
  }
}

