import org.scalatest.{FlatSpec, Matchers}

case class Character(health: Int = 1000, level: Int = 1, isAlive: Boolean = true)

class RpgSpec extends FlatSpec with Matchers {

  it should "create a character" in {
    Character() should equal(Character(health = 1000, level = 1, isAlive = true))
  }

  def attack(attacker: Character, receiver: Character, damage: 1) = {
    Character(health = receiver.health - 1, level = receiver.level, isAlive = receiver.isAlive)
  }

  it should "damage another default character" in {
    val nelson = Character()
    val juanma = Character(level = 3)

    val damagedJuanma = attack(nelson, juanma, 1)

    damagedJuanma should be (Character(health = 999, level = 3))
  }
}

