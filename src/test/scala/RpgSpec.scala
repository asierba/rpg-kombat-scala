import org.scalatest.{FlatSpec, Matchers}
import Character._

class RpgSpec extends FlatSpec with Matchers {

  it should "create a character" in {
    Character() should equal(Character(health = Alive(1000)))
  }

  it should "damage another default character" in {
    val nelson = Character()
    val juanma = Character(level = 3)

    val damagedJuanma = attack(nelson, juanma, 1)

    damagedJuanma should be(Character(health = Alive(999), level = 3))
  }

  it should "damage another default character till die" in {
    val nelson = Character()
    val juanma = Character(level = 3)

    val damagedJuanma = attack(nelson, juanma, 1000)

    damagedJuanma should be(Character(health = Dead(), level = 3))
  }

  it should "damage another character with health lower than damage" in {
    val nelson = Character()
    val juanma = Character(level = 3, health = Alive(50))

    val damagedJuanma = attack(nelson, juanma, 1000)

    damagedJuanma should be(Character(health = Dead(), level = 3))
  }

  it should "juanma heals nelson" in {
    val nelson = Character(health = Alive(1))
    val juanma = Character()
    val healedNelson = heal(juanma, nelson, 99)
    healedNelson should be(Character(health = Alive(100)))
  }

  it should "juanma doesnt heal dead nelson" in {
    val deadNelson = Character(health = Dead())
    val juanma = Character()
    val healedNelson = heal(juanma, deadNelson, 99)
    healedNelson should be(deadNelson)
  }

  it should "when juanma tries to heal a full health nelson, nelson health does not increase" in {
    val fullytHealthNelson = Character(health = Alive(1000))
    val juanma = Character()
    val healedNelson = heal(juanma, fullytHealthNelson, 1)
    healedNelson should be(fullytHealthNelson)
  }

  it should "when juanma tries to heal a full health nelson, nelson health can not increase over 1000" in {
    val nelson = Character(health = Alive(999))
    val juanma = Character()
    val healedNelson = heal(juanma, nelson, 2)
    val expectedNelson = Character(health = Alive(1000))
    healedNelson should be(expectedNelson)
  }

}
