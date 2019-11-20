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

  it should "juanma tries to heal nelson, but fails" in {
    val nelson = Character(health = Alive(1))
    val juanma = Character()
    val healedNelson = heal(juanma, nelson, 99)
    healedNelson should be(nelson)
  }

  it should "juanma doesnt heal dead juanma" in {
    val deadJuanma = Character(health = Dead())
    val healedJuanma = heal(deadJuanma, deadJuanma, 99)
    healedJuanma should be(deadJuanma)
  }

  it should "when juanma tries to heal a full health juanma, juanma's health does not increase" in {
    val fullytHealthJuanma = Character(health = Alive(1000))
    val healedJuanma = heal(fullytHealthJuanma, fullytHealthJuanma, 1)
    healedJuanma should be(fullytHealthJuanma)
  }

  it should "when juanma tries to heal a full health juanma, juanma's health can not increase over 1000" in {
    val juanma = Character(health = Alive(999))
    val healedJuanma = heal(juanma, juanma, 2)
    val expectedJuanma = Character(health = Alive(1000))
    healedJuanma should be(expectedJuanma)
  }

  it should "when juanma tries to damage himself, nothing happens" in {
    val juanma = Character()
    val damagedJuanma = attack(juanma, juanma, 2)
    damagedJuanma should be(juanma)
  }

  it should "when juanma tries to heal himself, its health increases" in {
    val juanma = Character(Alive(700))
    val healedJuanma = heal(juanma, juanma, 10)
    healedJuanma should be(Character(Alive(710)))
  }
















  it should "If the target is 5 Levels above the attacker, Damage is reduced by 50%" in {
    val javier = Character(level = 1)
    val enrique = Character(Alive(700),level = 6)
    val attackedEnrique = attack(javier, enrique, 10)
    attackedEnrique should be(Character(Alive(695), level = 6))
  }

  it should "If the target is more than 5 Levels above the attacker, Damage is reduced by 50%" in {
    val javier = Character(level = 1)
    val enrique = Character(Alive(700),level = 27)
    val attackedEnrique = attack(javier, enrique, 10)
    attackedEnrique should be(Character(Alive(695), level = 27))
  }

  it should "If the target is 5 levels or below the attacker, Damage is increased by 50%" in {
    val javier = Character(Alive(1000),level = 1)
    val enrique = Character(level = 27)
    val attackedJavier = attack(enrique, javier, 10)
    attackedJavier should be(Character(Alive(985), level = 1))
  }
}
