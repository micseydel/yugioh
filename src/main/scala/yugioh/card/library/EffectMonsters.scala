package yugioh.card.library

import yugioh.Player
import yugioh.card.monster._

// this just makes IDE navigation easier
object EffectMonsters

object TheSternMystic extends InstantiableCard[TheSternMystic]
class TheSternMystic(val owner: Player) extends EffectMonster {
  override val printedName = "The Stern Mystic"
  override val printedAttack = 1500
  override val printedDefense = 1200
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Light
  override val printedType = Spellcaster
  // TODO: TheSternMystic effect implementation
}

object WallOfIllusion extends InstantiableCard[WallOfIllusion]
class WallOfIllusion(val owner: Player) extends EffectMonster {
  override val printedName = "Wall of Illusion"
  override val printedAttack = 1000
  override val printedDefense = 1850
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Dark
  override val printedType = Fiend
  // TODO: WallOfIllusion effect implementation
}

object TrapMaster extends InstantiableCard[TrapMaster]
class TrapMaster(val owner: Player) extends FlipMonster {
  override val printedName = "Trap Master"
  override val printedAttack = 500
  override val printedDefense = 1100
  override val maybePrintedLevel = Some(3)
  override val printedAttribute = Earth
  override val printedType = Warrior
  // TODO: TrapMaster effect implementation
}

object ManEaterBug extends InstantiableCard[ManEaterBug]
class ManEaterBug(val owner: Player) extends FlipMonster {
  override val printedName = "Man-Eater Bug"
  override val printedAttack = 450
  override val printedDefense = 600
  override val maybePrintedLevel = Some(2)
  override val printedAttribute = Earth
  override val printedType = Insect
  // TODO: ManEaterBug effect implementation
}
