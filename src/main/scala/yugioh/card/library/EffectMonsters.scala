package yugioh.card.library

import yugioh.Player
import yugioh.card.Effect
import yugioh.card.monster._

// this just makes IDE navigation easier
object EffectMonsters

object TheSternMystic extends InstantiableCard[TheSternMystic]
class TheSternMystic(val Owner: Player) extends FlipMonster {
  override val PrintedName = "The Stern Mystic"
  override val PrintedAttack = 1500
  override val PrintedDefense = 1200
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Light
  override val PrintedType = Spellcaster

  override lazy val Effects: Seq[Effect] = ??? // TODO: TheSternMystic effect implementation - http://yugioh.wikia.com/wiki/The_Stern_Mystic
}

object WallOfIllusion extends InstantiableCard[WallOfIllusion]
class WallOfIllusion(val Owner: Player) extends EffectMonster {
  override val PrintedName = "Wall of Illusion"
  override val PrintedAttack = 1000
  override val PrintedDefense = 1850
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Dark
  override val PrintedType = Fiend

  override lazy val Effects: Seq[Effect] = ??? // TODO: WallOfIllusion effect implementation - http://yugioh.wikia.com/wiki/The_Stern_Mystic
}

object TrapMaster extends InstantiableCard[TrapMaster]
class TrapMaster(val Owner: Player) extends FlipMonster {
  override val PrintedName = "Trap Master"
  override val PrintedAttack = 500
  override val PrintedDefense = 1100
  override val MaybePrintedLevel = Some(3)
  override val PrintedAttribute = Earth
  override val PrintedType = Warrior

  override lazy val Effects: Seq[Effect] = ??? // TODO: TrapMaster effect implementation - http://yugioh.wikia.com/wiki/Trap_Master
}

object ManEaterBug extends InstantiableCard[ManEaterBug]
class ManEaterBug(val Owner: Player) extends FlipMonster {
  override val PrintedName = "Man-Eater Bug"
  override val PrintedAttack = 450
  override val PrintedDefense = 600
  override val MaybePrintedLevel = Some(2)
  override val PrintedAttribute = Earth
  override val PrintedType = Insect

  override lazy val Effects: Seq[Effect] = ??? // TODO: ManEaterBug effect implementation - http://yugioh.wikia.com/wiki/Man-Eater_Bug
}
