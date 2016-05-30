package yugioh.card.library

import yugioh.Player
import yugioh.card.monster._

// this just makes IDE navigation easier
object NormalMonsters

object MysticalElf extends InstantiableCard[MysticalElf]
class MysticalElf(val owner: Player) extends NormalMonster {
  override val printedName = "Mystical Elf"
  override val printedAttack = 800
  override val printedDefense = 2000
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Light
  override val printedType = Spellcaster
}

object FeralImp extends InstantiableCard[FeralImp]
class FeralImp(val owner: Player) extends NormalMonster {
  override val printedName = "Feral Imp"
  override val printedAttack = 1300
  override val printedDefense = 1400
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Dark
  override val printedType = Fiend
}

object WingedDragonGuardOftheFortressNo1 extends InstantiableCard[WingedDragonGuardOftheFortressNo1]
class WingedDragonGuardOftheFortressNo1(val owner: Player) extends NormalMonster {
  override val printedName = "Winged Dragon, Guardian of the Fortress #1"
  override val printedAttack = 1400
  override val printedDefense = 1200
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Wind
  override val printedType = Dragon
}

object SummonedSkull extends InstantiableCard[SummonedSkull]
class SummonedSkull(val owner: Player) extends NormalMonster {
  override val printedName = "Summoned Skull"
  override val printedAttack = 2500
  override val printedDefense = 1200
  override val maybePrintedLevel = Some(6)
  override val printedAttribute = Dark
  override val printedType = Fiend
}

object BeaverWarrior extends InstantiableCard[BeaverWarrior]
class BeaverWarrior(val owner: Player) extends NormalMonster {
  override val printedName = "Beaver Warrior"
  override val printedAttack = 1200
  override val printedDefense = 1500
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Earth
  override val printedType = BeastWarrior
}

object DarkMagician extends InstantiableCard[DarkMagician]
class DarkMagician(val owner: Player) extends NormalMonster {
  override val printedName = "Dark Magician"
  override val printedAttack = 2500
  override val printedDefense = 2100
  override val maybePrintedLevel = Some(7)
  override val printedAttribute = Dark
  override val printedType = Spellcaster
}

object GaiaTheFierceKnight extends InstantiableCard[GaiaTheFierceKnight]
class GaiaTheFierceKnight(val owner: Player) extends NormalMonster {
  override val printedName = "Gaia The Fierce Knight"
  override val printedAttack = 2300
  override val printedDefense = 2100
  override val maybePrintedLevel = Some(7)
  override val printedAttribute = Earth
  override val printedType = Warrior
}

object CurseOfDragon extends InstantiableCard[CurseOfDragon]
class CurseOfDragon(val owner: Player) extends NormalMonster {
  override val printedName = "Curse of Dragon"
  override val printedAttack = 2000
  override val printedDefense = 1500
  override val maybePrintedLevel = Some(5)
  override val printedAttribute = Dark
  override val printedType = Dragon
}

object CelticGuard extends InstantiableCard[CelticGuard]
class CelticGuard(val owner: Player) extends NormalMonster {
  override val printedName = "Celtic Guard"
  override val printedAttack = 1400
  override val printedDefense = 1200
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Earth
  override val printedType = Warrior
}

object MammothGraveyard extends InstantiableCard[MammothGraveyard]
class MammothGraveyard(val owner: Player) extends NormalMonster {
  override val printedName = "Mammoth Graveyard"
  override val printedAttack = 1200
  override val printedDefense = 800
  override val maybePrintedLevel = Some(3)
  override val printedAttribute = Earth
  override val printedType = Dinosaur
}

object GreatWhite extends InstantiableCard[GreatWhite]
class GreatWhite(val owner: Player) extends NormalMonster {
  override val printedName = "Great White"
  override val printedAttack = 1600
  override val printedDefense = 800
  override val maybePrintedLevel = Some(5)
  override val printedAttribute = Water
  override val printedType = Fish
}

object SilverFang extends InstantiableCard[SilverFang]
class SilverFang(val owner: Player) extends NormalMonster {
  override val printedName = "Silver Fang"
  override val printedAttack = 1200
  override val printedDefense = 800
  override val maybePrintedLevel = Some(3)
  override val printedAttribute = Earth
  override val printedType = Beast
}

object GiantSoldierOfStone extends InstantiableCard[GiantSoldierOfStone]
class GiantSoldierOfStone(val owner: Player) extends NormalMonster {
  override val printedName = "Giant Soldier of Stone"
  override val printedAttack = 1300
  override val printedDefense = 2000
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Earth
  override val printedType = Rock
}

object DragonZombie extends InstantiableCard[DragonZombie]
class DragonZombie(val owner: Player) extends NormalMonster {
  override val printedName = "Dragon Zombie"
  override val printedAttack = 1600
  override val printedDefense = 0
  override val maybePrintedLevel = Some(3)
  override val printedAttribute = Dark
  override val printedType = Zombie
}

object DomaTheAngelOfSilence extends InstantiableCard[DomaTheAngelOfSilence]
class DomaTheAngelOfSilence(val owner: Player) extends NormalMonster {
  override val printedName = "Doma the Angel of Silence"
  override val printedAttack = 1600
  override val printedDefense = 1400
  override val maybePrintedLevel = Some(5)
  override val printedAttribute = Dark
  override val printedType = Fairy
}

object Ansatsu extends InstantiableCard[Ansatsu]
class Ansatsu(val owner: Player) extends NormalMonster {
  override val printedName = "Ansatsu"
  override val printedAttack = 1700
  override val printedDefense = 1200
  override val maybePrintedLevel = Some(5)
  override val printedAttribute = Earth
  override val printedType = Warrior
}

object WittyPhantom extends InstantiableCard[WittyPhantom]
class WittyPhantom(val owner: Player) extends NormalMonster {
  override val printedName = "Witty Phantom"
  override val printedAttack = 1400
  override val printedDefense = 1300
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Dark
  override val printedType = Fiend
}

object ClawReacher extends InstantiableCard[ClawReacher]
class ClawReacher(val owner: Player) extends NormalMonster {
  override val printedName = "Claw Reacher"
  override val printedAttack = 1000
  override val printedDefense = 800
  override val maybePrintedLevel = Some(3)
  override val printedAttribute = Dark
  override val printedType = Fiend
}

object MysticClown extends InstantiableCard[MysticClown]
class MysticClown(val owner: Player) extends NormalMonster {
  override val printedName = "Mystic Clown"
  override val printedAttack = 1500
  override val printedDefense = 1000
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Dark
  override val printedType = Fiend
}

object AncientElf extends InstantiableCard[AncientElf]
class AncientElf(val owner: Player) extends NormalMonster {
  override val printedName = "Ancient Elf"
  override val printedAttack = 1450
  override val printedDefense = 1200
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Light
  override val printedType = Spellcaster
}

object MagicalGhost extends InstantiableCard[MagicalGhost]
class MagicalGhost(val owner: Player) extends NormalMonster {
  override val printedName = "Magical Ghost"
  override val printedAttack = 1300
  override val printedDefense = 1400
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Dark
  override val printedType = Zombie
}

object NeoTheMagicSwordsman extends InstantiableCard[NeoTheMagicSwordsman]
class NeoTheMagicSwordsman(val owner: Player) extends NormalMonster {
  override val printedName = "Neo the Magic Swordsman"
  override val printedAttack = 1700
  override val printedDefense = 1000
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Light
  override val printedType = Spellcaster
}

object BaronOfTheFiendSword extends InstantiableCard[BaronOfTheFiendSword]
class BaronOfTheFiendSword(val owner: Player) extends NormalMonster {
  override val printedName = "Baron of the Fiend Sword"
  override val printedAttack = 1550
  override val printedDefense = 800
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Dark
  override val printedType = Fiend
}

object ManEatingTreasureChest extends InstantiableCard[ManEatingTreasureChest]
class ManEatingTreasureChest(val owner: Player) extends NormalMonster {
  override val printedName = "Man-Eating Treasure Chest"
  override val printedAttack = 1600
  override val printedDefense = 1000
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Dark
  override val printedType = Fiend
}

object SorcererOfTheDoomed extends InstantiableCard[SorcererOfTheDoomed]
class SorcererOfTheDoomed(val owner: Player) extends NormalMonster {
  override val printedName = "Sorcerer of the Doomed"
  override val printedAttack = 1450
  override val printedDefense = 1200
  override val maybePrintedLevel = Some(4)
  override val printedAttribute = Dark
  override val printedType = Spellcaster
}
