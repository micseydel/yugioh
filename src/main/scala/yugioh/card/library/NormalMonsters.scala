package yugioh.card.library

import yugioh.Player
import yugioh.card.monster._

// this just makes IDE navigation easier
object NormalMonsters

object MysticalElf extends InstantiableCard[MysticalElf]
class MysticalElf(val owner: Player) extends NormalMonster {
  override val PrintedName = "Mystical Elf"
  override val PrintedAttack = 800
  override val PrintedDefense = 2000
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Light
  override val PrintedType = Spellcaster
}

object FeralImp extends InstantiableCard[FeralImp]
class FeralImp(val owner: Player) extends NormalMonster {
  override val PrintedName = "Feral Imp"
  override val PrintedAttack = 1300
  override val PrintedDefense = 1400
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Dark
  override val PrintedType = Fiend
}

object WingedDragonGuardOftheFortressNo1 extends InstantiableCard[WingedDragonGuardOftheFortressNo1]
class WingedDragonGuardOftheFortressNo1(val owner: Player) extends NormalMonster {
  override val PrintedName = "Winged Dragon, Guardian of the Fortress #1"
  override val PrintedAttack = 1400
  override val PrintedDefense = 1200
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Wind
  override val PrintedType = Dragon
}

object SummonedSkull extends InstantiableCard[SummonedSkull]
class SummonedSkull(val owner: Player) extends NormalMonster {
  override val PrintedName = "Summoned Skull"
  override val PrintedAttack = 2500
  override val PrintedDefense = 1200
  override val MaybePrintedLevel = Some(6)
  override val PrintedAttribute = Dark
  override val PrintedType = Fiend
}

object BeaverWarrior extends InstantiableCard[BeaverWarrior]
class BeaverWarrior(val owner: Player) extends NormalMonster {
  override val PrintedName = "Beaver Warrior"
  override val PrintedAttack = 1200
  override val PrintedDefense = 1500
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Earth
  override val PrintedType = BeastWarrior
}

object DarkMagician extends InstantiableCard[DarkMagician]
class DarkMagician(val owner: Player) extends NormalMonster {
  override val PrintedName = "Dark Magician"
  override val PrintedAttack = 2500
  override val PrintedDefense = 2100
  override val MaybePrintedLevel = Some(7)
  override val PrintedAttribute = Dark
  override val PrintedType = Spellcaster
}

object GaiaTheFierceKnight extends InstantiableCard[GaiaTheFierceKnight]
class GaiaTheFierceKnight(val owner: Player) extends NormalMonster {
  override val PrintedName = "Gaia The Fierce Knight"
  override val PrintedAttack = 2300
  override val PrintedDefense = 2100
  override val MaybePrintedLevel = Some(7)
  override val PrintedAttribute = Earth
  override val PrintedType = Warrior
}

object CurseOfDragon extends InstantiableCard[CurseOfDragon]
class CurseOfDragon(val owner: Player) extends NormalMonster {
  override val PrintedName = "Curse of Dragon"
  override val PrintedAttack = 2000
  override val PrintedDefense = 1500
  override val MaybePrintedLevel = Some(5)
  override val PrintedAttribute = Dark
  override val PrintedType = Dragon
}

object CelticGuard extends InstantiableCard[CelticGuard]
class CelticGuard(val owner: Player) extends NormalMonster {
  override val PrintedName = "Celtic Guard"
  override val PrintedAttack = 1400
  override val PrintedDefense = 1200
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Earth
  override val PrintedType = Warrior
}

object MammothGraveyard extends InstantiableCard[MammothGraveyard]
class MammothGraveyard(val owner: Player) extends NormalMonster {
  override val PrintedName = "Mammoth Graveyard"
  override val PrintedAttack = 1200
  override val PrintedDefense = 800
  override val MaybePrintedLevel = Some(3)
  override val PrintedAttribute = Earth
  override val PrintedType = Dinosaur
}

object GreatWhite extends InstantiableCard[GreatWhite]
class GreatWhite(val owner: Player) extends NormalMonster {
  override val PrintedName = "Great White"
  override val PrintedAttack = 1600
  override val PrintedDefense = 800
  override val MaybePrintedLevel = Some(5)
  override val PrintedAttribute = Water
  override val PrintedType = Fish
}

object SilverFang extends InstantiableCard[SilverFang]
class SilverFang(val owner: Player) extends NormalMonster {
  override val PrintedName = "Silver Fang"
  override val PrintedAttack = 1200
  override val PrintedDefense = 800
  override val MaybePrintedLevel = Some(3)
  override val PrintedAttribute = Earth
  override val PrintedType = Beast
}

object GiantSoldierOfStone extends InstantiableCard[GiantSoldierOfStone]
class GiantSoldierOfStone(val owner: Player) extends NormalMonster {
  override val PrintedName = "Giant Soldier of Stone"
  override val PrintedAttack = 1300
  override val PrintedDefense = 2000
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Earth
  override val PrintedType = Rock
}

object DragonZombie extends InstantiableCard[DragonZombie]
class DragonZombie(val owner: Player) extends NormalMonster {
  override val PrintedName = "Dragon Zombie"
  override val PrintedAttack = 1600
  override val PrintedDefense = 0
  override val MaybePrintedLevel = Some(3)
  override val PrintedAttribute = Dark
  override val PrintedType = Zombie
}

object DomaTheAngelOfSilence extends InstantiableCard[DomaTheAngelOfSilence]
class DomaTheAngelOfSilence(val owner: Player) extends NormalMonster {
  override val PrintedName = "Doma the Angel of Silence"
  override val PrintedAttack = 1600
  override val PrintedDefense = 1400
  override val MaybePrintedLevel = Some(5)
  override val PrintedAttribute = Dark
  override val PrintedType = Fairy
}

object Ansatsu extends InstantiableCard[Ansatsu]
class Ansatsu(val owner: Player) extends NormalMonster {
  override val PrintedName = "Ansatsu"
  override val PrintedAttack = 1700
  override val PrintedDefense = 1200
  override val MaybePrintedLevel = Some(5)
  override val PrintedAttribute = Earth
  override val PrintedType = Warrior
}

object WittyPhantom extends InstantiableCard[WittyPhantom]
class WittyPhantom(val owner: Player) extends NormalMonster {
  override val PrintedName = "Witty Phantom"
  override val PrintedAttack = 1400
  override val PrintedDefense = 1300
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Dark
  override val PrintedType = Fiend
}

object ClawReacher extends InstantiableCard[ClawReacher]
class ClawReacher(val owner: Player) extends NormalMonster {
  override val PrintedName = "Claw Reacher"
  override val PrintedAttack = 1000
  override val PrintedDefense = 800
  override val MaybePrintedLevel = Some(3)
  override val PrintedAttribute = Dark
  override val PrintedType = Fiend
}

object MysticClown extends InstantiableCard[MysticClown]
class MysticClown(val owner: Player) extends NormalMonster {
  override val PrintedName = "Mystic Clown"
  override val PrintedAttack = 1500
  override val PrintedDefense = 1000
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Dark
  override val PrintedType = Fiend
}

object AncientElf extends InstantiableCard[AncientElf]
class AncientElf(val owner: Player) extends NormalMonster {
  override val PrintedName = "Ancient Elf"
  override val PrintedAttack = 1450
  override val PrintedDefense = 1200
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Light
  override val PrintedType = Spellcaster
}

object MagicalGhost extends InstantiableCard[MagicalGhost]
class MagicalGhost(val owner: Player) extends NormalMonster {
  override val PrintedName = "Magical Ghost"
  override val PrintedAttack = 1300
  override val PrintedDefense = 1400
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Dark
  override val PrintedType = Zombie
}

object NeoTheMagicSwordsman extends InstantiableCard[NeoTheMagicSwordsman]
class NeoTheMagicSwordsman(val owner: Player) extends NormalMonster {
  override val PrintedName = "Neo the Magic Swordsman"
  override val PrintedAttack = 1700
  override val PrintedDefense = 1000
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Light
  override val PrintedType = Spellcaster
}

object BaronOfTheFiendSword extends InstantiableCard[BaronOfTheFiendSword]
class BaronOfTheFiendSword(val owner: Player) extends NormalMonster {
  override val PrintedName = "Baron of the Fiend Sword"
  override val PrintedAttack = 1550
  override val PrintedDefense = 800
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Dark
  override val PrintedType = Fiend
}

object ManEatingTreasureChest extends InstantiableCard[ManEatingTreasureChest]
class ManEatingTreasureChest(val owner: Player) extends NormalMonster {
  override val PrintedName = "Man-Eating Treasure Chest"
  override val PrintedAttack = 1600
  override val PrintedDefense = 1000
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Dark
  override val PrintedType = Fiend
}

object SorcererOfTheDoomed extends InstantiableCard[SorcererOfTheDoomed]
class SorcererOfTheDoomed(val owner: Player) extends NormalMonster {
  override val PrintedName = "Sorcerer of the Doomed"
  override val PrintedAttack = 1450
  override val PrintedDefense = 1200
  override val MaybePrintedLevel = Some(4)
  override val PrintedAttribute = Dark
  override val PrintedType = Spellcaster
}
