package yugioh

import java.util
import java.util.Collections

import yugioh.card.Card
import yugioh.card.library._

import scala.collection.mutable.ListBuffer

trait Deck {
  val cards: ListBuffer[_ <: Card]

  val owner: Player

  def shuffle(): Unit = Collections.shuffle(util.Arrays.asList(cards: _*))

  def fromTop(): Card = fromTop(1).head

  def remaining = cards.size

  /**
    * An EmptyDeck GameLoss exception will be thrown if the deck is empty.
    */
  def fromTop(howMany: Int): Seq[Card] = {
    try {
      for (_ <- 1 to howMany) yield cards.remove(0)
    } catch {
      case outOfBounds: IndexOutOfBoundsException => throw EmptyDeckImpl
    }
  }
}

class TestDeck(val owner: Player) extends Deck {
  // TODO: spells+traps for the deck
  val cards = ListBuffer(
    AncientElf,
    Ansatsu,
    BaronOfTheFiendSword,
    BeaverWarrior,
    CelticGuard,
    ClawReacher,
    CurseOfDragon,
    DarkMagician,
    DomaTheAngelOfSilence,
    DragonZombie,
    FeralImp,
    GaiaTheFierceKnight,
    GiantSoldierOfStone,
    GreatWhite,
    MagicalGhost,
    MammothGraveyard,
    ManEaterBug,
    ManEatingTreasureChest,
    MysticClown,
    MysticalElf,
    NeoTheMagicSwordsman,
    SilverFang,
    SorcererOfTheDoomed,
    SummonedSkull,
    TheSternMystic,
    TrapMaster,
    WallOfIllusion,
    WingedDragonGuardOftheFortressNo1,
    WittyPhantom
  ).map(_(owner))
}
