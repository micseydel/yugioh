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
    new AncientElf(owner),
    new Ansatsu(owner),
    new BaronOfTheFiendSword(owner),
    new BeaverWarrior(owner),
    new CelticGuard(owner),
    new ClawReacher(owner),
    new CurseOfDragon(owner),
    new DarkMagician(owner),
    new DomaTheAngelOfSilence(owner),
    new DragonZombie(owner),
    new FeralImp(owner),
    new GaiaTheFierceKnight(owner),
    new GiantSoldierOfStone(owner),
    new GreatWhite(owner),
    new MagicalGhost(owner),
    new MammothGraveyard(owner),
    new ManEaterBug(owner),
    new ManEatingTreasureChest(owner),
    new MysticClown(owner),
    new MysticalElf(owner),
    new NeoTheMagicSwordsman(owner),
    new SilverFang(owner),
    new SorcererOfTheDoomed(owner),
    new SummonedSkull(owner),
    new TheSternMystic(owner),
    new TrapMaster(owner),
    new WallOfIllusion(owner),
    new WingedDragonGuardOftheFortressNo1(owner),
    new WittyPhantom(owner)
  )
}
