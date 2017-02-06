package yugioh

import java.util
import java.util.Collections

import yugioh.card.Card.AnyCard
import yugioh.card.library._

import scala.collection.mutable.ListBuffer

trait Deck {
  val cards: ListBuffer[_ <: AnyCard]

  val owner: Player

  def shuffle(): Unit = Collections.shuffle(util.Arrays.asList(cards: _*))

  def fromTop(): AnyCard = fromTop(1).head

  def remaining: Int = cards.size

  /**
    * An EmptyDeck GameLoss exception will be thrown if the deck is empty.
    */
  def fromTop(howMany: Int): Seq[AnyCard] = {
    try {
      for (_ <- 1 to howMany) yield cards.remove(0)
    } catch {
      case _: IndexOutOfBoundsException => throw EmptyDeck(owner)
    }
  }
}

class TestDeck(val owner: Player) extends Deck {
  // TODO LOW: spells+traps for the deck
  override val cards: ListBuffer[AnyCard] = ListBuffer(
    // Monsters
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
    WittyPhantom,
    // Traps
    //CastleWalls,
    //DragonCaptureJar,
    //Reinforcements,
    //ReverseTrap,
    TrapHole,
    //UltimateOffering,
    //Waboku,
    // Spells - though Dian Keto is above for easier testing
    //BookOfSecretArts,
    CardDestruction,
    //ChangeOfHeart
    DarkHole,
    DianKetoTheCureMaster,
    //DeSpell
    //Fissure
    //LastWill
    MonsterReborn
    //RemoveTrap,
    //SoulExchange,
    //SwordOfDarkDestruction,
    //Yami
  ).map(_(owner))
}
