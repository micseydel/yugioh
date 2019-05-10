package yugioh

import yugioh.card.Card.AnyCard
import yugioh.card.library._
import yugioh.card.library.traps.TrapHole

import scala.collection.mutable.ListBuffer

trait Deck {
  val cards: ListBuffer[_ <: AnyCard]

  val owner: Player

  def shuffle(): Unit

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

trait DeckModule {
  def newDeck(player: Player): Deck
}

trait DeckModuleComponent {
  def deckModule: DeckModule
}

trait DefaultDeckModuleComponent extends DeckModuleComponent {
  self: RandomnessModuleComponent =>

  //noinspection ConvertExpressionToSAM
  def deckModule: DeckModule = new DeckModule {
    override def newDeck(player: Player): Deck = new Deck {

      override def shuffle(): Unit = randomness.shuffle(cards)

      override val owner: Player = player

      override val cards: ListBuffer[_ <: AnyCard] = ListBuffer[InstantiableCard[_ <: AnyCard]](
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
  }
}
