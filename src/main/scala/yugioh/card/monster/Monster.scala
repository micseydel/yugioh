package yugioh.card.monster

import yugioh.card.state.MonsterFieldState
import yugioh.card.{Card, Delegate}
import yugioh._
import yugioh.action.{NormalSummonImpl, TributeSummonImpl}

trait Monster extends Card {
  val printedAttack: Int
  val printedDefense: Int
  val maybePrintedLevel: Option[Int]
  val maybePrintedRank: Option[Int] = None
  val printedAttribute: Attribute
  val printedType: Type

  def attack: Int = printedAttack
  def defense: Int = printedDefense
  def maybeLevel: Option[Int] = maybePrintedLevel
  def maybeRank: Option[Int] = maybePrintedRank
  def attribute: Attribute = printedAttribute
  def monsterType: Type = printedType

  override def fieldState: Option[MonsterFieldState] = None

  /**
    * Default implementation of being able to normal/tribute summon during main phases
    */
  override def actions(implicit gameState: GameState, turnPlayer: Player, phase: Phase, maybeStep: Option[Step]) = {
    phase match {
      case MainPhase | MainPhase2 if !gameState.hasNormalSummonedThisTurn =>
        //TODO need to be able to change position
        maybeLevel.map { level =>
          if (level <= 4) {
            Seq(new NormalSummonImpl(this))
          } else {
            Seq(new TributeSummonImpl(this))
          }
        }.getOrElse(Seq())
      case BattlePhase =>
        // TODO
        Seq()
      case _ =>
        Seq()
    }
  }
}

trait NormalMonster extends Monster


sealed trait Position

// TODO low: there is technically a way to be face down and in attack mode
case object Set extends Position // in defense mode
case object Attack extends Position
case object Defense extends Position

object Position {
  val FaceUp: Set[Position] = scala.collection.immutable.Set(Attack, Defense)
}


sealed trait Attribute

case object Dark extends Attribute
case object Earth extends Attribute
case object Fire extends Attribute
case object Light extends Attribute
case object Water extends Attribute
case object Wind extends Attribute


sealed trait Type

case object Aqua extends Type
case object Beast extends Type
case object BeastWarrior extends Type
case object CreatorGod extends Type
case object Dinosaur extends Type
case object DivineBeast extends Type
case object Dragon extends Type
case object Fairy extends Type
case object Fiend extends Type
case object Fish extends Type
case object Insect extends Type
case object Machine extends Type
case object Plant extends Type
case object Psychic extends Type
case object Pyro extends Type
case object Reptile extends Type
case object Rock extends Type
case object SeaSerpent extends Type
case object Spellcaster extends Type
case object Thunder extends Type
case object Warrior extends Type
case object WingedBeast extends Type
case object Wyrm extends Type
case object Zombie extends Type

/**
  * This is essentially a wrapper that allows a SpellOrTrap to be treated as a Monster card.
  */
trait MonsterDelegate extends Delegate with Monster
