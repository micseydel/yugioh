package yugioh.card.monster

import yugioh.GameState
import yugioh.card.Card
import yugioh.card.Delegate
import yugioh.action.Action
import yugioh.card.state.HowSummoned

trait Monster extends Card {
  val printedAttack: Int
  val printedDefense: Int
  val maybePrintedLevel: Option[Int]
  val maybePrintedRank: Option[Int]
  val printedAttribute: Attribute
  val printedType: Type

  def maybePosition: Option[Position]
  def attack: Int = printedAttack
  def defense: Int = printedDefense
  def maybeLevel: Option[Int] = maybePrintedLevel
  def maybeRank: Option[Int] = maybePrintedRank
  def attribute: Attribute = printedAttribute
  def _type: Type = printedType

  var howSummoned: HowSummoned

  var attackedThisTurn = false
  var manuallyChangedPositionsThisTurn = false // also set to true if attacked

  def getActions(implicit gameState: GameState): Seq[Action]
}


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
