package yugioh.card.state

import yugioh.card.monster._

/**
  * State associated with a card while it remains on the field.
  */
trait FieldState

case class MonsterFieldState(monster: Monster, var howSummoned: HowSummoned) extends FieldState {
  def properlySummoned: Boolean = {
    monster match {
      case _: FusionMonster => FusionSummoned(monster)
      case _: SynchroMonster => SynchroSummoned(monster)
      case _: RitualMonster => RitualSummoned(monster)
      case _: XyzMonster => XyzSummoned(monster)
      case _: SemiNomi => true // TODO: need way to keep track of this
      case _ => true // by default, this is true, special cases are the only times it's not
    }
  }
}

sealed trait HowSummoned

/**
  * Makes the objects callable to see if a card has them as the location or not.
  */
sealed trait SimpleHowSummonedChecker extends HowSummoned {
  def apply(monster: Monster): Boolean = monster.maybeMonsterFieldState.exists(_.howSummoned == this)
}

/**
  * Such as when being set, or going straight to the graveyard from hand or deck.
  */
case object NotSummoned extends SimpleHowSummonedChecker

sealed trait NormalSummoned extends SimpleHowSummonedChecker
case object NormalSummoned extends NormalSummoned

case object TributeSummoned extends NormalSummoned

case object FlipSummoned extends SimpleHowSummonedChecker

sealed trait SpecialSummoned extends SimpleHowSummonedChecker
case object SpecialSummoned extends SpecialSummoned

case object FusionSummoned extends SpecialSummoned
case object SynchroSummoned extends SpecialSummoned
case object RitualSummoned extends SpecialSummoned
case object XyzSummoned extends SpecialSummoned
case object PendulumSummoned extends SpecialSummoned
