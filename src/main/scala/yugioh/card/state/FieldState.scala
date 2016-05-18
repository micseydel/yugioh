package yugioh.card.state

import yugioh.card.Card
import yugioh.card.monster.{Monster, Position}

/**
  * State associated with a card while it remains on the field.
  */
trait FieldState {
  def faceup(implicit card: Card): Boolean
}

trait MonsterFieldState extends FieldState {
  var howSummoned: HowSummoned = NotSummoned

  override def faceup(implicit card: Card): Boolean = {
    // though not technically exhaustive, this should be an invariant
    card match {
      case monster: Monster => monster.maybePosition.exists(Position.FaceUp.contains)
    }
  }
}


sealed trait HowSummoned

case object NotSummoned extends HowSummoned
case object NormalSummoned extends HowSummoned
case object FlipSummoned extends HowSummoned

sealed trait SpecialSummoned extends HowSummoned
case object SpecialSummoned extends HowSummoned

case object FusionSummoned extends SpecialSummoned
case object SynchroSummoned extends SpecialSummoned
case object RitualSummoned extends SpecialSummoned
case object XyzSummoned extends SpecialSummoned
case object PendulumSummoned extends SpecialSummoned
