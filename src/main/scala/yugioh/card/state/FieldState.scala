package yugioh.card.state

import yugioh.card.monster.Position

/**
  * State associated with a card while it remains on the field.
  */
trait FieldState {
  def faceup: Boolean
}

trait MonsterFieldState extends FieldState {
  var howSummoned: HowSummoned = NotSummoned
  var maybePosition: Option[Position] = None

  // TODO: these need listeners for the turn ending
  var attackedThisTurn = false
  var manuallyChangedPositionsThisTurn = false // also set to true if attacked

  override def faceup: Boolean = maybePosition.exists(Position.FaceUp.contains)
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
