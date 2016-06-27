package yugioh.card.state

/**
  * State associated with a card while it remains on the field.
  */
trait FieldState

case class MonsterFieldState(var howSummoned: HowSummoned) extends FieldState


sealed trait HowSummoned

case object NotSummoned extends HowSummoned

sealed trait NormalSummoned extends HowSummoned
case object NormalSummoned extends NormalSummoned

case object TributeSummoned extends NormalSummoned

case object FlipSummoned extends HowSummoned

sealed trait SpecialSummoned extends HowSummoned
case object SpecialSummoned extends SpecialSummoned

case object FusionSummoned extends SpecialSummoned
case object SynchroSummoned extends SpecialSummoned
case object RitualSummoned extends SpecialSummoned
case object XyzSummoned extends SpecialSummoned
case object PendulumSummoned extends SpecialSummoned
