package yugioh.card

import yugioh.action.{ActionModule, InherentAction, NoAction}
import yugioh.events.EventsModule
import yugioh.{Criteria, GameState, Player}


sealed trait EffectType

object Effect extends EffectType
object Ignition extends EffectType
object Trigger extends EffectType
object Flip extends EffectType
object Quick extends EffectType
object Continuous extends EffectType
object Condition extends EffectType
object Unclassified extends EffectType

sealed trait SpellSpeed

object SpellSpeed1 extends SpellSpeed
object SpellSpeed2 extends SpellSpeed
object SpellSpeed3 extends SpellSpeed
object SpellSpeed4 extends SpellSpeed

/**
  * An effect will go on chain, and its activation will occur, and its resolution will occur as the chain resolves.
  */
trait Effect {
  val Card: Card

  val EffectType: EffectType

  val Compulsory: Boolean = false

  /**
    * Inferred from EffectType, but can be overridden.
    */
  lazy val SpellSpeed: SpellSpeed = EffectType match {
    case Effect => SpellSpeed1
    case Ignition => SpellSpeed1
    case Trigger => SpellSpeed1
    case Flip => SpellSpeed1
    case Quick => SpellSpeed2
    case Continuous => null
    case Condition => null
    case Unclassified => null
  }

  val maybeTargetCriteria: Option[Criteria[Card]]

  /**
    * Conditions for activation. Infers costs and targets are meetable.
    *
    * TODO: infer state changes (e.g. "once per turn")
    */
  val ActivationConditions: Conditions = new Conditions {
    override def met(implicit gameState: GameState): Boolean = {
      activationTimingCorrect &&
        Seq(maybeCostCriteria, maybeTargetCriteria).flatten.forall(_.meetable) &&
        specialActivationConditionsMet.getOrElse(true)
    }
  }

  def activationTimingCorrect(implicit gameState: GameState): Boolean

  /**
    * Special conditions such as Dark Hole requiring at least one monster on the field, or Card Destruction requiring enough cards in deck.
    */
  def specialActivationConditionsMet(implicit gameState: GameState): Option[Boolean] = None

  /**
    * Things like placing a spell on the field, flipping a set S/T, and placeholder for things like "once per turn."
    */
  lazy val StateChange: InherentAction = NoAction(Card.controller)

  // TODO LOW: this should include things like discarding, which may be disallowed, and costs that include life points
  val maybeCostCriteria: Option[Criteria[Card]]

  val Cost: InherentAction

  lazy val SelectTargets: InherentAction = maybeTargetCriteria.map(TargetCriteria => new InherentAction {
    override protected def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
      player.selectEffectTargets(TargetCriteria)
    }

    override val player: Player = Card.controller
  }).getOrElse(NoAction(Card.controller))

  // TODO LOW: are targets ever selected before cost?
  // DSL should be able to write "StateChange andThen Cost andThen SelectTargets"
  lazy val Activation: InherentAction = StateChange.andThen(Cost).andThen(SelectTargets)

  val Resolution: InherentAction

  protected[this] var selectedTargets: Seq[Card] = _

  /**
    * Helper method for determining if an effect targets or not.
    */
  final def doesTarget(implicit gameState: GameState) = maybeTargetCriteria.isDefined
}

/**
  * Any condition(s) for activation; this includes valid targets and fast effect timing.
  */
trait Conditions {
  // TODO LOW: declarativeness, e.g. ".when().youCan()"
  def met(implicit gameState: GameState): Boolean
}

trait FlipEffect extends Effect
