package yugioh.card.library

import yugioh.action.{InherentAction, NoAction}
import yugioh.card.trap.{NormalTrap, TrapEffect}
import yugioh.card.{Card, Effect, EffectType}
import yugioh.{Criteria, GameState, Player}

// mostly for IDE navigation
private[this] object Traps

object TrapHole extends InstantiableCard[TrapHole]
class TrapHole(val Owner: Player) extends NormalTrap {
  override val PrintedName = "Trap Hole"

  override val Effects: List[Effect] = List(TrapHoleEffect)

  object TrapHoleEffect extends TrapEffect {
    override val Card: Card = TrapHole.this
    override val EffectType: EffectType = Effect
    override val maybeCostCriteria: Option[Criteria[Card]] = None
    override val Cost: InherentAction = NoAction(Card.Owner)

    // TODO: Trap Hole
    override val maybeTargetCriteria: Option[Criteria[Card]] = ???
    override def activationTimingCorrect(implicit gameState: GameState): Boolean = ???
    override val Resolution: InherentAction = ???
  }
}
