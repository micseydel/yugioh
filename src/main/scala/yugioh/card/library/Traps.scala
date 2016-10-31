package yugioh.card.library

import yugioh._
import yugioh.action.monster.NormalOrFlipSummon
import yugioh.action.{ActionModule, InherentAction, NoAction}
import yugioh.card.monster.Monster
import yugioh.card.trap.{NormalTrap, TrapEffect}
import yugioh.card.{Card, Effect, EffectType}
import yugioh.events.{ActionEvent, EventsModule}

// mostly for IDE navigation
private[this] object Traps

object TrapHole extends InstantiableCard[TrapHole]
class TrapHole(val Owner: Player) extends NormalTrap {
  override val PrintedName = "Trap Hole"

  override val Effects: List[Effect] = List(TrapHoleEffect)

  object TrapHoleEffect extends TrapEffect {
    override val Card = TrapHole.this
    override val EffectType: EffectType = Effect
    override val maybeCostCriteria: Option[Criteria[Card]] = None
    override val Cost: InherentAction = NoAction(Card.Owner)

    override def activationTimingCorrect(implicit gameState: GameState): Boolean = {
      gameState match {
        case GameState(_, _, _, _, _: DamageStep, _) =>
          // TODO LOW: refactor this (no activation in damage step) logic to be checked for in an ancestor
          false
        case GameState(_, _, _: CheckForTrigger | _: PlayerFastEffects | _: ChainRules, _, _, _) =>
          true
        case _ =>
          false
      }
    }

    override val maybeTargetCriteria: Option[Criteria[Card]] = Some(new Criteria[Card] {
      /**
        * Can the player possibly meet the requirements?
        */
      override def meetable(implicit gameState: GameState): Boolean = {
        gameState.inResponseTo.exists {
          case ActionEvent(normalOrFlipSummon: NormalOrFlipSummon) =>
            normalOrFlipSummon.monster.attack > 1000
          case _ =>
            false
        }
      }

      /**
        * Verify that the subject of availableChoices which has been selected is valid.
        */
      override def validSelection(choices: Seq[Card])(implicit gameState: GameState): Boolean = {
        choices match {
          case Seq(monster: Monster) if monster.attack > 1000 =>
            true
          case _ =>
            false
        }
      }

      /**
        * Available choices to fulfill the requirements.
        */
      override def availableChoices(implicit gameState: GameState): Seq[Card] = {
        val choices = gameState.inResponseTo.collect {
          case ActionEvent(normalOrFlipSummon: NormalOrFlipSummon) if normalOrFlipSummon.monster.attack > 1000 =>
            normalOrFlipSummon.monster
        }

        assert(choices.length < 2, "There wasn't supposed to be a situation where inResponseTo had multiple normal/flip summons at the same time.")

        choices
      }
    })

    override val Resolution: InherentAction = new InherentAction {
      override val player: Player = Card.Owner

      override def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
        val target = selectedTargets match {
          case Seq(monster: Monster) =>
            monster
          case _ =>
            throw new AssertionError(s"selectedTargets should have contained a single monster, not $selectedTargets")
        }

        target.destroy()
      }
    }
  }
}
