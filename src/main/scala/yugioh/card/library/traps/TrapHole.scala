package yugioh.card.library.traps

import yugioh.{Criteria, GameState, Player, PlayerFastEffects}
import yugioh.action._
import yugioh.action.monster.NormalOrFlipSummon
import yugioh.card.Card.AnyCard
import yugioh.card.{Effect, EffectType, NormalTrap, TrapEffect}
import yugioh.card.library.InstantiableCard
import yugioh.card.monster.Monster
import yugioh.events.{ActionEvent, EventsModule}

object TrapHole extends InstantiableCard[TrapHole]
class TrapHole(val Owner: Player) extends NormalTrap {
  override val PrintedName = "Trap Hole"

  override val Effects: List[Effect] = List(TrapHoleEffect)

  object TrapHoleEffect extends TrapEffect { effect =>
    override val Card: TrapHole = TrapHole.this
    override val EffectType: EffectType = Effect
    override val maybeCostCriteria: Option[Criteria[AnyCard]] = None
    override val Cost: InherentAction = NoAction(Card.Owner)

    /**
      * Override normal trap behavior, for cards like Compulsory Evacuation Device, which are more lenient with timing.
      */
    override def activationTimingCorrect(implicit gameState: GameState): Boolean = {
      gameState match {
        case GameState(_, _, _: PlayerFastEffects, _, _, _) =>
          false
        case _ =>
          super.activationTimingCorrect
      }
    }

    override val maybeTargetCriteria: Option[Criteria[Monster]] = Some(new Criteria[Monster] {
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
      override def validSelection[T >: Monster](choices: Seq[T])(implicit gameState: GameState): Boolean = {
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
      override def availableChoices(implicit gameState: GameState): List[Monster] = {
        val choices = gameState.inResponseTo.collect {
          case ActionEvent(normalOrFlipSummon: NormalOrFlipSummon) if normalOrFlipSummon.monster.attack > 1000 =>
            normalOrFlipSummon.monster
        }

        assert(choices.length < 2, "There wasn't supposed to be a situation where inResponseTo had multiple normal/flip summons at the same time.")

        choices
      }
    })

    override val Resolution: InherentAction = new InherentAction {
      override val cause: Cause = EffectCause(effect, Card.Owner)

      override def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
        val target: MonsterTarget = selectedTargets match {
          case Seq(target@MonsterTarget(_, _)) =>
            target
          case _ =>
            throw new AssertionError(s"selectedTargets should have contained a single monster, not $selectedTargets")
        }

        if (target.validTarget) {
          val destructionCause = EffectCause(effect, cause)
          target.card.destroy(destructionCause)
        }
      }
    }
  }
}
