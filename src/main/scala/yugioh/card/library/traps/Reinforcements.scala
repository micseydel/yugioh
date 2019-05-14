package yugioh.card.library.traps

import yugioh._
import yugioh.action._
import yugioh.card.Card.AnyCard
import yugioh.card.library.InstantiableCard
import yugioh.card.monster.Monster
import yugioh.card.state.AttackDelta
import yugioh.card.{Effect, EffectType, NormalTrap, TrapEffect}
import yugioh.events.{EventsModule, PhaseStartEvent}

object Reinforcements extends InstantiableCard[Reinforcements]
class Reinforcements(val Owner: Player) extends NormalTrap {
  override val PrintedName = "Trap Hole"

  override val Effects: List[Effect] = List(ReinforcementsEffect)

  object ReinforcementsEffect extends TrapEffect {
    override val Card: Reinforcements = Reinforcements.this
    override val EffectType: EffectType = Effect
    override val maybeCostCriteria: Option[Criteria[AnyCard]] = None
    override val Cost: InherentAction = NoAction(Card.Owner)

    /**
      * Override normal trap behavior, for cards like Compulsory Evacuation Device, which are more lenient with timing.
      */
    override def activationTimingCorrect(implicit gameState: GameState): Boolean = {
      gameState match {
        case GameState(_, _, _, _, StartOfTheDamageStep | BeforeDamageCalculation, _) =>
          true
        case _ =>
          super.activationTimingCorrect
      }
    }

    override val maybeTargetCriteria: Option[Criteria[Monster]] = Some(new Criteria[Monster] {
      /**
        * Can the player possibly meet the requirements?
        */
      override def meetable(implicit gameState: GameState): Boolean = {
        gameState.turnPlayers.both.exists { player =>
          player.field.monsterZones.exists { zone =>
            // we're ok with using get() here because monsters in monster zones should have controlled state
            zone.exists(_.maybeControlledState.get.faceup)
          }
        }
      }

      /**
        * Verify that the subject of availableChoices which has been selected is valid.
        */
      override def validSelection[T >: Monster](choices: Seq[T])(implicit gameState: GameState): Boolean = {
        choices match {
          case Seq(monster: Monster) if monster.maybeControlledState.get.faceup =>
            true
          case _ =>
            false
        }
      }

      /**
        * Available choices to fulfill the requirements.
        */
      override def availableChoices(implicit gameState: GameState): List[Monster] = {
        // faceup monsters
        val choices = for {
          player <- gameState.turnPlayers.both
          monsters <- player.field.monsterZones
          monster <- monsters if monster.maybeControlledState.get.faceup
        } yield {
          monster
        }

        choices.toList
      }
    })

    override val Resolution: InherentAction = new InherentAction {
      override val cause: Cause = PlayerCause(Card.Owner)

      override def doAction()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
        val target = selectedTargets match {
          case Seq(target@MonsterTarget(_: Monster, _)) =>
            target
          case _ =>
            throw new AssertionError(s"selectedTargets should have contained a single monster, not $selectedTargets")
        }

        if (target.validTarget) {
          val attackIncrease = AttackDelta(500)
          // it's a valid target, so it must still be on the field
          val state = target.card.maybeMonsterFieldState.get
          state.attackModifiers = attackIncrease :: state.attackModifiers

          eventsModule.observe {
            case PhaseStartEvent(EndPhase) =>
              state.attackModifiers = state.attackModifiers.filterNot(_ eq attackIncrease)
          }
        }
      }
    }
  }
}
