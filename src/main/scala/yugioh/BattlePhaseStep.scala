package yugioh

import yugioh.action.{ActionModule, NoAction, PlayerCause}
import yugioh.action.monster.{Battle, DeclareAttackOnMonster, DeclareDirectAttack}
import yugioh.card.monster.{Attack, Defense, Monster, Set}
import yugioh.events._

sealed trait Step

trait BattlePhaseModule {
  def loop(gameState: GameState)(implicit eventsModule: EventsModule, actionModule: ActionModule)
}

trait BattlePhaseModuleComponent {
  implicit def battlePhaseModule: BattlePhaseModule
}

trait DefaultBattlePhaseModuleComponent extends BattlePhaseModuleComponent {
  override def battlePhaseModule: BattlePhaseModule = new BattlePhaseModule {
    def loop(gameState: GameState)(implicit eventsModule: EventsModule, actionModule: ActionModule): Unit = {
      var battlePhaseStep: BattlePhaseStep = StartStep
      do {
        battlePhaseStep.emitStartEvent()
        val nextBattlePhaseStepAndMonster = battlePhaseStep.next(gameState)
        battlePhaseStep.emitEndEvent()
        battlePhaseStep = nextBattlePhaseStepAndMonster
      } while (battlePhaseStep != null)
    }
  }
}


sealed trait BattlePhaseStep extends Step {
  def emitStartEvent()(implicit eventsModule: EventsModule): Unit = eventsModule.emit(BattlePhaseStepStartEvent(this))

  def emitEndEvent()(implicit eventsModule: EventsModule): Unit = eventsModule.emit(BattlePhaseStepEndEvent(this))

  def next(gameState: GameState)(implicit eventsModule: EventsModule, actionModule: ActionModule): BattlePhaseStep
}

case object StartStep extends BattlePhaseStep {
  override def next(gameState: GameState)(implicit eventsModule: EventsModule, actionModule: ActionModule): BattleStep.type = {
    FastEffectTiming.loop(gameState.copy(step = this))
    BattleStep
  }
}

case object BattleStep extends BattlePhaseStep {
  override def next(gameState: GameState)(implicit eventsModule: EventsModule, actionModule: ActionModule): BattlePhaseStep = {
    var attacker: Monster = null
    var target: Monster = null
    val subscription = eventsModule.observe {
      case ActionEvent(DeclareAttackOnMonster(_, theAttacker, theTarget)) =>
        attacker = theAttacker
        target = theTarget
      case ActionEvent(DeclareDirectAttack(_, theAttacker)) =>
        attacker = theAttacker
    }

    // listen for an attack declaration here
    FastEffectTiming.loop(gameState.copy(step = this))

    subscription.dispose()

    if (attacker != null) {
      BattleStepWithPendingAttack(Battle(attacker, Option(target)))
    } else {
      EndStep
    }
  }
}

case class BattleStepWithPendingAttack(battle: Battle) extends BattlePhaseStep {
  override def next(gameState: GameState)(implicit eventsModule: EventsModule, actionModule: ActionModule): DamageStep = {
    FastEffectTiming.loop(gameState.copy(step = this), start = CheckForTrigger(Nil))
    // TODO: replays - BattleStepWithPendingAttack -> BattleStep instead of DamageStep
    DamageStep(battle)
  }
}

case class DamageStep(battle: Battle) extends BattlePhaseStep {
  override def next(gameState: GameState)(implicit eventsModule: EventsModule, actionModule: ActionModule): BattleStep.type = {
    DamageStepSubStep.loop(battle)(gameState.copy(step = this), eventsModule, actionModule)
    BattleStep
  }
}

case object EndStep extends BattlePhaseStep {
  override def next(gameState: GameState)(implicit eventsModule: EventsModule, actionModule: ActionModule): Null = {
    FastEffectTiming.loop(gameState.copy(step = this))
    null
  }
}


sealed trait DamageStepSubStep extends Step {
  def performAndGetNext(battle: Battle)
                       (implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): DamageStepSubStep
}

object DamageStepSubStep {
  def loop(battle: Battle)(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    var subStep: DamageStepSubStep = StartOfTheDamageStep
    do {
      eventsModule.emit(DamageSubStepStartEvent(subStep))
      val nextSubStep = subStep.performAndGetNext(battle)
      eventsModule.emit(DamageSubStepEndEvent(subStep))
      subStep = nextSubStep
    } while (subStep != null)
  }
}

// http://www.yugioh-card.com/uk/gameplay/damage.html
case object StartOfTheDamageStep extends DamageStepSubStep {
  override def performAndGetNext(battle: Battle)
                                (implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): BeforeDamageCalculation.type = {
    FastEffectTiming.loop(gameState.copy(step = this))
    BeforeDamageCalculation
  }
}

case object BeforeDamageCalculation extends DamageStepSubStep {
  override def performAndGetNext(battle: Battle)
                                (implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): PerformDamageCalculation.type = {
    // flip the target if need be
    battle match {
      case Battle(_, Some(target)) =>
        for (controlledState <- target.maybeControlledState)
          if (controlledState.position == Set) {
            controlledState.position = Defense
            eventsModule.emit(FlippedRegular(target, battle))
          }
      case _ =>
    }

    FastEffectTiming.loop(gameState.copy(step = this))
    PerformDamageCalculation
  }
}

case object PerformDamageCalculation extends DamageStepSubStep {
  override def performAndGetNext(battle: Battle)
                                (implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): AfterDamageCalculation = {
    FastEffectTiming.loop(gameState.copy(step = this))

    var destroyed: Set[Monster] = collection.immutable.Set()
    val subscription = eventsModule.observe {
      case ActionEvent(DestroyByBattle(_, monster, _)) =>
        destroyed += monster
    }

    battle match {
      case Battle(attacker, None) =>
        CauseBattleDamage(PlayerCause(attacker.controller), gameState.turnPlayers.turnPlayer, gameState.turnPlayers.opponent, attacker.attack).execute()
      case Battle(attacker, Some(target)) =>
        for {
          monsterControlledState <- target.maybeControlledState
          position = monsterControlledState.position
          turnPlayer = gameState.turnPlayers.turnPlayer
        }
        position match {
            case Defense =>
              if (attacker.attack > target.defense) {
                if (attacker.isPiercing) {
                  CauseBattleDamage(turnPlayer, gameState.turnPlayers.turnPlayer, gameState.turnPlayers.opponent, attacker.attack - target.defense).execute()
                } else {
                  NoAction(turnPlayer)
                }.andThen(DestroyByBattle(turnPlayer, target, attacker)).execute()
              }
              // TODO: damage from attacking a monster with higher defense
            case Attack =>
              // if both have 0 attack, nothing happens here
              if (attacker.attack != 0 || target.attack != 0) {
                val difference = attacker.attack - target.attack
                (difference.signum match { // get the sign of the difference
                  case -1 => // attacker destroyed
                    CauseBattleDamage(turnPlayer, gameState.turnPlayers.opponent, gameState.turnPlayers.turnPlayer, difference)
                      .also(DestroyByBattle(turnPlayer, attacker, target))
                  case 0 =>
                    DestroyByBattle(turnPlayer, attacker, target)
                      .also(DestroyByBattle(turnPlayer, target, attacker))
                  case 1 => // target destroyed
                    CauseBattleDamage(turnPlayer, gameState.turnPlayers.opponent, gameState.turnPlayers.turnPlayer, difference)
                      .also(DestroyByBattle(turnPlayer, target, attacker))
                }).execute()
              }
            case Set =>
              throw new IllegalStateException("Attacked monster should have been flipped face up.")
          }

    }

    subscription.dispose()

    AfterDamageCalculation(destroyed)
  }
}

case class AfterDamageCalculation(destroyed: Set[Monster]) extends DamageStepSubStep {
  override def performAndGetNext(battle: Battle)
                                (implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): EndOfTheDamageStep = {
    FastEffectTiming.loop(gameState.copy(step = this))
    EndOfTheDamageStep(destroyed)
  }
}

case class EndOfTheDamageStep(destroyed: Set[Monster]) extends DamageStepSubStep {
  override def performAndGetNext(battle: Battle)
                                (implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Null = {
    for (monster <- destroyed) {
      monster.sendToGrave(battle.attacker.controller)
      DestroyByBattle(monster.controller, monster, battle.attacker).execute()
    }

    FastEffectTiming.loop(gameState.copy(step = this))
    null
  }
}
