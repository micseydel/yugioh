package yugioh

import yugioh.action._
import yugioh.action.monster.DeclareAttack
import yugioh.card.NonContinuousSpellOrTrap
import yugioh.events.{EffectActivationNegationEvent, Event, EventsModule, TimeSeparationEvent}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
  * There is a nice visual at www.yugioh-card.com/en/gameplay/fasteffects_timing.html
  *
  * Here is a brief mapping:
  * A - OpenGameState
  * B - TurnPlayerFastEffects
  * C - OpponentFastEffects
  * D - ChainRules
  * E - TryToEnd (note that the purple box below is combined into TryToEnd)
  *
  * There are two additional states, CheckForTrigger which represents the yellow box above B, and End to signal
  * the end of a phase or step.
  */
sealed trait FastEffectTiming {
  override val toString = this.getClass.getSimpleName

  def next(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): FastEffectTiming

  protected def actionsForPlayer(player: Player)(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    if (gameState.turnPlayers.turnPlayer == player) {
      turnPlayerActions
    } else {
      opponentActions
    }
  }

  protected def turnPlayerActions(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    val turnPlayers = gameState.turnPlayers

    // field includes grave + banished
    Seq(PassPriority(turnPlayers.turnPlayer)) ++
      turnPlayers.turnPlayer.hand.flatMap(_.actions) ++
      turnPlayers.turnPlayer.extraDeck.flatMap(_.actions) ++
      turnPlayers.turnPlayer.field.actions ++
      turnPlayers.opponent.field.actions
  }

  protected def opponentActions(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    val turnPlayers = gameState.turnPlayers

    // field includes grave + banished
    Seq(PassPriority(turnPlayers.opponent)) ++
      turnPlayers.opponent.hand.flatMap(_.actions) ++
      turnPlayers.opponent.field.actions ++
      turnPlayers.turnPlayer.field.actions
  }
}

object FastEffectTiming {
  /**
    * Defaults to starting in open game state, but allows for starting elsewhere (e.g. CheckForTrigger at start of DP).
    */
  def loop(gameState: GameState, start: FastEffectTiming = OpenGameState)
          (implicit eventsModule: EventsModule, actionModule: ActionModule) = {
    var state = start
    while (state != null) {
      state = state.next(gameState.copy(fastEffectTiming = state), eventsModule, actionModule)
    }
  }
}

/**
  * A in the fast effect timing chart.
  */
object OpenGameState extends FastEffectTiming {
  override def next(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    val choice = gameState.turnPlayers.turnPlayer.chooseAction(turnPlayerActions)

    val action = choice.execute()

    eventsModule.emit(TimeSeparationEvent)

    action match {
      case pass: PassPriority => TryToEnd
      case activation: Activation => ChainRules(activation, Nil)
      case attackDeclaration: DeclareAttack => null // BattlePhaseStep will change, manually go to ChainRules
      case action: InherentAction => CheckForTrigger(List(action))
    }
  }
}

/**
  * B in the fast effect timing chart. Turn player can use fast effects.
  */
case class TurnPlayerFastEffects(inResponseTo: List[Event]) extends FastEffectTiming {
  override def next(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    nextWithUpdatedGameState(gameState.copy(inResponseTo = inResponseTo), eventsModule, actionModule)
  }

  private def nextWithUpdatedGameState(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    val choice = gameState.turnPlayers.turnPlayer.chooseAction(turnPlayerActions)

    val action = choice.execute()

    eventsModule.emit(TimeSeparationEvent)

    action match {
      case pass: PassPriority => OpponentFastEffects
      case activation: Activation => ChainRules(activation, inResponseTo)
      case _: InherentAction => throw new IllegalStateException("Inherent actions cannot be taken when game state is closed.")
    }
  }
}

/**
  * C in the fast effect timing chart. Opposing player can use fast effects.
  */
object OpponentFastEffects extends FastEffectTiming {
  override def next(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    val choice = gameState.turnPlayers.opponent.chooseAction(opponentActions)

    val action = choice.execute()

    eventsModule.emit(TimeSeparationEvent)

    action match {
      case pass: PassPriority => OpenGameState
      case activation: Activation => ChainRules(activation, Nil)
      case _: InherentAction => throw new IllegalStateException("Inherent actions cannot be taken by the opposing player.")
    }
  }
}

/**
  * D in the fast effect timing chart. Build then resolve a chain.
  *
  * @param inResponseTo MUST be non-Nil when coming from an event which triggered an effect,
  *                     or from B or C in the fast effect timing chart,
  *                     and MUST be None coming from E.
  */
case class ChainRules(activation: Activation, inResponseTo: List[Event]) extends FastEffectTiming {
  override def next(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    nextWithUpdatedGameState(gameState.copy(inResponseTo = inResponseTo), eventsModule, actionModule)
  }

  var chain = mutable.Stack(activation)

  private def nextWithUpdatedGameState(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    // listen to all events, and keep track of the "last thing to happen" which can include ***multiple*** events (e.g. Blackship of Corn)
    //   anytime a TimeSeparationEvent occurs, we drop all the old events
    // also listen for effect activation negation, and remove those effects from the chain
    var lastThingToHappen = new ListBuffer[Event]()
    val subscription = eventsModule.observe { event =>
      event match {
        case TimeSeparationEvent =>
          lastThingToHappen = new ListBuffer[Event]()
        case EffectActivationNegationEvent(_, _) =>
          chain.pop()
        case _ =>
          lastThingToHappen.append(event)
      }
    }

    // build the chain
    var keepBuilding = true
    var passedPreviously = false
    var player = gameState.turnPlayers.other(chain.top.player)
    while (keepBuilding) {
      player.chooseAction(actionsForPlayer(player)) match {
        case passPriority: PassPriority =>
          if (passedPreviously) {
            keepBuilding = false
          } else {
            passedPreviously = true
            player = gameState.turnPlayers.other(chain.top.player)
          }
        case activation: Activation =>
          activation.execute()
          eventsModule.emit(TimeSeparationEvent)
          chain.push(activation)
          passedPreviously = false
          player = gameState.turnPlayers.other(chain.head.player)
        case _: InherentAction => throw new IllegalStateException("Inherent actions cannot be taken when game state is closed.")
      }
    }

    // resolve the chain
    val nonContinuousSpellTraps = new ListBuffer[NonContinuousSpellOrTrap]
    while (chain.nonEmpty) {
      val activation: Activation = chain.pop()
      activation.Effect.Resolution.execute()

      // we can't emit this event when the last chain link is resolved, because non-continuous S/T cleanup
      //   is considered simultaneous with the last thing to happen in the chain
      if (chain.nonEmpty) {
        eventsModule.emit(TimeSeparationEvent)
      }

      // keep track of the normal spells and traps so that we can remove them after the chain is empty
      activation.Effect.Card match {
        case noncontinuous: NonContinuousSpellOrTrap =>
          nonContinuousSpellTraps.append(noncontinuous)
        case _ =>
      }
    }

    // tell normal spells/traps to go to the grave after the chain has resolved
    for (noncontinuous <- nonContinuousSpellTraps) {
      noncontinuous.afterChainCleanup()
    }

    subscription.dispose()

    // we do want to emit this after the cleanup, but we must wait until we've removed our observer
    eventsModule.emit(TimeSeparationEvent)

    // in damage calculation, we only get a single chain
    //   if this condition is true, we deviate from what the fast effect timing chart documents
    if (gameState.step == PerformDamageCalculation) {
      TryToEnd
    } else {
      CheckForTrigger(lastThingToHappen.toList)
    }
  }
}

/**
  * E in the fast effect timing chart and the purple box below it.
  *
  * Opponent can use fast effects or pass, and if they pass, we check with both players to see if they wish to end turn.
  */
object TryToEnd extends FastEffectTiming {
  override def next(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    val turnPlayers = gameState.turnPlayers
    val choice = turnPlayers.opponent.chooseAction(opponentActions)

    val action = choice.execute()

    eventsModule.emit(TimeSeparationEvent)

    action match {
      case activation: Activation => ChainRules(activation, Nil)
      case pass: PassPriority =>
        if (turnPlayers.turnPlayer.consentToEnd && turnPlayers.opponent.consentToEnd) {
          if (gameState.phase == EndPhase && turnPlayers.turnPlayer.hand.size > Constants.HandSizeLimit) {
            // TODO LOW: discarding for turn may result in trigger effects; this should be updated once the trigger effect system is in place
            actionModule.newDiscardForHandSizeLimit().execute()
            eventsModule.emit(TimeSeparationEvent)
          }
          null
        } else {
          OpenGameState
        }
      case _: InherentAction => throw new IllegalStateException("Inherent actions cannot be taken when game state is closed.")
    }
  }
}

/**
  * Represents the yellow box above B in the chart.
  */
case class CheckForTrigger(inResponseTo: List[Event]) extends FastEffectTiming {
  override def next(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    nextWithUpdatedGameState(gameState.copy(inResponseTo = inResponseTo))
  }

  private def nextWithUpdatedGameState(implicit gameState: GameState) = {
    // TODO: should be able to go to ChainRules from here, must check for trigger effects
    TurnPlayerFastEffects(inResponseTo)
  }
}
