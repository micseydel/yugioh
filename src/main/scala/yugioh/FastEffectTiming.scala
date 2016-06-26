package yugioh

import yugioh.action._
import yugioh.action.monster.DeclareAttack
import yugioh.events.Event


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

  def next(implicit gameState: GameState): FastEffectTiming

  protected def actions(implicit gameState: GameState) = {
    val turnPlayers = gameState.turnPlayers

    // field includes grave + banished
    Seq(new PassPriorityImpl) ++
      turnPlayers.turnPlayer.hand.flatMap(_.actions) ++
      turnPlayers.turnPlayer.extraDeck.flatMap(_.actions) ++
      turnPlayers.turnPlayer.field.actions ++
      turnPlayers.opponent.field.actions
  }

  protected def opponentActions(implicit gameState: GameState) = {
    val turnPlayers = gameState.turnPlayers

    // field includes grave + banished
    Seq(new PassPriorityImpl) ++
      turnPlayers.opponent.hand.flatMap(_.actions) ++
      turnPlayers.opponent.field.actions ++
      turnPlayers.turnPlayer.field.actions
  }
}

object FastEffectTiming {
  /**
    * Defaults to starting in open game state, but allows for starting elsewhere (e.g. CheckForTrigger at start of DP).
    */
  def loop(gameState: GameState, start: FastEffectTiming = OpenGameState): Unit = {
    var state = start
    while (state != null) {
      state = state.next(gameState.copy(fastEffectTiming = state))
    }
  }
}

/**
  * A in the fast effect timing chart.
  */
object OpenGameState extends FastEffectTiming {
  override def next(implicit gameState: GameState) = {
    val choice = gameState.turnPlayers.turnPlayer.chooseAction(actions)
    choice.execute() match {
      case pass: PassPriority => TryToEnd
      case activation: Activation => ChainRules(None)
      case attackDeclaration: DeclareAttack => null // BattlePhaseStep will change, manually go to ChainRules
      case action: InherentAction => CheckForTrigger(action)
    }
  }
}

/**
  * B in the fast effect timing chart. Turn player can use fast effects.
  */
case class TurnPlayerFastEffects(inResponseTo: Event) extends FastEffectTiming {
  override def next(implicit gameState: GameState) = nextWithUpdatedGameState(gameState.copy(inResponseTo = inResponseTo))

  private def nextWithUpdatedGameState(implicit gameState: GameState) = {
    val choice = gameState.turnPlayers.turnPlayer.chooseAction(actions)
    choice.execute() match {
      case pass: PassPriority => OpponentFastEffects
      case activation: Activation => ChainRules(Some(inResponseTo))
    }
  }
}

/**
  * C in the fast effect timing chart. Opposing player can use fast effects.
  */
object OpponentFastEffects extends FastEffectTiming {
  override def next(implicit gameState: GameState) = {
    val choice = gameState.turnPlayers.opponent.chooseAction(opponentActions)
    choice.execute() match {
      case pass: PassPriority => OpenGameState
      case activation: Activation => ChainRules(None)
    }
  }
}

/**
  * D in the fast effect timing chart. Build then resolve a chain.
  *
  * @param maybeInResponseTo MUST be provided when coming from an event which triggered an effect,
  *                          or from B or C in the fast effect timing chart,
  *                          and MUST be None coming from E.
  */
case class ChainRules(maybeInResponseTo: Option[Event]) extends FastEffectTiming {
  override def next(implicit gameState: GameState) = nextWithUpdatedGameState(gameState.copy(inResponseTo = maybeInResponseTo.orNull))

  private def nextWithUpdatedGameState(implicit gameState: GameState) = {
    // in damage calculation, we only get a single chain
    //   if this condition is true, we deviate from what the fast effect timing chart documents
    if (gameState.step == PerformDamageCalculation) {
      TryToEnd
    } else {
      CheckForTrigger(null) // TODO: ChainRules need to communicate last thing to happen here
    }
  }
}

/**
  * E in the fast effect timing chart and the purple box below it.
  *
  * Opponent can use fast effects or pass, and if they pass, we check with both players to see if they wish to end turn.
  */
object TryToEnd extends FastEffectTiming {
  override def next(implicit gameState: GameState) = {
    val turnPlayers = gameState.turnPlayers
    val choice = turnPlayers.opponent.chooseAction(opponentActions)

    choice.execute() match {
      case activation: Activation => ChainRules(None)
      case pass: PassPriority =>
        if (turnPlayers.turnPlayer.consentToEnd && turnPlayers.opponent.consentToEnd) {
          null
        } else {
          OpenGameState
        }
    }
  }
}

/**
  * Represents the yellow box above B in the chart.
  */
case class CheckForTrigger(inResponseTo: Event) extends FastEffectTiming {
  override def next(implicit gameState: GameState) = nextWithUpdatedGameState(gameState.copy(inResponseTo = inResponseTo))

  private def nextWithUpdatedGameState(implicit gameState: GameState) = {
    // TODO: once a proper trigger system is in place, this will be able to go to ChainRules
    TurnPlayerFastEffects(inResponseTo)
  }
}
