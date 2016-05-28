package yugioh

import yugioh.action._


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
  protected implicit val implicitFastEffectTiming = this
  override val toString = this.getClass.getSimpleName

  def next(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step = null): FastEffectTiming

  protected def actions(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step = null) = {
    // field includes grave + banished
    Seq(new PassPriorityImpl) ++
      turnPlayers.turnPlayer.hand.flatMap(_.actions) ++
      turnPlayers.turnPlayer.extraDeck.flatMap(_.actions) ++
      turnPlayers.turnPlayer.field.actions ++
      turnPlayers.opponent.field.actions
  }

  protected def opponentActions(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step = null) = {
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
  def loop(start: FastEffectTiming = OpenGameState)
          (implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step = null): Unit = {
    var state = start
    while (state != EndPhaseOrStep) {
      state = state.next
    }
  }
}

/**
  * A in the fast effect timing chart.
  */
object OpenGameState extends FastEffectTiming {
  override def next(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step) = {
    val choice = turnPlayers.turnPlayer.chooseAction(actions)
    choice.execute()
    choice match {
      case pass: PassPriority => TryToEnd
      case activation: Activation => ChainRules
      case action: InherentAction => CheckForTrigger
    }
  }
}

/**
  * B in the fast effect timing chart. Turn player can use fast effects.
  */
object TurnPlayerFastEffects extends FastEffectTiming {
  override def next(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step) = {
    val choice = turnPlayers.turnPlayer.chooseAction(actions)
    choice.execute()
    choice match {
      case pass: PassPriority => OpponentFastEffects
      case activation: Activation => ChainRules
    }
  }
}

/**
  * C in the fast effect timing chart. Opposing player can use fast effects.
  */
object OpponentFastEffects extends FastEffectTiming {
  override def next(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step) = {
    val choice = turnPlayers.opponent.chooseAction(opponentActions)
    choice.execute()
    choice match {
      case pass: PassPriority => OpenGameState
      case activation: Activation => ChainRules
    }
  }
}

/**
  * D in the fast effect timing chart. Build then resolve a chain.
  */
object ChainRules extends FastEffectTiming {
  override def next(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step) = {
    CheckForTrigger // TODO: ChainRules
  }
}

/**
  * E in the fast effect timing chart and the purple box below it.
  *
  * Opponent can use fast effects or pass, and if they pass, we check with both players to see if they wish to end turn.
  */
object TryToEnd extends FastEffectTiming {
  override def next(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step) = {
    val choice = turnPlayers.opponent.chooseAction(opponentActions)
    choice.execute()
    choice match {
      case activation: Activation => ChainRules
      case pass: PassPriority =>
        if (turnPlayers.turnPlayer.consentToEnd && turnPlayers.opponent.consentToEnd) {
          EndPhaseOrStep
        } else {
          OpenGameState
        }
    }
  }
}

/**
  * Represents the yellow box above B in the chart.
  */
object CheckForTrigger extends FastEffectTiming {
  override def next(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step) = {
    TurnPlayerFastEffects // TODO: once a trigger system is in place, this will be able to go to ChainRules
  }
}

/**
  * Signals the end of a phase or step.
  */
object EndPhaseOrStep extends FastEffectTiming {
  override def next(implicit gameState: GameState, turnPlayers: TurnPlayers, phase: Phase, step: Step) = {
    throw new UnsupportedOperationException("End.next should not be called.")
  }
}
