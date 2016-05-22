package yugioh

import yugioh.action.{Activation, InherentAction, PassPriority, PassPriorityImpl}


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
  def next(implicit turnPlayer: Player, gameState: GameState, phase: Phase, maybeStep: Option[Step]): FastEffectTiming
}

/**
  * A in the fast effect timing chart.
  */
object OpenGameState extends FastEffectTiming {
  override def next(implicit turnPlayer: Player, gameState: GameState, phase: Phase, maybeStep: Option[Step]) = {
    val actions = Seq(new PassPriorityImpl) // TODO

    turnPlayer.chooseAction(actions) match {
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
  override def next(implicit turnPlayer: Player, gameState: GameState, phase: Phase, maybeStep: Option[Step]) = {
    val actions = Seq(new PassPriorityImpl) // TODO

    turnPlayer.chooseAction(actions) match {
      case pass: PassPriority => OpponentFastEffects
      case activation: Activation => ChainRules
    }
  }
}

/**
  * C in the fast effect timing chart. Opposing player can use fast effects.
  */
object OpponentFastEffects extends FastEffectTiming {
  override def next(implicit turnPlayer: Player, gameState: GameState, phase: Phase, maybeStep: Option[Step]) = {
    val actions = Seq(new PassPriorityImpl) // TODO

    turnPlayer.chooseAction(actions) match {
      case pass: PassPriority => OpenGameState
      case activation: Activation => ChainRules
    }
  }
}

/**
  * D in the fast effect timing chart. Build then resolve a chain.
  */
object ChainRules extends FastEffectTiming {
  override def next(implicit turnPlayer: Player, gameState: GameState, phase: Phase, maybeStep: Option[Step]) = {
    // TODO
    CheckForTrigger
  }
}

/**
  * E in the fast effect timing chart and the purple box below it.
  *
  * Opponent can use fast effects or pass, and if they pass, we check with both players to see if they wish to end turn.
  */
object TryToEnd extends FastEffectTiming {
  override def next(implicit turnPlayer: Player, gameState: GameState, phase: Phase, maybeStep: Option[Step]) = {
    val actions = Seq(new PassPriorityImpl) // TODO

    turnPlayer.chooseAction(actions) match {
      case activation: Activation => ChainRules
      case pass: PassPriority =>
        if (turnPlayer.consentToEnd && turnPlayer.opponent.consentToEnd) {
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
  override def next(implicit turnPlayer: Player, gameState: GameState, phase: Phase, maybeStep: Option[Step]) = {
    TurnPlayerFastEffects // TODO: once a trigger system is in place, this will be able to go to D
  }
}

/**
  * Signals the end of a phase or step.
  */
object EndPhaseOrStep extends FastEffectTiming {
  override def next(implicit turnPlayer: Player, gameState: GameState, phase: Phase, maybeStep: Option[Step]) = {
    throw new UnsupportedOperationException("End.next should not be called.")
  }
}
