package yugioh

import yugioh.action._
import yugioh.action.monster.DeclareAttack
import yugioh.events.Event

import scala.collection.mutable


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

  protected def actionsForPlayer(player: Player)(implicit gameState: GameState) = {
    if (gameState.turnPlayers.turnPlayer == player) {
      turnPlayerActions
    } else {
      opponentActions
    }
  }

  protected def turnPlayerActions(implicit gameState: GameState) = {
    val turnPlayers = gameState.turnPlayers

    // field includes grave + banished
    Seq(new PassPriority(turnPlayers.turnPlayer)) ++
      turnPlayers.turnPlayer.hand.flatMap(_.actions) ++
      turnPlayers.turnPlayer.extraDeck.flatMap(_.actions) ++
      turnPlayers.turnPlayer.field.actions ++
      turnPlayers.opponent.field.actions
  }

  protected def opponentActions(implicit gameState: GameState) = {
    val turnPlayers = gameState.turnPlayers

    // field includes grave + banished
    Seq(new PassPriority(turnPlayers.opponent)) ++
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
    val choice = gameState.turnPlayers.turnPlayer.chooseAction(turnPlayerActions)
    choice.execute() match {
      case pass: PassPriority => TryToEnd
      case activation: ExistsInAChainAction => ChainRules(activation, Nil)
      case attackDeclaration: DeclareAttack => null // BattlePhaseStep will change, manually go to ChainRules
      case action: InherentAction => CheckForTrigger(List(action))
    }
  }
}

/**
  * B in the fast effect timing chart. Turn player can use fast effects.
  */
case class TurnPlayerFastEffects(inResponseTo: List[Event]) extends FastEffectTiming {
  override def next(implicit gameState: GameState) = nextWithUpdatedGameState(gameState.copy(inResponseTo = inResponseTo))

  private def nextWithUpdatedGameState(implicit gameState: GameState) = {
    val choice = gameState.turnPlayers.turnPlayer.chooseAction(turnPlayerActions)
    choice.execute() match {
      case pass: PassPriority => OpponentFastEffects
      case activation: ExistsInAChainAction => ChainRules(activation, inResponseTo)
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
      case activation: ExistsInAChainAction => ChainRules(activation, Nil)
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
case class ChainRules(existsInAChainAction: ExistsInAChainAction, inResponseTo: List[Event]) extends FastEffectTiming {
  override def next(implicit gameState: GameState) = nextWithUpdatedGameState(gameState.copy(inResponseTo = inResponseTo))

  var chain = mutable.Stack(existsInAChainAction)

  private def nextWithUpdatedGameState(implicit gameState: GameState) = {
    // TODO LOW: more sophisticated chains will require changes to this code

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
        case existsInAChainAction: ExistsInAChainAction =>
          existsInAChainAction.execute()
          chain.push(existsInAChainAction)
          passedPreviously = false
          player = gameState.turnPlayers.other(chain.head.player)
      }
    }

    // resolve the chain
    while (chain.nonEmpty) {
      val existsInAChainAction: ExistsInAChainAction = chain.pop()
      existsInAChainAction.resolve()
      existsInAChainAction match {
        case CardActivation(card, Some(effect)) =>
          existsInAChainAction.player.field.sendToGrave(card)
        case ignore =>
      }
    }

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
      case activation: ExistsInAChainAction => ChainRules(activation, Nil)
      case pass: PassPriority =>
        if (turnPlayers.turnPlayer.consentToEnd && turnPlayers.opponent.consentToEnd) {
          if (gameState.phase == EndPhase && turnPlayers.turnPlayer.hand.size > Constants.HandSizeLimit) {
            // TODO LOW: discarding for turn may result in trigger effects; this should be updated once the trigger effect system is in place
            new DiscardForHandSizeLimitImpl().execute()
          }
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
case class CheckForTrigger(inResponseTo: List[Event]) extends FastEffectTiming {
  override def next(implicit gameState: GameState) = nextWithUpdatedGameState(gameState.copy(inResponseTo = inResponseTo))

  private def nextWithUpdatedGameState(implicit gameState: GameState) = {
    // TODO: once a proper trigger system is in place, this will be able to go to ChainRules
    TurnPlayerFastEffects(inResponseTo)
  }
}
