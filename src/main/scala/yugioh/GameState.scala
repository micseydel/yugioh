package yugioh

import yugioh.action.{Action, NormalSummon, SetAsMonster}
import yugioh.events.Observable._
import yugioh.events.{TurnEndEvent, TurnStartEvent}

trait GameState {
  val Players: (Player, Player)
  val IterablePlayers = Seq(Players._1,Players._1)

  var turnCount = 0
  var hasNormalSummonedThisTurn: Boolean = false
  var history: List[Action] = Nil

  def mainLoop(): Unit
}

class GameStateImpl(val Players: (Player, Player)) extends GameState {
  private implicit val gameState = this

  def mainLoop() = {
    setupObservables()

    // before turns start, each player draws
    for (player <- IterablePlayers) {
      player.draw(Constants.InitialHandSize)
    }

    // loop until a game loss exception is thrown
    // TODO LOW: catch the game loss exception here and do stuff based on it
    for (turnPlayers <- playersCycle) {
      takeTurn(turnPlayers)
    }
  }

  private def takeTurn(implicit turnPlayers: TurnPlayers) = {
    turnCount += 1

    emit(TurnStartEvent(turnPlayers, this))
    Phase.loop()
    emit(TurnEndEvent)
  }

  /**
    * An infinite cycle alternating between the two players.
    */
  private def playersCycle: Iterator[TurnPlayers] = {
    val (player1, player2) = Players
    Iterator.continually(Seq(TurnPlayers(player1, player2), TurnPlayers(player2, player1))).flatten
  }

  private def setupObservables() = {
    // set hook for clearing turn state
    observe { event =>
      event match {
        case TurnStartEvent(_, _) =>
          hasNormalSummonedThisTurn = false
        case ignore =>
      }
    }

    // listen for a normal summon or set, flag that it happened
    observe { event =>
      event match {
        case _:NormalSummon | _:SetAsMonster =>
          hasNormalSummonedThisTurn = true
        case ignore =>
      }
    }
  }
}
