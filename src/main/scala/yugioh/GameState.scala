package yugioh

import yugioh.action.Action

trait GameState {
  val players: Seq[Player]

  var turnCount = 0
  var hasNormalSummonedThisTurn: Boolean = false
  var history: List[Action] = Nil

  def mainLoop(): Unit
}

class GameStateImpl(val players: Seq[Player]) extends GameState {
  private implicit val gameState = this

  def mainLoop() = {
    for (player <- players) {
      player.draw(Constants.InitialHandSize)
    }

    for (players <- playersCycle) {
      takeTurn(players)
    }
  }

  private def takeTurn(implicit turnPlayers: TurnPlayers) = {
    turnCount += 1

    println(s"Turn number $turnCount, turn player ${turnPlayers.turnPlayer}.")

    hasNormalSummonedThisTurn = false // TODO: move this once an event based system is in place

    implicit var phase: Phase = DrawPhase
    while (phase != EndTurn) {
      println(s"Entering $phase")
      phase = phase.next
    }
  }

  /**
    * An infinite cycle alternating between the two players.
    */
  private def playersCycle: Iterator[TurnPlayers] = {
    Iterator.continually(Seq(TurnPlayers(players(0), players(1)), TurnPlayers(players(1), players(0)))).flatten
  }
}
