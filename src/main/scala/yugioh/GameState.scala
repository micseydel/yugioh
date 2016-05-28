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
    players foreach {
      _.draw(Constants.InitialHandSize)
    }

    playersCycle foreach { player =>
      takeTurn(player)
    }
  }

  private def takeTurn(implicit turnPlayer: Player) = {
    turnCount += 1

    println(s"Turn number $turnCount, turn player $turnPlayer.")

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
  private def playersCycle: Iterator[Player] = {
    Iterator.continually(players).flatten
  }
}
