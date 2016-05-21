package yugioh

import yugioh.Util.intWithTimes
import yugioh.action.{Action, DiscardImpl, PassPriorityImpl}

trait GameState {
  val players: Seq[Player]

  var turnCount = 0
  var hasNormalSummonedThisTurn: Boolean = false
  var history: List[Action] = Nil

  def mainLoop(): Unit
}

class GameStateImpl(val players: Seq[Player]) extends GameState {
  private val InitialHandSize = 5
  private val HandSizeLimit = 6

  def mainLoop() = {
    implicit val gameState = this

    players foreach { player =>
      // TODO low: common logic that should be refactored for reuse
      InitialHandSize times {
        player.draw()
      }
    }

    playersCycle foreach { case (turnPlayer, opponent) =>
      implicit val implicitTurnPlayer = turnPlayer

      turnCount += 1
      hasNormalSummonedThisTurn = false

      implicit var phase: Phase = DrawPhase
      while (phase != EndPhase) {
        phase match {
          case DrawPhase =>
            if (turnCount > 1) {
              // TODO: this will have to emit an event in some way
              turnPlayer.draw()
            }
          case StandbyPhase =>
          case MainPhase =>
          case BattlePhase =>
          case MainPhase2 =>
          case EndPhase =>
            while (turnPlayer.hand.size > HandSizeLimit) {
              val action = turnPlayer.chooseAction(Seq(new DiscardImpl))
              action.execute()
            }
        }

        val action = turnPlayer.chooseAction(Seq(new PassPriorityImpl))
        action.execute()

        phase = phase.next
      }
    }
  }

  /**
    * An infinite cycle of [(player1, player2), (player2, player1), ...]
    *
    * TODO low: apparently an iterator would be more efficient (good habit here)
    */
  private def playersCycle: Stream[(Player, Player)] = {
    val (player1, player2) = players match {
      case Seq(p1, p2) => (p1, p2)
    }

    Stream.continually(Seq((player1, player2), (player2, player1)).toStream).flatten
  }
}
