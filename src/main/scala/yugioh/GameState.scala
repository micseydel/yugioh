package yugioh

import java.util.logging.Logger

import yugioh.Util.intWithTimes
import yugioh.action.{Action, DiscardImpl, PassPriorityImpl}

trait GameState {
  val players: Seq[Player]

  var turnCount = 0
  var hasNormalSummonedThisTurn: Boolean = false

  def openGameState: Boolean

  def mainLoop(): Unit
}

class GameStateImpl(val players: Seq[Player]) extends GameState {
  private val InitialHandSize = 5
  private val HandSizeLimit = 6

  private var history: List[Action] = Nil

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

      actionablePhases(turnPlayer) foreach { phase =>
        implicit val implicitPhase = phase

        phase match {
          case DrawPhase =>
            if (turnCount > 1) {
              turnPlayer.draw()
            }
          case StandbyPhase =>
            val action = turnPlayer.chooseAction(Seq(new PassPriorityImpl))
            action.execute()
          case MainPhase =>
            val action = turnPlayer.chooseAction(Seq(new PassPriorityImpl))
            action.execute()
          case BattlePhase =>
            val action = turnPlayer.chooseAction(Seq(new PassPriorityImpl))
            action.execute()
          case MainPhase2 =>
            val action = turnPlayer.chooseAction(Seq(new PassPriorityImpl))
            action.execute()
          case EndPhase =>
            while (turnPlayer.hand.size > HandSizeLimit) {
              val action = turnPlayer.chooseAction(Seq(new DiscardImpl))
              action.execute()
              history :+= action
            }
            val action = turnPlayer.chooseAction(Seq(new PassPriorityImpl))
            action.execute()
        }
      }
    }
  }

  /**
    * First turn this should
    */
  private def actionablePhases(implicit turnPlayer: Player): Iterable[Phase] = {
    if (turnCount > 1) {
      Phase.phases // TODO low: need magic like in Python
    } else {
      Seq(DrawPhase, StandbyPhase, MainPhase, EndPhase)
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

  override def openGameState: Boolean = {
    false // TODO
  }
}
