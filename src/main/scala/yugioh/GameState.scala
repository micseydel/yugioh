package yugioh

import java.util.logging.Logger

import yugioh.Util.intWithTimes

trait GameState {
  val players: Seq[Player]

  var turnCount = 0
  var hasNormalSummonedThisTurn: Boolean

  def currentPhase: Phase
  def openGameState: Boolean
  def battlePhaseStep: Option[BattlePhaseStep]
  def damageStepSubStep: Option[DamageStepSubStep]

  def mainLoop(): Unit
}

// TODO should be an `object` or `class` rather than `abstract class`
abstract class GameStateImpl(val players: Seq[Player]) extends GameState {
  private val InitialHandSize = 5

  def mainLoop() = {
    players foreach { player =>
      // TODO low: common logic that should be refactored for reuse
      InitialHandSize times {
        player.draw()
      }
    }

    playersCycle foreach { case (turnPlayer, opponent) =>
      turnCount += 1
      hasNormalSummonedThisTurn = false

      actionablePhases(turnPlayer) foreach {
        case DrawPhase =>
          if (turnCount > 1) {
            turnPlayer.draw()
          }
        case StandbyPhase =>
        case MainPhase =>
        case BattlePhase if turnCount > 1 =>
        case MainPhase2 if turnCount > 1 =>
        case EndPhase =>
          // TODO low: ideally would remove this blanket case and the turnCount checks above
        case _ => Logger.getLogger("GameState").info(s"Skipped phase $currentPhase (turncount $turnCount)")
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
}
