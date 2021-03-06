package yugioh

import yugioh.action.ActionModuleComponent
import yugioh.action.monster.{DeclareAttack, NormalSummon, SetAsMonster}
import yugioh.events._

trait PlayGameComponent {
  val Players: (Player, Player)

  trait PlayGame {
    val IterablePlayers = Seq(Players._1,Players._2)

    val mutableGameState: MutableGameState

    def mainLoop(): Unit
  }

  def playGame: PlayGame
}

trait DefaultPlayGameComponent extends PlayGameComponent {
  self: EventsModuleComponent
    with PhaseModuleComponent
    with ActionModuleComponent =>

  override def playGame: PlayGame = new PlayGame {
    override val mutableGameState = new MutableGameState

    /**
      * A GameLossException will be thrown to end the game.
      */
    def mainLoop(): Unit = {
      setupObservables()

      // before turns start, each player draws
      for (player <- IterablePlayers) {
        implicit val gameState: GameState = GameState(mutableGameState, null)
        player.draw(Constants.InitialHandSize)
      }

      // loop until a game loss exception is thrown
      for (turnPlayers <- playersCycle) {
        takeTurn(turnPlayers)
      }
    }

    private def takeTurn(turnPlayers: TurnPlayers) = {
      mutableGameState.turnCount += 1

      eventsModule.emit(TurnStartEvent(turnPlayers, mutableGameState))
      phaseModule.loop(GameState(mutableGameState, turnPlayers), actionModule)
      eventsModule.emit(TurnEndEvent)
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
      eventsModule.observe {
        case TurnStartEvent(_, _) =>
          mutableGameState.hasNormalSummonedThisTurn = false
      }

      // listen for a normal summon or set, flag that it happened
      eventsModule.observe {
        case ActionEvent(_: NormalSummon | _: SetAsMonster) =>
          mutableGameState.hasNormalSummonedThisTurn = true
      }

      // listen for an attack declaration, tag that monster as having attacked
      eventsModule.observe {
        case ActionEvent(attackDeclaration: DeclareAttack) =>
          // we use get here because it would be a bug for an attacker to not have controlled state
          attackDeclaration.attacker.maybeControlledState.get.attackedThisTurn = true
      }
    }
  }
}
