package yugioh

import yugioh.action.monster.{DeclareAttack, NormalSummon, SetAsMonster}
import yugioh.events.{BattleDamage, EventsModuleComponent, TurnEndEvent, TurnStartEvent}

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
    with PhaseModuleComponent =>

  override def playGame: PlayGame = new PlayGame {
    override val mutableGameState = new MutableGameState

    /**
      * A GameLossException will be thrown to end the game.
      */
    def mainLoop() = {
      setupObservables()

      // before turns start, each player draws
      for (player <- IterablePlayers) {
        player.draw(Constants.InitialHandSize)
      }

      // loop until a game loss exception is thrown
      for (turnPlayers <- playersCycle) {
        takeTurn(turnPlayers)
      }
    }

    private def takeTurn(turnPlayers: TurnPlayers) = {
      mutableGameState.turnCount += 1

      events.emit(TurnStartEvent(turnPlayers, mutableGameState))
      phaseModule.loop(GameState(mutableGameState, turnPlayers))
      events.emit(TurnEndEvent)
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
      events.observe { event =>
        event match {
          case TurnStartEvent(_, _) =>
            mutableGameState.hasNormalSummonedThisTurn = false
          case ignore =>
        }
      }

      // listen for a normal summon or set, flag that it happened
      events.observe { event =>
        event match {
          case _:NormalSummon | _:SetAsMonster =>
            mutableGameState.hasNormalSummonedThisTurn = true
          case ignore =>
        }
      }

      // listen for an attack declaration, tag that monster as having attacked
      events.observe { event =>
        event match {
          case attackDeclaration: DeclareAttack =>
            for (monsterControlledState <- attackDeclaration.attacker.maybeMonsterControlledState) {
              monsterControlledState.attackedThisTurn = true
            }
          case ignore =>
        }
      }

      // listen for and handle life point damage
      events.observe { event =>
        event match {
          case BattleDamage(player, damage) =>
            player.lifePoints -= damage
            if (player.lifePoints <= 0) {
              throw OutOfLifepoints(player)
            }
          case ignore =>
        }
      }
    }
  }
}
