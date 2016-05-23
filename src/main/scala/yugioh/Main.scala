package yugioh

object Main {
  def main(args: Array[String]): Unit = {
    val player1 = new CommandLineHumanPlayer("Human")
    val player2 = new PassivePlayer

    player1.opponent = player2
    player2.opponent = player1

    val gameState = new GameStateImpl(Seq(player1, player2))
    gameState.mainLoop()
  }
}
