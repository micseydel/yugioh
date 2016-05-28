package yugioh

object Main {
  def main(args: Array[String]): Unit = {
    val player1 = new CommandLineHumanPlayer("Human")
    val player2 = new PassivePlayer

    val gameState = new GameStateImpl(Seq(player1, player2))
    gameState.mainLoop()
  }
}
