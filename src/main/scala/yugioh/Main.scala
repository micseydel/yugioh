package yugioh

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")

    val players: Seq[Player] = Seq(new CommandLineHumanPlayer("Human"), new PassivePlayer)

    val gameState = new GameStateImpl(players)
    gameState.mainLoop()
  }
}
