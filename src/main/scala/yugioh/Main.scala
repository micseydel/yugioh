package yugioh

object Main {
  def main(args: Array[String]): Unit = {
    val players: Seq[Player] = Seq(new CommandLineHumanPlayer("Human"), new PassivePlayer)
    players.head.opponent = players.last
    players.last.opponent = players.head

    val gameState = new GameStateImpl(players)
    gameState.mainLoop()
  }
}
