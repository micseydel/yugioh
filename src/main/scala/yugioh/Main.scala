package yugioh

import yugioh.events.DefaultEventsComponent

object Main {
  def main(args: Array[String]): Unit = {
    val player1 = new CommandLineHumanPlayer("Human") with DefaultEventsComponent
    val player2 = new PassivePlayer

    val gameState = new PlayGameImpl((player1, player2)) with DefaultEventsComponent
    gameState.mainLoop()
  }
}
