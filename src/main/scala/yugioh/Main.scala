package yugioh

import yugioh.events.DefaultEventsComponent

object Main extends DefaultPlayGameComponent
    with DefaultEventsComponent {
  val player1 = new CommandLineHumanPlayer("Human") with DefaultEventsComponent
  val player2 = new PassivePlayer

  override val Players = (player1, player2)

  def main(args: Array[String]): Unit = {
    try {
      playGame.mainLoop()
    } catch {
      case gameLoss: GameLoss =>
        println(s"${gameLoss.loser} has lost! " + gameLoss)
    }
  }
}
