package yugioh

import yugioh.action.DefaultActionModuleComponent
import yugioh.events.DefaultEventsModuleComponent

object Main extends DefaultPlayGameComponent
    with DefaultEventsModuleComponent
    with DefaultPhaseModuleComponent
    with DefaultFieldModuleComponent
    with DefaultBattlePhaseModuleComponent
    with DefaultActionModuleComponent {

  val player1 = new CommandLineHumanPlayer("Human")(eventsModule, fieldModule)
  val player2 = new PassivePlayer()(fieldModule)

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
