package yugioh

import yugioh.action.DefaultActionModuleComponent
import yugioh.events.DefaultEventsModuleComponent

object Main extends DefaultPlayGameComponent
    with DefaultEventsModuleComponent
    with DefaultPhaseModuleComponent
    with DefaultFieldModuleComponent
    with DefaultBattlePhaseModuleComponent
    with DefaultActionModuleComponent
    with CommandLineHumanPlayerModuleComponent
    with PassivePlayerModuleComponent {

  val player1: Player = newCommandLineHumanPlayer("Human")
  val player2: Player = newPassivePlayer

  override val Players: (Player, Player) = (player1, player2)

  def main(args: Array[String]): Unit = {
    try {
      playGame.mainLoop()
    } catch {
      case gameLoss: GameLoss =>
        println(s"${gameLoss.loser} has lost! " + gameLoss)
    }
  }
}
