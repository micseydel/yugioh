package yugioh

import yugioh.action.monster.NormalSummon
import yugioh.action.{Action, ActionModule, Cause}
import yugioh.card.Card.AnyCard
import yugioh.card.monster._
import yugioh.events.{EventsModule, EventsModuleComponent, PhaseStartEvent, TurnStartEvent}

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

trait Player extends Cause {
  val name: String
  val deck: Deck
  val extraDeck: ListBuffer[ExtraDeckMonster] = new ListBuffer[ExtraDeckMonster]
  val hand = new ListBuffer[AnyCard]

  val field: Field

  var lifePoints: Int = Constants.InitialLifePoints

  /**
    * @param actions must contain at least one thing, typically an option to pass
    */
  def chooseAction(actions: Seq[Action])(implicit gameState: GameState): Action

  /**
    * Called when MP1 is ended.
    *
    * @return true to enter BP, otherwise go to EP
    */
  def enterBattlePhase(implicit gameState: GameState): Boolean

  def draw()(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = draw(1)
  def draw(howMany: Int)(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule): Unit = {
    val toAdd = deck.fromTop(howMany)
    for (card <- toAdd) {
      card.location = InHand
      card.notifyMoved()
    }

    hand ++= toAdd
  }

  def cardToDiscardForHandSizeLimit(implicit gameState: GameState): Seq[AnyCard]

  /**
    * Ask player if they wish to end the phase or step.
    */
  def consentToEnd(implicit gameState: GameState): Boolean

  /**
    * Ask a player what summon material (tribute, synchro, etc.) they wish to use for toSummon.
    */
  def selectSummonMaterial(summonCriteria: SummonCriteria)
                          (implicit gameState: GameState): Seq[Monster]

  def selectAttackTarget(attacker: Monster, potentialTargets: Seq[Monster])(implicit gameState: GameState): Monster

  /**
    * When an effect needs to select a target, this is how an activation asks a player what targets to use.
    */
  def selectEffectTargets[C <: AnyCard](criteria: Criteria[C])(implicit gameState: GameState): Seq[C]

  /**
    * For a monster in the process of being special summoned, select from positions options which position to SS in.
    */
  def selectSpecialSummonPosition(monster: Monster, positions: Seq[Position])(implicit gameState: GameState): Position

  override def toString = name

  def opponent(implicit gameState: GameState) = {
    gameState.turnPlayers.other(this)
  }
}

case class TurnPlayers(turnPlayer: Player, opponent: Player) {
  def other(player: Player): Player = {
    if (player == turnPlayer) {
      opponent
    } else if (player == opponent) {
      turnPlayer
    } else {
      throw new IllegalArgumentException("Must be called with a turn player.")
    }
  }

  def both = Seq(turnPlayer, opponent)
}

trait CommandLineHumanPlayerModuleComponent {
  self: EventsModuleComponent
    with FieldModuleComponent =>

  def newCommandLineHumanPlayer(playerName: String) = new Player {
    Me =>

    val name = playerName

    override val field = fieldModule.createField

    override val deck: Deck = new TestDeck(this) // TODO: be more than just a stub
    eventsModule.observe {
      case TurnStartEvent(turnPlayers, mutableGameState) =>
        println(s"\nTurn #${mutableGameState.turnCount}")
        println("================================")
        showField(turnPlayers, mutableGameState)
      case phaseStart: PhaseStartEvent =>
        println(s"Entering ${phaseStart.phase}")
      case ignore =>
    }

    private def showField(implicit turnPlayers: TurnPlayers, mutableGameState: MutableGameState) = {
      // TODO LOW: this should happen after any change to the board, and should include field zone and pendulums

      println(s"\nOpponent ${turnPlayers.opponent} (${turnPlayers.opponent.lifePoints})")
      print(s"Deck (${turnPlayers.opponent.deck.remaining}) | ")
      print(s"Hand (${turnPlayers.opponent.hand.size}) | ")
      print(s"Grave (${turnPlayers.opponent.field.graveyard.size}) | ")
      print(s"Banished (${turnPlayers.opponent.field.banished.size}) | ")
      println(s"Extra Deck (${turnPlayers.opponent.extraDeck.size})")

      for (cards <- Seq(turnPlayers.opponent.field.spellTrapZones, turnPlayers.opponent.field.monsterZones)) {
        println(cards.map(_.map(_.toString(this)).getOrElse("Empty")).mkString(" | "))
      }

      println("                 ---")

      for (cards <- Seq(turnPlayers.turnPlayer.field.monsterZones, turnPlayers.turnPlayer.field.spellTrapZones)) {
        println(cards.map(_.map(_.toString(this)).getOrElse("Empty")).mkString(" | "))
      }

      print(s"Hand (${turnPlayers.turnPlayer.hand.size}): ")
      println(turnPlayers.turnPlayer.hand.map(_.name).mkString(" | "))

      print(s"Deck (${turnPlayers.turnPlayer.deck.remaining}) | ")
      print(s"Grave (${turnPlayers.turnPlayer.field.graveyard.size}) | ")
      print(s"Banished (${turnPlayers.turnPlayer.field.banished.size}) | ")
      println(s"Extra Deck (${turnPlayers.turnPlayer.extraDeck.size})")
      println(s"Turn player ${turnPlayers.turnPlayer} (${turnPlayers.turnPlayer.lifePoints})\n")
    }

    override def chooseAction(actions: Seq[Action])(implicit gameState: GameState) = {
      gameState match {
        case GameState(_, _, fastEffectTiming, phase, step, _) =>
          if (actions.size == 1) {
            val action = actions.head
            println(s"Action $action was only option ($fastEffectTiming, $phase${Option(step).map(", " + _).getOrElse("")}), taking implicitly.")
            action
          } else {
            select("Select an action:", actions)
          }
      }
    }

    override def cardToDiscardForHandSizeLimit(implicit gameState: GameState): Seq[AnyCard] = {
      val criteria = new Criteria[AnyCard] {
        override def meetable(implicit gameState: GameState) = true
        override def validSelection[T >: AnyCard](choices: Seq[T])(implicit gameState: GameState) = choices.size == hand.size - Constants.HandSizeLimit
        override def availableChoices(implicit gameState: GameState) = hand
      }

      selectMultiple(s"Must discard for hand size limit ($criteria):", criteria)
    }

    override def enterBattlePhase(implicit gameState: GameState) = {
      print("MP1 is ending; enter BP? (If not, will go to EP) ")
      StdIn.readBoolean()
    }

    /**
      * Ask the user for a specific element of a sequence.
      */
    private def select[A](prompt: String, choices: Seq[A])
      (implicit gameState: GameState): A = {
      println(prompt + s" (${gameState.fastEffectTiming}, ${gameState.phase}${Option(gameState.step).map(", " + _).getOrElse("")})")

      for ((action, i) <- choices.zipWithIndex) {
        println(s"($i) $action")
      }

      print("> ")
      var choice = StdIn.readInt()
      while (choice < 0 || choice >= choices.size) {
        println("Not a valid option.")
        print("> ")
        choice = StdIn.readInt()
      }

      choices(choice)
    }

    /**
      * Get some of the values from the choices.
      */
    private def selectMultiple[A](prompt: String, criteria: Criteria[A])
      (implicit gameState: GameState): Seq[A] = {
      println(prompt)

      val choices = criteria.availableChoices

      for ((choice, i) <- choices.zipWithIndex) {
        println(s"($i) $choice")
      }

      var selection = StdIn.readLine("> ").split(",").map(_.toInt).map(choices(_))
      while (!criteria.validSelection(selection)) {
        println(s"Selection was not valid, criteria is: $criteria.")
        selection = StdIn.readLine("> ").split(",").map(_.toInt).map(choices(_))
      }
      selection
    }

    /**
      * After SP, will ask via StdIn, otherwise just consents.
      */
    override def consentToEnd(implicit gameState: GameState): Boolean = {
      gameState match {
        case GameState(_, TurnPlayers(Me, _), _, phase@(MainPhase | BattlePhase | MainPhase2 | EndPhase), step, _) if !step.isInstanceOf[DamageStepSubStep] && !step.isInstanceOf[BattleStepWithPendingAttack] =>
          print(s"End ${Option(step).getOrElse(phase)}? ")
          StdIn.readBoolean()
        case _ => true
      }
    }

    override def selectSummonMaterial(summonCriteria: SummonCriteria)(implicit gameState: GameState) = {
      // TODO: streamlined logic for when there is only a single possibility
      selectMultiple(s"To summon ${summonCriteria.monster}, please enter comma separated monster(s) to use ($summonCriteria).", summonCriteria)
    }

    override def selectAttackTarget(attacker: Monster, potentialTargets: Seq[Monster])(implicit gameState: GameState): Monster = {
      select(s"Select target for $attacker", potentialTargets)
    }

    override def selectEffectTargets[C <: AnyCard](criteria: Criteria[C])(implicit gameState: GameState) = {
      selectMultiple(s"Select targets for effect.", criteria)
    }

    /**
      * For a monster in the process of being special summoned, select from positions options which position to SS in.
      */
    override def selectSpecialSummonPosition(monster: Monster, positions: Seq[Position])(implicit gameState: GameState) = {
      select(s"Select position to special summon $monster.", positions)
    }
  }
}

/**
  * Useful for testing purposes.
  *
  * When given an option like discarding for hand size or choosing an action, will just select the first.
  */
trait PassivePlayerModuleComponent {
  self: FieldModuleComponent =>

  //noinspection NotImplementedCode - explicitly ignoring the ??? here since we don't want to bother with it
  def newPassivePlayer = new Player {
    override val field = fieldModule.createField
    override val name = "PassivePlayer"
    override val deck = new TestDeck(this)
    override def cardToDiscardForHandSizeLimit(implicit gameState: GameState): Seq[AnyCard] = Seq(hand.head)
    override def consentToEnd(implicit gameState: GameState) = true
    override def enterBattlePhase(implicit gameState: GameState) = false
    override def selectSummonMaterial(summonCriteria: SummonCriteria)(implicit gameState: GameState) = ???
    override def selectAttackTarget(attacker: Monster, potentialTargets: Seq[Monster])(implicit gameState: GameState) = ???
    override def selectEffectTargets[C <: AnyCard](criteria: Criteria[C])(implicit gameState: GameState) = ???
    override def selectSpecialSummonPosition(monster: Monster, positions: Seq[Position])(implicit gameState: GameState) = ???

    override def chooseAction(actions: Seq[Action])(implicit gameState: GameState) = {
      val action = actions.find(_.isInstanceOf[NormalSummon]).getOrElse(actions.head)

      if (gameState.turnPlayers.turnPlayer == this) {
        println(this + " going to take action " + action)
      }

      action
    }
  }
}
