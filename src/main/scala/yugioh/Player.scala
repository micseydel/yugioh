package yugioh

import yugioh.action.Action
import yugioh.card.Card
import yugioh.card.monster._
import yugioh.events.{EventsComponent, PhaseStartEvent, TurnStartEvent}

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

trait Player {
  val name: String
  val deck: Deck
  val extraDeck: ListBuffer[ExtraDeckMonster] = new ListBuffer[ExtraDeckMonster]
  val field: Field = new FieldImpl
  val hand: ListBuffer[Card] = new ListBuffer[Card]

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

  def draw(): Unit = draw(1)
  def draw(howMany: Int): Unit = {
    val toAdd = deck.fromTop(howMany)
    for (card <- toAdd) {
      card.location = InHand
    }

    hand ++= toAdd
  }

  def cardToDiscardForHandSizeLimit(implicit gameState: GameState): Seq[Card]

  /**
    * Ask player if they wish to end the phase or step.
    */
  def consentToEnd(implicit gameState: GameState): Boolean

  /**
    * Ask a player what summon material (tribute, synchro, etc.) they wish to use for toSummon.
    */
  def selectSummonMaterial(toSummon: Monster, summonCriteria: SummonCriteria, possibleMaterials: Seq[Monster])
                          (implicit gameState: GameState): Seq[Monster]

  def selectAttackTarget(attacker: Monster, potentialTargets: Seq[Monster])(implicit gameState: GameState): Monster

  /**
    * When an effect needs to select a target, this is how an activation asks a player what targets to use.
    */
  def selectEffectTargets[C <: Card](targets: Seq[C], criteria: Criteria[C])(implicit gameState: GameState): Seq[C]

  /**
    * For a monster in the process of being special summoned, select from positions options which position to SS in.
    */
  def selectSpecialSummonPosition(monster: Monster, positions: Seq[Position])(implicit gameState: GameState): Position

  override def toString = name
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

class CommandLineHumanPlayer(val name: String) extends Player {
  Me: EventsComponent =>

  override val deck: Deck = new TestDeck(this) // TODO: be more than just a stub

  events.observe { event =>
    event match {
      case TurnStartEvent(turnPlayers, mutableGameState) =>
        println(s"\nTurn #${mutableGameState.turnCount}")
        println("================================")
        showField(turnPlayers, mutableGameState)
      case phaseStart: PhaseStartEvent =>
        println(s"Entering ${phaseStart.phase}")
      case ignore =>
    }
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

  override def cardToDiscardForHandSizeLimit(implicit gameState: GameState): Seq[Card] = {
    val criteria = CountCriteria(hand.size - Constants.HandSizeLimit)
    selectMultiple(s"Must discard for hand size limit ($criteria):", hand, CountCriteria(hand.size - Constants.HandSizeLimit))
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
  private def selectMultiple[A](prompt: String, choices: Seq[A], criteria: Criteria[A])
                               (implicit gameState: GameState): Seq[A] = {
    println(prompt)

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

  override def selectSummonMaterial(toSummon: Monster, summonCriteria: SummonCriteria, possibleMaterials: Seq[Monster])(implicit gameState: GameState) = {
    summonCriteria match {
      case TributeSummonCriteria(count) if possibleMaterials.size == count =>
        println("Singular choice, automatically tributing " + possibleMaterials)
        possibleMaterials
      case _ =>
        selectMultiple(s"To summon $toSummon, please enter comma separated monster(s) to use ($summonCriteria).", possibleMaterials, summonCriteria)
    }
  }

  override def selectAttackTarget(attacker: Monster, potentialTargets: Seq[Monster])(implicit gameState: GameState): Monster = {
    select(s"Select target for $attacker", potentialTargets)
  }

  override def selectEffectTargets[C <: Card](targets: Seq[C], criteria: Criteria[C])(implicit gameState: GameState) = {
    selectMultiple(s"Select targets for effect.", targets, criteria)
  }

  /**
    * For a monster in the process of being special summoned, select from positions options which position to SS in.
    */
  override def selectSpecialSummonPosition(monster: Monster, positions: Seq[Position])(implicit gameState: GameState) = {
    select(s"Select position to special summon $monster.", positions)
  }
}

/**
  * Useful for testing purposes.
  *
  * When given an option like discarding for hand size or choosing an action, will just select the first.
  */
class PassivePlayer extends Player {
  override val name = "PassivePlayer"
  override val deck = new TestDeck(this)
  override def cardToDiscardForHandSizeLimit(implicit gameState: GameState) = Seq(hand.head)
  override def chooseAction(actions: Seq[Action])(implicit gameState: GameState) = actions.head
  override def consentToEnd(implicit gameState: GameState) = true
  override def enterBattlePhase(implicit gameState: GameState) = false
  override def selectSummonMaterial(toSummon: Monster, summonCriteria: SummonCriteria, possibleMaterials: Seq[Monster])(implicit gameState: GameState) = ???
  override def selectAttackTarget(attacker: Monster, potentialTargets: Seq[Monster])(implicit gameState: GameState) = ???
  override def selectEffectTargets[C <: Card](targets: Seq[C], criteria: Criteria[C])(implicit gameState: GameState) = ???
  override def selectSpecialSummonPosition(monster: Monster, positions: Seq[Position])(implicit gameState: GameState) = ???
}
