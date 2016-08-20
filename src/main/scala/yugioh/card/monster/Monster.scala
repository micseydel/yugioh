package yugioh.card.monster

import yugioh._
import yugioh.action.ActionModule
import yugioh.card._
import yugioh.card.state.{MonsterControlledState, MonsterFieldState}
import yugioh.events.EventsModule

trait Monster extends Card {
  val PrintedAttack: Int
  val PrintedDefense: Int
  val MaybePrintedLevel: Option[Int]
  val MaybePrintedRank: Option[Int] = None
  val PrintedAttribute: Attribute
  val PrintedType: Type

  def attack: Int = PrintedAttack
  def defense: Int = PrintedDefense
  def maybeLevel: Option[Int] = MaybePrintedLevel
  def maybeRank: Option[Int] = MaybePrintedRank
  def attribute: Attribute = PrintedAttribute
  def monsterType: Type = PrintedType

  var maybeMonsterFieldState: Option[MonsterFieldState] = None

  def maybeMonsterControlledState = maybeControlledState.asInstanceOf[Option[MonsterControlledState]]

  /**
    * Default implementation of being able to normal/tribute summon during main phases, does not apply to (Semi-)Nomi.
    */
  override def actions(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    gameState match {
      case GameState(MutableGameState(_, hasNormalSummonedThisTurn, _), _, OpenGameState, phase, _, _) =>
        phase match {
          case MainPhase | MainPhase2 =>
            mainPhaseActions(hasNormalSummonedThisTurn)
          case BattlePhase =>
            battlePhaseActions
          case _ =>
            Seq()
        }
      case _ => Seq()
    }
  }

  private def mainPhaseActions(hasNormalSummonedThisTurn: Boolean)
                              (implicit eventsModule: EventsModule, actionModule: ActionModule) = {
    location match {
      case InHand if !hasNormalSummonedThisTurn =>
        maybeLevel.map { level =>
          if (level <= 4) {
            if (Owner.field.hasFreeMonsterZone) {
              Seq(actionModule.newNormalSummon(this), actionModule.newSetAsMonster(this))
            } else {
              Seq()
            }
          } else {
            val controlledMonsters = Owner.field.monsterZones.count(_.isDefined)
            val canTribute = if (level <= 6) {
              controlledMonsters >= 1
            } else {
              controlledMonsters >= 2
            }

            if (canTribute) {
              Seq(actionModule.newTributeSummon(this), actionModule.newTributeSet(this))
            } else {
              Seq()
            }
          }
        }.getOrElse(Seq())
      case monsterZone: InMonsterZone if canSwitchPositions =>
        if (maybeControlledState.get.faceup) {
          Seq(actionModule.newSwitchPosition(this))
        } else {
          Seq(actionModule.newFlipSummon(this))
        }
      case _ =>
        Seq()
    }
  }

  private def canSwitchPositions(implicit eventsModule: EventsModule): Boolean = {
    maybeMonsterControlledState.exists { monsterControlledState =>
      !monsterControlledState.manuallyChangedPositionsThisTurn && !monsterControlledState.attackedThisTurn
    }
  }

  private def battlePhaseActions(implicit gameState: GameState, eventsModule: EventsModule, actionModule: ActionModule) = {
    val Controller = controller // for pattern matching
    gameState match {
      case GameState(_, TurnPlayers(Controller, opponent), OpenGameState, _, BattleStep, _) =>
        maybeMonsterControlledState.map { controlledState =>
          if (!controlledState.attackedThisTurn && controlledState.position == Attack) {
            val potentialTargets = opponent.field.monsterZones.toSeq.flatten
            if (potentialTargets.nonEmpty) {
              Seq(actionModule.newDeclareAttackOnMonster(this))
            } else {
              Seq(actionModule.newDeclareDirectAttack(this))
            }
          } else {
            Seq()
          }
        }.getOrElse(Seq())
      case _ => Seq()
    }
  }
}

trait NormalMonster extends Monster

trait EffectMonster extends Monster with EffectCard

trait FlipMonster extends EffectMonster

// TODO LOW: is there an is-A relationship between Nomi and Semi-Nomi?

/**
  * Cannot be Normal Summoned, Set or Special Summoned, except by fulfilling a special requirement.
  * e.g. Sephylon, the Ultimate Timelord & ritual monsters
  */
trait Nomi extends EffectMonster // TODO LOW

/**
  * "Cannot be Normal Summoned/Set. Must first be Special Summoned..." (or an older variant).
  *   e.g. BLS
  */
trait SemiNomi extends EffectMonster // TODO LOW

trait RitualMonster extends SemiNomi

trait ExtraDeckMonster extends SemiNomi

trait FusionMonster extends ExtraDeckMonster
trait SynchroMonster extends ExtraDeckMonster

trait XyzMonster extends ExtraDeckMonster {
  override val MaybePrintedLevel = None
  override val MaybePrintedRank = ??? // TODO LOW: nicer way to be convenient elsewhere but force rank definition here?
}

trait PendulumMonster extends Monster
