package yugioh.card.monster

import yugioh._
import yugioh.action.Action
import yugioh.action.monster._
import yugioh.card.Card
import yugioh.card.state.{MonsterControlledState, MonsterFieldState}

trait Monster extends Card {
  val printedAttack: Int
  val printedDefense: Int
  val maybePrintedLevel: Option[Int]
  val maybePrintedRank: Option[Int] = None
  val printedAttribute: Attribute
  val printedType: Type

  def attack: Int = printedAttack
  def defense: Int = printedDefense
  def maybeLevel: Option[Int] = maybePrintedLevel
  def maybeRank: Option[Int] = maybePrintedRank
  def attribute: Attribute = printedAttribute
  def monsterType: Type = printedType

  var maybeMonsterFieldState: Option[MonsterFieldState] = None

  def maybeMonsterControlledState: Option[MonsterControlledState] = maybeControlledState.asInstanceOf[Option[MonsterControlledState]]

  /**
    * Default implementation of being able to normal/tribute summon during main phases, does not apply to (Semi-)Nomi.
    */
  override def actions(implicit gameState: GameState) = {
    gameState match {
      case GameState(MutableGameState(_, hasNormalSummonedThisTurn, _), turnPlayers, fastEffectTiming, phase, step, _) =>
        fastEffectTiming match {
          case OpenGameState =>
            phase match {
              case MainPhase | MainPhase2 =>
                mainPhaseActions(hasNormalSummonedThisTurn)
              case BattlePhase =>
                battlePhaseActions(turnPlayers.opponent.field.monsterZones.toSeq.flatten)
              case _ =>
                Seq()
            }
          case _ => Seq()
        }
    }
  }

  private def mainPhaseActions(hasNormalSummonedThisTurn: Boolean): Seq[Action] = {
    location match {
      case InHand if !hasNormalSummonedThisTurn =>
        maybeLevel.map { level =>
          if (level <= 4) {
            Seq(new NormalSummonImpl(this), new SetAsMonsterImpl(this))
          } else {
            val controlledMonsters = owner.field.monsterZones.count(_.isDefined)
            val canTribute = if (level <= 6) {
              controlledMonsters >= 1
            } else {
              controlledMonsters >= 2
            }

            if (canTribute) {
              Seq(new TributeSummonImpl(this), new TributeSetImpl(this))
            } else {
              Seq()
            }
          }
        }.getOrElse(Seq())
      case monsterZone: InMonsterZone if !maybeMonsterControlledState.get.manuallyChangedPositionsThisTurn =>
        if (maybeControlledState.get.faceup) {
          Seq(new SwitchPositionImpl(this))
        } else {
          Seq(new FlipSummonImpl(this))
        }
      case _ =>
        Seq()
    }
  }

  private def battlePhaseActions(potentialTargets: Seq[Monster]): Seq[Action] = {
    maybeMonsterControlledState.map { controlledState =>
      if (!controlledState.attackedThisTurn && controlledState.position == Attack) {
        if (potentialTargets.nonEmpty) {
          Seq(new DeclareAttackOnMonster(this))
        } else {
          Seq(new DeclareDirectAttack(this))
        }
      } else {
        Seq()
      }
    }.getOrElse(Seq())
  }
}

trait NormalMonster extends Monster

trait EffectMonster extends Monster
trait FlipMonster extends EffectMonster

// TODO LOW: is there an is-A relationship between Nomi and Semi-Nomi?

/**
  * Cannot be Normal Summoned, Set or Special Summoned, except by fulfilling a special requirement.
  * e.g. Sephylon, the Ultimate Timelord & ritual monsters
  */
trait Nomi extends EffectMonster // TODO

/**
  * "Cannot be Normal Summoned/Set. Must first be Special Summoned..." (or an older variant).
  *   e.g. BLS
  */
trait SemiNomi extends EffectMonster // TODO

trait RitualMonster extends SemiNomi

trait ExtraDeckMonster extends SemiNomi

trait FusionMonster extends ExtraDeckMonster
trait SynchroMonster extends ExtraDeckMonster

trait XyzMonster extends ExtraDeckMonster {
  override val maybePrintedLevel = None
  override val maybePrintedRank = ??? // TODO: nicer way to be convenient elsewhere but force rank definition here?
}

trait PendulumMonster extends Monster
