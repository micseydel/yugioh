package yugioh.action

import yugioh.action.monster.{FlipSummon, SpecialSummon, _}
import yugioh.card.Card.AnyCard
import yugioh.card.monster.{Monster, Position}
import yugioh.card.SpellOrTrap
import yugioh.events.EventsModule
import yugioh.{GameState, Player}

trait ActionModule {
  def newSetAsSpellOrTrap(cause: Cause, spellOrTrap: SpellOrTrap): SetAsSpellOrTrap

  def newNormalSummon(cause: Cause, monster: Monster): NormalSummon

  def newSetAsMonster(cause: Cause, monster: Monster): SetAsMonster

  def newTributeSummon(cause: Cause, monster: Monster): TributeSummon

  def newTributeSet(cause: Cause, monster: Monster): TributeSet

  def newSwitchPosition(cause: Cause, monster: Monster)(implicit eventsModule: EventsModule): SwitchPosition

  def newFlipSummon(cause: Cause, monster: Monster)(implicit eventsModule: EventsModule): FlipSummon

  def newDiscard(cause: Cause, hand: Seq[AnyCard]): Discard

  def newDraw(cause: Cause, player: Player, howMany: Int): Draw

  def newDrawForTurn(implicit gameState: GameState): DrawForTurn

  def newDestroy(cause: Cause, card: AnyCard): Destroy = newDestroy(cause, Seq(card))

  def newDestroy(cause: Cause, cards: Seq[AnyCard]): Destroy

  def newSpecialSummon(cause: Cause, controller: Player, monster: Monster, position: Position): SpecialSummon

  def newDiscardForHandSizeLimit()(implicit gameState: GameState): DiscardForHandSizeLimit

  def newDeclareDirectAttack(cause: Cause, monster: Monster): DeclareDirectAttack

  def newDeclareAttackOnMonster(cause: Cause, attacker: Monster, target: Monster): DeclareAttackOnMonster

  def newChangeLifePoints(cause: Cause, lifePointsChange: Int, player: Player): ChangeLifePoints
}

trait ActionModuleComponent {
  implicit def actionModule: ActionModule
}

trait DefaultActionModuleComponent extends ActionModuleComponent {
  def actionModule: ActionModule = new ActionModule {

    override def newSetAsSpellOrTrap(cause: Cause, spellOrTrap: SpellOrTrap): SetAsSpellOrTrap = {
      new SetAsSpellOrTrapImpl(cause, spellOrTrap)
    }

    override def newDestroy(cause: Cause, cards: Seq[AnyCard]): Destroy = {
      DestroyImpl(cause, cards)
    }

    override def newDiscard(cause: Cause, hand: Seq[AnyCard]): Discard = {
      new DiscardImpl(cause, hand)
    }

    override def newSetAsMonster(cause: Cause, monster: Monster): SetAsMonster = {
      new SetAsMonsterImpl(cause, monster)
    }

    override def newSpecialSummon(cause: Cause, controller: Player, monster: Monster, position: Position): SpecialSummonImpl = {
      SpecialSummonImpl(cause, controller, monster, position)
    }

    override def newNormalSummon(cause: Cause, monster: Monster): NormalSummon = {
      new NormalSummonImpl(cause, monster)
    }

    override def newFlipSummon(cause: Cause, monster: Monster)(implicit eventsModule: EventsModule): FlipSummon = {
      new FlipSummonImpl(cause, monster)
    }

    override def newDraw(cause: Cause, player: Player, howMany: Int): Draw = {
      new DrawImpl(cause, player, howMany)
    }

    override def newDiscardForHandSizeLimit()(implicit gameState: GameState): DiscardForHandSizeLimit = {
      new DiscardForHandSizeLimitImpl
    }

    override def newSwitchPosition(cause: Cause, monster: Monster)(implicit eventsModule: EventsModule): SwitchPosition = {
      new SwitchPositionImpl(cause, monster)
    }

    override def newTributeSummon(cause: Cause, monster: Monster): TributeSummon = {
      new TributeSummonImpl(cause, monster)
    }

    override def newTributeSet(cause: Cause, monster: Monster): TributeSet = {
      new TributeSetImpl(cause, monster)
    }

    override def newDrawForTurn(implicit gameState: GameState): DrawForTurn = new DrawForTurnImpl

    override def newDeclareDirectAttack(cause: Cause, monster: Monster) = DeclareDirectAttack(cause, monster)

    override def newDeclareAttackOnMonster(cause: Cause, attacker: Monster, target: Monster) = DeclareAttackOnMonster(cause, attacker, target)

    override def newChangeLifePoints(cause: Cause, lifePointsChange: Int, player: Player): ChangeLifePoints = ChangeLifePoints(cause, lifePointsChange, player)
  }
}
