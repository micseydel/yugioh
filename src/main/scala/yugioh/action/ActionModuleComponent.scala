package yugioh.action

import yugioh.action.monster.{FlipSummon, SpecialSummon, _}
import yugioh.card.Card.AnyCard
import yugioh.card.monster.{Monster, Position}
import yugioh.card.{SetAsSpellOrTrap, SetAsSpellOrTrapImpl, SpellOrTrap}
import yugioh.events.EventsModule
import yugioh.{GameState, Player}

trait ActionModule {
  def newSetAsSpellOrTrap(spellOrTrap: SpellOrTrap): SetAsSpellOrTrap

  def newNormalSummon(monster: Monster): NormalSummon

  def newSetAsMonster(monster: Monster): SetAsMonster

  def newTributeSummon(monster: Monster): TributeSummon

  def newTributeSet(monster: Monster): TributeSet

  def newSwitchPosition(monster: Monster)(implicit eventsModule: EventsModule): SwitchPosition

  def newFlipSummon(monster: Monster)(implicit eventsModule: EventsModule): FlipSummon

  def newDiscard(cause: Player, hand: Seq[AnyCard]): Discard

  def newDraw(player: Player, howMany: Int): Draw

  def newDrawForTurn(implicit gameState: GameState): DrawForTurn

  def newDestroy(player: Player, card: AnyCard): Destroy

  def newDestroy(player: Player, cards: Seq[AnyCard]): Destroy

  def newSpecialSummon(controller: Player, monster: Monster, position: Position): SpecialSummon

  def newDiscardForHandSizeLimit()(implicit gameState: GameState): DiscardForHandSizeLimit

  def newDeclareDirectAttack(monster: Monster): DeclareDirectAttack

  def newDeclareAttackOnMonster(attacker: Monster, target: Monster): DeclareAttackOnMonster

  def newChangeLifePoints(lifePointsChange: Int, player: Player): ChangeLifePoints
}

trait ActionModuleComponent {
  implicit def actionModule: ActionModule
}

trait DefaultActionModuleComponent extends ActionModuleComponent {
  def actionModule = new ActionModule {

    override def newSetAsSpellOrTrap(spellOrTrap: SpellOrTrap): SetAsSpellOrTrap = {
      new SetAsSpellOrTrapImpl(spellOrTrap)
    }

    override def newDestroy(player: Player, cards: Seq[AnyCard]): Destroy = {
      DestroyImpl(player, cards)
    }

    override def newDestroy(player: Player, card: AnyCard): Destroy = {
      newDestroy(player, Seq(card))
    }

    override def newDiscard(cause: Player, hand: Seq[AnyCard]): Discard = {
      new DiscardImpl(cause, hand)
    }

    override def newSetAsMonster(monster: Monster): SetAsMonster = {
      new SetAsMonsterImpl(monster)
    }

    override def newSpecialSummon(controller: Player, monster: Monster, position: Position) = {
      SpecialSummonImpl(controller, monster, position)
    }

    override def newNormalSummon(monster: Monster): NormalSummon = {
      new NormalSummonImpl(monster)
    }

    override def newFlipSummon(monster: Monster)(implicit eventsModule: EventsModule): FlipSummon = {
      new FlipSummonImpl(monster)
    }

    override def newDraw(player: Player, howMany: Int): Draw = {
      new DrawImpl(player, howMany)
    }

    override def newDiscardForHandSizeLimit()(implicit gameState: GameState): DiscardForHandSizeLimit = {
      new DiscardForHandSizeLimitImpl
    }

    override def newSwitchPosition(monster: Monster)(implicit eventsModule: EventsModule): SwitchPosition = {
      new SwitchPositionImpl(monster)
    }

    override def newTributeSummon(monster: Monster): TributeSummon = {
      new TributeSummonImpl(monster)
    }

    override def newTributeSet(monster: Monster): TributeSet = {
      new TributeSetImpl(monster)
    }

    override def newDrawForTurn(implicit gameState: GameState): DrawForTurn = new DrawForTurnImpl

    override def newDeclareDirectAttack(monster: Monster) = DeclareDirectAttack(monster)

    override def newDeclareAttackOnMonster(attacker: Monster, target: Monster) = DeclareAttackOnMonster(attacker, target)

    override def newChangeLifePoints(lifePointsChange: Int, player: Player): ChangeLifePoints = ChangeLifePoints(lifePointsChange, player)
  }
}
