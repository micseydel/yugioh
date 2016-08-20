package yugioh.action

import yugioh.{GameState, Player}
import yugioh.action.monster.{FlipSummon, SpecialSummon, _}
import yugioh.card.{Card, SetAsSpellOrTrap, SetAsSpellOrTrapImpl, SpellOrTrap}
import yugioh.card.monster.{Monster, Position}
import yugioh.events.EventsModule

trait ActionModule {
  def newSetAsSpellOrTrap(spellOrTrap: SpellOrTrap): SetAsSpellOrTrap

  def newNormalSummon(monster: Monster): NormalSummon

  def newSetAsMonster(monster: Monster): SetAsMonster

  def newTributeSummon(monster: Monster): TributeSummon

  def newTributeSet(monster: Monster): TributeSet

  def newSwitchPosition(monster: Monster)(implicit eventsModule: EventsModule): SwitchPosition

  def newFlipSummon(monster: Monster)(implicit eventsModule: EventsModule): FlipSummon

  def newDiscard(cause: Player, hand: Seq[Card]): Discard

  def newDraw(player: Player, howMany: Int): Draw

  def newDrawForTurn(implicit gameState: GameState): DrawForTurn

  def newDestroy(player: Player, card: Card): Destroy

  def newDestroy(player: Player, cards: Seq[Card]): Destroy

  def newSpecialSummon(controller: Player, monster: Monster, position: Position): SpecialSummon

  def newDiscardForHandSizeLimit()(implicit gameState: GameState): DiscardForHandSizeLimit

  def newDeclareDirectAttack(monster: Monster): DeclareDirectAttack

  def newDeclareAttackOnMonster(monster: Monster): DeclareAttackOnMonster

  def newChangeLifePoints(lifePointsChange: Int, player: Player): ChangeLifePoints
}

trait ActionModuleComponent {
  def actionModule: ActionModule
}

trait DefaultActionModuleComponent extends ActionModuleComponent {
  def actionModule = new ActionModule {

    override def newSetAsSpellOrTrap(spellOrTrap: SpellOrTrap): SetAsSpellOrTrap = {
      new SetAsSpellOrTrapImpl(spellOrTrap)
    }

    override def newDestroy(player: Player, cards: Seq[Card]): Destroy = {
      DestroyImpl(player, cards)
    }

    override def newDestroy(player: Player, card: Card): Destroy = {
      newDestroy(player, Seq(card))
    }

    override def newDiscard(cause: Player, hand: Seq[Card]): Discard = {
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

    override def newDeclareDirectAttack(monster: Monster) = DeclareDirectAttackImpl(monster)

    override def newDeclareAttackOnMonster(monster: Monster) = DeclareAttackOnMonsterImpl(monster)

    override def newChangeLifePoints(lifePointsChange: Int, player: Player): ChangeLifePoints = ChangeLifePointsImpl(lifePointsChange, player)
  }
}
