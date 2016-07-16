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

  def newDiscard(cause: Player, hand: Seq[Card], existsInAChainAction: ExistsInAChainAction = null): Discard

  def newDraw(player: Player, howMany: Int, existsInAChainAction: ExistsInAChainAction = null): Draw

  def newDrawForTurn(implicit gameState: GameState): DrawForTurn

  /**
    * To provide a parent action, use the overridden version of this method that allows for cards plural.
    *
    * This is, unfortunately, a limitation of Scala:
-   * http://stackoverflow.com/questions/4652095/why-does-the-scala-compiler-disallow-overloaded-methods-with-default-arguments
    */
  def newDestroy(player: Player, card: Card): Destroy

  def newDestroy(player: Player, cards: Seq[Card], parent: Action = null): Destroy

  def newSpecialSummon(controller: Player, monster: Monster, position: Position, existsInAChainAction: ExistsInAChainAction): SpecialSummon

  def newDiscardForHandSizeLimit()(implicit gameState: GameState): DiscardForHandSizeLimit
}

trait ActionModuleComponent {
  def actionModule: ActionModule
}

trait DefaultActionModuleComponent extends ActionModuleComponent {
  def actionModule = new ActionModule {

    override def newSetAsSpellOrTrap(spellOrTrap: SpellOrTrap): SetAsSpellOrTrap = {
      new SetAsSpellOrTrapImpl(spellOrTrap)
    }

    override def newDestroy(player: Player, cards: Seq[Card], parent: Action = null): Destroy = {
      new DestroyImpl(player, cards, parent)
    }

    override def newDestroy(player: Player, card: Card): Destroy = {
      newDestroy(player, Seq(card))
    }

    override def newDiscard(cause: Player, hand: Seq[Card], existsInAChainAction: ExistsInAChainAction): Discard = {
      new DiscardImpl(cause, hand, existsInAChainAction)
    }

    override def newSetAsMonster(monster: Monster): SetAsMonster = {
      new SetAsMonsterImpl(monster)
    }

    override def newSpecialSummon(controller: Player, monster: Monster, position: Position, existsInAChainAction: ExistsInAChainAction) = {
      new SpecialSummonImpl(controller, monster, position, existsInAChainAction)
    }

    override def newNormalSummon(monster: Monster): NormalSummon = {
      new NormalSummonImpl(monster)
    }

    override def newFlipSummon(monster: Monster)(implicit eventsModule: EventsModule): FlipSummon = {
      new FlipSummonImpl(monster)
    }

    override def newDraw(player: Player, howMany: Int, existsInAChainAction: ExistsInAChainAction): Draw = {
      new DrawImpl(player, howMany, existsInAChainAction)
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
  }
}
