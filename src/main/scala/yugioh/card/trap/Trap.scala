package yugioh.card.trap

import yugioh.card.{ContinuousSpellOrTrap, NormalSpellOrTrap, SpellOrTrap}

sealed trait Trap extends SpellOrTrap

trait NormalTrap extends Trap with NormalSpellOrTrap
trait ContinuousTrap extends Trap with ContinuousSpellOrTrap
trait CounterTrap extends Trap
