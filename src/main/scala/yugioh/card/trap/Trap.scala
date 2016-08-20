package yugioh.card.trap

import yugioh.card.{NonContinuousSpellOrTrap, SpellOrTrap}

sealed trait Trap extends SpellOrTrap

trait NormalTrap extends Trap with NonContinuousSpellOrTrap
trait CounterTrap extends Trap with NonContinuousSpellOrTrap
trait ContinuousTrap extends Trap
