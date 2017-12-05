package yugioh.card

import yugioh.card.monster.Monster

// TODO LOW: How similar are http://yugioh.wikia.com/wiki/Pseudo-Trap_Monster ?

/**
  * A delegate for a card. Useful when a card wants to pretend to be a different type.
  *
  * For example, Artifact Moralltach.
  */
sealed trait Delegate

/**
  * This is a wrapper so that a Monster can be treated as a SpellOrTrap card.
  */
trait SpellOrTrapDelegate extends Delegate with SpellOrTrap

/**
  * This is essentially a wrapper that allows a SpellOrTrap to be treated as a Monster card.
  */
trait MonsterDelegate extends Delegate with Monster
