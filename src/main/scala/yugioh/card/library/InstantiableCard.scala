package yugioh.card.library

import yugioh.Player
import yugioh.card.Card


/**
  * Basis for creating objects that allow easy instantiations.
  */
abstract class InstantiableCard[C <: Card](implicit m: scala.reflect.Manifest[C]) {
  def apply(owner: Player): C = {
    m.runtimeClass.getConstructor(classOf[Player]).newInstance(owner).asInstanceOf[C]
  }
}
