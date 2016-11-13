package yugioh.card.library

import yugioh.Player
import yugioh.card.Card.AnyCard


/**
  * Basis for creating objects that allow easy instantiations.
  */
abstract class InstantiableCard[C <: AnyCard](implicit m: scala.reflect.Manifest[C]) {
  def apply(owner: Player): C = {
    m.runtimeClass.getConstructor(classOf[Player]).newInstance(owner).asInstanceOf[C]
  }
}
